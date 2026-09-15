//! Immutable host text-input projection. The screen owns the actual selection.
const Snapshot = @This();
const std = @import("std");
const Terminal = @import("Terminal.zig");
const ScreenSet = @import("ScreenSet.zig");
const Selection = @import("Selection.zig");
const Pin = @import("PageList.zig").Pin;
const Allocator = std.mem.Allocator;

pub const Anchor = struct {
    selection: Selection,
    screen_key: ScreenSet.Key,
    screen_id: usize,

    pub fn init(t: *Terminal) !?Anchor {
        const sel = t.screens.active.selection orelse return null;
        if (sel.start().garbage or sel.end().garbage) return null;
        return .{
            .selection = try (Selection.init(sel.start(), sel.end(), false)).track(t.screens.active),
            .screen_key = t.screens.active_key,
            .screen_id = t.screens.generation(t.screens.active_key),
        };
    }

    pub fn deinit(self: Anchor, t: *Terminal) void {
        if (t.screens.generation(self.screen_key) == self.screen_id) {
            if (t.screens.get(self.screen_key)) |screen| self.selection.deinit(screen);
        }
    }
};

pub const Cell = extern struct {
    offset: usize,
    length: usize,
    x: u32,
    y: u32,
    width: u32,
};

text: [:0]const u8,
cells: []const Cell,
cols: u32,
rows: u32,
utf16_len: usize,
has_selection: bool,
selection_start: usize,
selection_len: usize,
screen_key: ScreenSet.Key,
screen_id: usize,
top: *Pin,

pub fn init(a: Allocator, t: *Terminal) !Snapshot {
    const screen = t.screens.active;
    const top = try screen.pages.trackPin(screen.pages.getTopLeft(.viewport));
    errdefer screen.pages.untrackPin(top);
    var text: std.ArrayList(u8) = .empty;
    defer text.deinit(a);
    var cells: std.ArrayList(Cell) = .empty;
    errdefer cells.deinit(a);
    var utf16_len: usize = 0;
    var selection_start: ?usize = null;
    var selection_end: usize = 0;
    const sel = screen.selection;
    const tl = if (sel) |v| v.topLeft(screen) else null;
    const br = if (sel) |v| v.bottomRight(screen) else null;
    for (0..screen.pages.rows) |y| {
        const start = screen.pages.pin(.{ .viewport = .{ .x = 0, .y = @intCast(y) } }) orelse break;
        var end = start;
        end.x = screen.pages.cols - 1;
        const map = try screen.selectionStringMap(a, .{ .sel = .init(start, end, true), .trim = false });
        defer map.deinit(a);
        // Format each physical row separately; UIKit geometry uses grid rows.
        const line = std.mem.trimEnd(u8, map.string, "\r\n");
        var it = (try std.unicode.Utf8View.init(line)).iterator();
        var byte_offset: usize = 0;
        while (it.nextCodepoint()) |cp| {
            const byte_len = std.unicode.utf8CodepointSequenceLength(cp) catch unreachable;
            const pin = map.map.get(byte_offset) orelse return error.InvalidTextMap;
            const length: usize = if (cp > 0xFFFF) 2 else 1;
            const width: u32 = if (pin.rowAndCell().cell.wide == .wide) 2 else 1;
            if (cells.items.len > 0 and cells.items[cells.items.len - 1].x == pin.x and
                cells.items[cells.items.len - 1].y == y)
            {
                cells.items[cells.items.len - 1].length += length;
            } else {
                try cells.append(a, .{ .offset = utf16_len, .length = length, .x = pin.x, .y = @intCast(y), .width = width });
            }
            if (tl != null and !pin.before(tl.?) and !br.?.before(pin)) {
                if (selection_start == null) selection_start = utf16_len;
                selection_end = utf16_len + length;
            }
            utf16_len = try std.math.add(usize, utf16_len, length);
            byte_offset += byte_len;
        }
        try text.appendSlice(a, line);
        if (y + 1 < screen.pages.rows) {
            try text.append(a, '\n');
            utf16_len = try std.math.add(usize, utf16_len, 1);
        }
    }
    const owned_text = try text.toOwnedSliceSentinel(a, 0);
    errdefer a.free(owned_text);
    return .{
        .text = owned_text,
        .cells = try cells.toOwnedSlice(a),
        .cols = screen.pages.cols,
        .rows = screen.pages.rows,
        .utf16_len = utf16_len,
        .has_selection = sel != null,
        .selection_start = selection_start orelse 0,
        .selection_len = if (selection_start) |start| selection_end - start else 0,
        .screen_key = t.screens.active_key,
        .screen_id = t.screens.generation(t.screens.active_key),
        .top = top,
    };
}

pub fn deinit(self: Snapshot, a: Allocator, t: *Terminal) void {
    if (t.screens.generation(self.screen_key) == self.screen_id) {
        if (t.screens.get(self.screen_key)) |screen| screen.pages.untrackPin(self.top);
    }
    a.free(self.text);
    a.free(self.cells);
}

pub fn matches(self: Snapshot, other: Snapshot, t: *Terminal) bool {
    if (self.screen_key != other.screen_key or self.screen_id != other.screen_id or
        t.screens.generation(self.screen_key) != self.screen_id) return false;
    if (self.top.garbage or other.top.garbage or !self.top.eql(other.top.*)) return false;
    if (self.cols != other.cols or self.rows != other.rows or
        !std.mem.eql(u8, self.text, other.text) or self.cells.len != other.cells.len) return false;
    for (self.cells, other.cells) |old, new| {
        if (!std.meta.eql(old, new)) return false;
    }
    return true;
}

/// Reject stale UIKit ranges instead of mapping them to unrelated new text.
/// Call with the renderer lock held, including all snapshot lifetime operations.
pub fn select(self: Snapshot, a: Allocator, t: *Terminal, offset: usize, length: usize) !bool {
    return self.selectAnchored(a, t, offset, length, null);
}

pub fn selectAnchored(self: Snapshot, a: Allocator, t: *Terminal, offset: usize, length: usize, anchor: ?*const Anchor) !bool {
    if (length == 0 or offset >= self.utf16_len or length > self.utf16_len - offset) return false;
    if (t.screens.active_key != self.screen_key or t.screens.generation(self.screen_key) != self.screen_id) return false;
    const screen = t.screens.active;
    if (self.top.garbage or self.cols != screen.pages.cols or self.rows != screen.pages.rows or
        !self.top.eql(screen.pages.getTopLeft(.viewport))) return false;
    var current = try Snapshot.init(a, t);
    defer current.deinit(a, t);
    if (!self.matches(current, t)) return false;
    var start: ?Cell = null;
    var end: ?Cell = null;
    for (self.cells) |cell| {
        if (cell.offset + cell.length <= offset) continue;
        if (cell.offset >= offset + length) break;
        if (start == null) start = cell;
        end = cell;
    }
    const first = start orelse return false;
    const last = end orelse return false;
    var first_pin = screen.pages.pin(.{ .viewport = .{ .x = @intCast(first.x), .y = first.y } }) orelse return false;
    var last_pin = screen.pages.pin(.{ .viewport = .{ .x = @intCast(last.x), .y = last.y } }) orelse return false;
    if (anchor) |v| {
        if (v.screen_key != t.screens.active_key or v.screen_id != t.screens.generation(v.screen_key)) return false;
        if (v.selection.start().garbage or v.selection.end().garbage) return false;
        const anchor_start = v.selection.topLeft(screen);
        const anchor_end = v.selection.bottomRight(screen);
        if (anchor_start.before(first_pin)) first_pin = anchor_start;
        if (last_pin.before(anchor_end)) last_pin = anchor_end;
    }
    try screen.select(.init(first_pin, last_pin, false));
    if (screen.pages.viewport == .active) screen.pages.pinViewport();
    return true;
}

test "SelectionSnapshot tracks selection through output and reflow" {
    const a = std.testing.allocator;
    var t: Terminal = try .init(std.testing.io, a, .{ .cols = 20, .rows = 3 });
    defer t.deinit(a);
    try t.screens.active.testWriteString("alpha selected omega");
    var before = try Snapshot.init(a, &t);
    defer before.deinit(a, &t);
    try std.testing.expect(try before.select(a, &t, 6, 8));
    try t.screens.active.testWriteString("\nsecond\nthird\nfourth\nfifth");
    var after = try Snapshot.init(a, &t);
    defer after.deinit(a, &t);
    try std.testing.expectEqualStrings("selected", after.text[after.selection_start..][0..after.selection_len]);
    try t.resize(a, .{ .cols = 12, .rows = 3 });
    const selected = try t.screens.active.selectionString(a, .{ .sel = t.screens.active.selection.?, .trim = false });
    defer a.free(selected);
    try std.testing.expectEqualStrings("selected", selected);
}

test "SelectionSnapshot rejects stale text and invalid ranges" {
    const a = std.testing.allocator;
    var t: Terminal = try .init(std.testing.io, a, .{ .cols = 20, .rows = 3 });
    defer t.deinit(a);
    try t.screens.active.testWriteString("original");
    var snapshot = try Snapshot.init(a, &t);
    defer snapshot.deinit(a, &t);
    try std.testing.expect(!try snapshot.select(a, &t, std.math.maxInt(usize), 2));
    t.screens.active.cursorAbsolute(0, 0);
    try t.screens.active.testWriteString("replaced");
    try std.testing.expect(!try snapshot.select(a, &t, 0, 8));
    try std.testing.expect(t.screens.active.selection == null);
}

test "SelectionSnapshot maps UTF16 to terminal cells" {
    const a = std.testing.allocator;
    var t: Terminal = try .init(std.testing.io, a, .{ .cols = 20, .rows = 3 });
    defer t.deinit(a);
    var stream = t.vtStream();
    defer stream.deinit();
    stream.nextSlice("A😀界e\u{301}Z");
    var snapshot = try Snapshot.init(a, &t);
    defer snapshot.deinit(a, &t);
    try std.testing.expect(try snapshot.select(a, &t, 1, 3));
    const selected = try t.screens.active.selectionString(a, .{ .sel = t.screens.active.selection.?, .trim = false });
    defer a.free(selected);
    try std.testing.expectEqualStrings("😀界", selected);
}

test "SelectionSnapshot drag anchor follows reflow and shrinks" {
    const a = std.testing.allocator;
    var t: Terminal = try .init(std.testing.io, a, .{ .cols = 20, .rows = 4 });
    defer t.deinit(a);
    try t.screens.active.testWriteString("alpha selected omega");
    var snapshot = try Snapshot.init(a, &t);
    defer snapshot.deinit(a, &t);
    try std.testing.expect(try snapshot.select(a, &t, 6, 8));
    var anchor = (try Anchor.init(&t)).?;
    defer anchor.deinit(&t);
    try t.resize(a, .{ .cols = 12, .rows = 4 });
    var next = try Snapshot.init(a, &t);
    defer next.deinit(a, &t);
    try std.testing.expect(try next.selectAnchored(a, &t, 0, 1, &anchor));
    try std.testing.expect(try next.selectAnchored(a, &t, 6, 1, &anchor));
    const selected = try t.screens.active.selectionString(a, .{ .sel = t.screens.active.selection.?, .trim = false });
    defer a.free(selected);
    try std.testing.expectEqualStrings("selected", selected);
}

test "SelectionSnapshot preserves selection through selected text redraws" {
    const a = std.testing.allocator;
    var t: Terminal = try .init(std.testing.io, a, .{ .cols = 20, .rows = 3 });
    defer t.deinit(a);
    t.screens.active.cursorAbsolute(0, 1);
    try t.screens.active.testWriteString("Footer animation 0");
    var before = try Snapshot.init(a, &t);
    defer before.deinit(a, &t);
    try std.testing.expect(try before.select(a, &t, 1, 18));
    for (0..10) |frame| {
        t.screens.active.cursorAbsolute(17, 1);
        const digit = [_]u8{'0' + @as(u8, @intCast(frame))};
        try t.screens.active.testWriteString(&digit);
        var after = try Snapshot.init(a, &t);
        defer after.deinit(a, &t);
        try std.testing.expect(after.has_selection);
        try std.testing.expectEqual(@as(usize, 1), after.selection_start);
        try std.testing.expectEqual(@as(usize, 18), after.selection_len);
        const text = try t.screens.active.selectionString(a, .{ .sel = t.screens.active.selection.?, .trim = false });
        defer a.free(text);
        try std.testing.expectEqual(digit[0], text[text.len - 1]);
    }
}
