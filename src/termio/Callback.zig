//! Callback implements a termio backend that uses external callbacks for I/O.
//! This is useful for environments that don't have a pty, such as SSH clients
//! or web-based terminals where the I/O is handled externally.
const Callback = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;
const renderer = @import("../renderer.zig");
const terminal = @import("../terminal/main.zig");
const termio = @import("../termio.zig");

const log = std.log.scoped(.io_callback);

/// The callback function type for writing data.
/// This is called when the terminal wants to send data (e.g., keyboard input).
///
/// IMPORTANT: The userdata pointer must remain valid for the lifetime of the
/// surface. The caller is responsible for ensuring userdata outlives the
/// surface and is not freed while the callback may still be invoked.
pub const WriteFn = *const fn (
    userdata: ?*anyopaque,
    data: [*]const u8,
    len: usize,
) callconv(.c) void;

/// The current grid size for resize notifications.
grid_size: renderer.GridSize = .{ .columns = 80, .rows = 24 },
screen_size: renderer.ScreenSize = .{ .width = 800, .height = 600 },

/// Initialize the callback backend.
pub fn init(
    alloc: Allocator,
    cfg: Config,
) !Callback {
    _ = alloc;
    _ = cfg;
    return .{};
}

pub fn deinit(self: *Callback) void {
    _ = self;
}

/// Initialize terminal state as necessary for this backend.
pub fn initTerminal(self: *Callback, term: *terminal.Terminal) void {
    // Callback backend doesn't need to initialize terminal state
    // since it doesn't have a subprocess with a specific CWD or initial state.
    _ = self;
    _ = term;
}

pub fn threadEnter(
    self: *Callback,
    alloc: Allocator,
    io: *termio.Termio,
    td: *termio.Termio.ThreadData,
) !void {
    _ = self;
    _ = alloc;
    _ = io;

    // Initialize our thread data
    td.backend = .{ .callback = .{} };
}

pub fn threadExit(self: *Callback, td: *termio.Termio.ThreadData) void {
    _ = self;
    _ = td;
}

pub fn focusGained(
    self: *Callback,
    td: *termio.Termio.ThreadData,
    focused: bool,
) !void {
    _ = self;
    _ = td;
    _ = focused;
    // Focus changes don't need special handling for callback backend
}

pub fn resize(
    self: *Callback,
    grid_size: renderer.GridSize,
    screen_size: renderer.ScreenSize,
) !void {
    self.grid_size = grid_size;
    self.screen_size = screen_size;
}

/// Queue a write to the external handler via callback.
pub fn queueWrite(
    self: *Callback,
    alloc: Allocator,
    td: *termio.Termio.ThreadData,
    data: []const u8,
    linefeed: bool,
) !void {
    _ = self;
    _ = alloc;

    const cb = &td.backend.callback;

    // Get the write callback from thread data
    const write_fn = cb.write_fn orelse {
        log.warn("no write callback set, dropping {} bytes", .{data.len});
        return;
    };

    if (!linefeed) {
        // Fast path: no linefeed conversion needed
        write_fn(cb.userdata, data.ptr, data.len);
        return;
    }

    // Slow path: need to convert \r to \r\n by iterating and
    // making multiple callback invocations.
    var i: usize = 0;
    while (i < data.len) {
        // Find the next \r or end of data
        var j = i;
        while (j < data.len and data[j] != '\r') : (j += 1) {}

        // Write the chunk before \r
        if (j > i) {
            write_fn(cb.userdata, data.ptr + i, j - i);
        }

        // If we found a \r, write \r\n
        if (j < data.len and data[j] == '\r') {
            const crlf = "\r\n";
            write_fn(cb.userdata, crlf.ptr, crlf.len);
            j += 1;
        }

        i = j;
    }
}

/// Handle abnormal child exit - not applicable for callback backend
pub fn childExitedAbnormally(
    self: *Callback,
    gpa: Allocator,
    t: *terminal.Terminal,
    exit_code: u32,
    runtime_ms: u64,
) !void {
    _ = self;
    _ = gpa;
    _ = t;
    _ = exit_code;
    _ = runtime_ms;
    // Callback backend doesn't have a child process
}

/// Thread-local data for the callback implementation.
pub const ThreadData = struct {
    /// The write callback function.
    write_fn: ?WriteFn = null,

    /// Userdata passed to callbacks.
    userdata: ?*anyopaque = null,

    pub fn deinit(self: *ThreadData, alloc: Allocator) void {
        _ = self;
        _ = alloc;
    }
};

/// Configuration for the callback backend.
pub const Config = struct {
    /// Initial write callback (can be set later via setWriteCallback).
    write_fn: ?WriteFn = null,

    /// Userdata passed to callbacks.
    userdata: ?*anyopaque = null,
};
