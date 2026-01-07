//! A wrapper around a CALayer with a utility method
//! for settings its `contents` to an IOSurface.
const IOSurfaceLayer = @This();

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const objc = @import("objc");
const macos = @import("macos");

const IOSurface = macos.iosurface.IOSurface;

const log = std.log.scoped(.IOSurfaceLayer);

/// We subclass CALayer with a custom display handler, we only need
/// to make the subclass once, and then we can use it as a singleton.
var Subclass: ?objc.Class = null;

/// The underlying CALayer
layer: objc.Object,

pub fn init() !IOSurfaceLayer {
    // The layer returned by `[CALayer layer]` is autoreleased, which means
    // that at the end of the current autorelease pool it will be deallocated
    // if it isn't retained, so we retain it here manually an extra time.
    const layer = (try getSubclass()).msgSend(
        objc.Object,
        objc.sel("layer"),
        .{},
    ).retain();
    errdefer layer.release();

    // The layer gravity is set to top-left so that the contents aren't
    // stretched during resize operations before a new frame has been drawn.
    layer.setProperty("contentsGravity", macos.animation.kCAGravityTopLeft);
    layer.setProperty("opaque", true);

    layer.setInstanceVariable("display_cb", .{ .value = null });
    layer.setInstanceVariable("display_ctx", .{ .value = null });

    if (comptime builtin.os.tag == .ios) {
        log.info("IOSurfaceLayer init: using CAIOSurfaceLayer on iOS", .{});
    }

    return .{ .layer = layer };
}

pub fn release(self: *IOSurfaceLayer) void {
    self.layer.release();
}

/// Sets the layer's `contents` to the provided IOSurface.
///
/// Makes sure to do so on the main thread to avoid visual artifacts.
pub inline fn setSurface(self: *IOSurfaceLayer, surface: *IOSurface) !void {
    // We retain the surface to make sure it's not GC'd
    // before we can set it as the contents of the layer.
    //
    // We release in the callback after setting the contents.
    surface.retain();
    // NOTE: Since `self.layer` is passed as an `objc.c.id`, it's
    //       automatically retained when the block is copied, so we
    //       don't need to retain it ourselves like with the surface.

    var block = SetSurfaceBlock.init(.{
        .layer = self.layer.value,
        .surface = surface,
    }, &setSurfaceCallback);

    // We check if we're on the main thread and run the block directly if so.
    const NSThread = objc.getClass("NSThread").?;
    if (NSThread.msgSend(bool, "isMainThread", .{})) {
        setSurfaceCallback(&block);
    } else {
        // NOTE: The block will be copied when we pass it to dispatch_async,
        //       and then automatically be deallocated by the objc runtime
        //       once it's executed.

        macos.dispatch.dispatch_async(
            @ptrCast(macos.dispatch.queue.getMain()),
            @ptrCast(&block),
        );
    }
}

/// Sets the layer's `contents` to the provided IOSurface.
///
/// Does not ensure this happens on the main thread.
pub inline fn setSurfaceSync(self: *IOSurfaceLayer, surface: *IOSurface) void {
    self.layer.setProperty("contents", surface);
}

const SetSurfaceBlock = objc.Block(struct {
    layer: objc.c.id,
    surface: *IOSurface,
}, .{}, void);

fn setSurfaceCallback(
    block: *const SetSurfaceBlock.Context,
) callconv(.c) void {
    const layer = objc.Object.fromId(block.layer);
    const surface: *IOSurface = block.surface;

    // See explanation of why we retain and release in `setSurface`.
    defer surface.release();

    // We check to see if the surface is the appropriate size for
    // the layer, if it's not then we discard it. This is because
    // asynchronously drawn frames can sometimes finish just after
    // a synchronously drawn frame during a resize, and if we don't
    // discard the improperly sized surface it creates jank.
    const bounds = layer.getProperty(macos.graphics.Rect, "bounds");
    const scale = layer.getProperty(f64, "contentsScale");
    const width: usize = @intFromFloat(bounds.size.width * scale);
    const height: usize = @intFromFloat(bounds.size.height * scale);
    if (comptime builtin.os.tag == .ios) {
        log.info(
            "setSurfaceCallback: bounds={d}x{d} scale={d:.3} layer_px={d}x{d} surface={d}x{d} bpr={d} fmt={d}",
            .{
                bounds.size.width,
                bounds.size.height,
                scale,
                width,
                height,
                surface.getWidth(),
                surface.getHeight(),
                surface.getBytesPerRow(),
                @intFromEnum(surface.getPixelFormat()),
            },
        );
    }
    if (width != surface.getWidth() or height != surface.getHeight()) {
        log.debug(
            "setSurfaceCallback(): surface is wrong size for layer, surface = {d}x{d}, layer = {d}x{d}",
            .{ surface.getWidth(), surface.getHeight(), width, height },
        );

        // On iOS, try to correct the contentsScale instead of discarding.
        if (comptime builtin.os.tag == .ios) {
            const bw = bounds.size.width;
            const bh = bounds.size.height;
            if (bw > 0 and bh > 0) {
                const sx: f64 = @as(f64, @floatFromInt(surface.getWidth())) / bw;
                const sy: f64 = @as(f64, @floatFromInt(surface.getHeight())) / bh;
                const new_scale: f64 = if (sx > sy) sx else sy;
                layer.setProperty("contentsScale", new_scale);
            }
        } else {
            return;
        }
    }

    layer.setProperty("contents", surface);
    if (comptime builtin.os.tag == .ios) {
        log.info("setSurfaceCallback: contents set", .{});
    }
}

pub const DisplayCallback = ?*align(8) const fn (?*anyopaque) void;

pub fn setDisplayCallback(
    self: *IOSurfaceLayer,
    display_cb: DisplayCallback,
    display_ctx: ?*anyopaque,
) void {
    self.layer.setInstanceVariable(
        "display_cb",
        objc.Object.fromId(@constCast(display_cb)),
    );
    self.layer.setInstanceVariable(
        "display_ctx",
        objc.Object.fromId(display_ctx),
    );
}

fn getSubclass() error{ObjCFailed}!objc.Class {
    if (Subclass) |c| return c;

    // On iOS, use CAIOSurfaceLayer as base class for better IOSurface integration
    const base_layer = switch (comptime builtin.os.tag) {
        .ios => objc.getClass("CAIOSurfaceLayer") orelse
            objc.getClass("CALayer") orelse return error.ObjCFailed,
        else => objc.getClass("CALayer") orelse return error.ObjCFailed,
    };

    var subclass =
        objc.allocateClassPair(base_layer, "IOSurfaceLayer") orelse return error.ObjCFailed;
    errdefer objc.disposeClassPair(subclass);

    if (!subclass.addIvar("display_cb")) return error.ObjCFailed;
    if (!subclass.addIvar("display_ctx")) return error.ObjCFailed;

    subclass.replaceMethod("display", struct {
        fn display(target: objc.c.id, sel: objc.c.SEL) callconv(.c) void {
            _ = sel;
            const self = objc.Object.fromId(target);
            const display_cb: DisplayCallback = @ptrFromInt(@intFromPtr(
                self.getInstanceVariable("display_cb").value,
            ));
            if (display_cb) |cb| cb(
                @ptrCast(self.getInstanceVariable("display_ctx").value),
            );
        }
    }.display);

    // Disable all animations for this layer by returning null for all actions.
    subclass.replaceMethod("actionForKey:", struct {
        fn actionForKey(
            target: objc.c.id,
            sel: objc.c.SEL,
            key: objc.c.id,
        ) callconv(.c) objc.c.id {
            _ = target;
            _ = sel;
            _ = key;
            return objc.getClass("NSNull").?.msgSend(objc.c.id, "null", .{});
        }
    }.actionForKey);

    objc.registerClassPair(subclass);

    Subclass = subclass;

    return subclass;
}
