const GhosttyXCFramework = @This();

const std = @import("std");
const Config = @import("Config.zig");
const SharedDeps = @import("SharedDeps.zig");
const GhosttyLib = @import("GhosttyLib.zig");
const XCFrameworkStep = @import("XCFrameworkStep.zig");
const Target = @import("xcframework.zig").Target;

xcframework: *XCFrameworkStep,
target: Target,

pub fn init(
    b: *std.Build,
    deps: *const SharedDeps,
    target: Target,
) !GhosttyXCFramework {
    // Universal macOS build
    const macos_universal = try GhosttyLib.initMacOSUniversal(b, deps);

    // Native macOS build
    const macos_native = try GhosttyLib.initStatic(b, &try deps.retarget(
        b,
        Config.genericMacOSTarget(b, null),
    ));

    const ios = try GhosttyLib.initStatic(b, &try deps.retarget(
        b,
        b.resolveTargetQuery(.{
            .cpu_arch = .aarch64,
            .os_tag = .ios,
            .os_version_min = Config.osVersionMin(.ios),
        }),
    ));

    const ios_simulator = try GhosttyLib.initStatic(b, &try deps.retarget(
        b,
        b.resolveTargetQuery(.{
            .cpu_arch = .aarch64,
            .cpu_model = .{ .explicit = &std.Target.aarch64.cpu.apple_a17 },
            .os_tag = .ios,
            .os_version_min = Config.osVersionMin(.ios),
            .abi = .simulator,
        }),
    ));

    // Generate a headers directory with only ghostty.h and the module
    // map. We can't use include/ directly because it also contains the
    // libghostty-vt headers under include/ghostty/, which would trigger
    // "umbrella header does not include header" warnings from Clang's
    // module system.
    const wf = b.addWriteFiles();
    _ = wf.addCopyFile(b.path("include/ghostty.h"), "ghostty.h");
    _ = wf.addCopyFile(b.path("include/module.modulemap"), "module.modulemap");
    const headers = wf.getDirectory();

    // The xcframework wraps our ghostty library so that we can link
    // it to the final app built with Swift.
    const xcframework = XCFrameworkStep.create(b, .{
        .name = "GhosttyKit",
        .out_path = "macos/GhosttyKit.xcframework",
        .libraries = switch (target) {
            .universal => &.{
                .{
                    .library = macos_universal.output,
                    .headers = headers,
                    .dsym = macos_universal.dsym,
                },
                .{
                    .library = ios.output,
                    .headers = headers,
                    .dsym = ios.dsym,
                },
                .{
                    .library = ios_simulator.output,
                    .headers = headers,
                    .dsym = ios_simulator.dsym,
                },
            },

            .native => &.{.{
                .library = macos_native.output,
                .headers = headers,
                .dsym = macos_native.dsym,
            }},
        },
    });

    return .{
        .xcframework = xcframework,
        .target = target,
    };
}

pub fn install(self: *const GhosttyXCFramework) void {
    const b = self.xcframework.step.owner;
    self.addStepDependencies(b.getInstallStep());
}

pub fn addStepDependencies(
    self: *const GhosttyXCFramework,
    other_step: *std.Build.Step,
) void {
    other_step.dependOn(self.xcframework.step);
}
