const std = @import("std");
const builtin = @import("builtin");

pub const bytes = if (builtin.os.tag == .visionos)
    4096
else
    std.fs.max_path_bytes;
