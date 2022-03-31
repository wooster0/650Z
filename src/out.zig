const std = @import("std");
const io = std.io;
const fmt = std.fmt;
const mem = std.mem;

const stderr = io.getStdErr().writer();

/// Terminal foreground color escape sequences.
pub const colors = struct {
    pub const yellow = "\u{1b}[93m";
    pub const red = "\u{1b}[91m";
    pub const none = "\u{1b}[39m";
};

/// Terminal intensity escape sequences.
pub const intensity = struct {
    pub const dim = "\u{1b}[2m";
    pub const none = "\u{1b}[22m";
};

pub fn info(allocator: mem.Allocator, comptime format: []const u8, args: anytype) !void {
    try stderr.writeAll(try fmt.allocPrint(allocator, format, args));
}

pub fn warn(comptime format: []const u8, args: anytype) !void {
    try stderr.print(colors.yellow ++ format ++ colors.none ++ "\n", args);
}

pub fn err(comptime format: []const u8, args: anytype) !void {
    try stderr.print(colors.red ++ format ++ colors.none ++ "\n", args);
}
