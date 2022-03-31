const std = @import("std");
const debug = std.debug;
const fmt = std.fmt;
const out = @import("out.zig");
const heap = std.heap;
const mem = std.mem;
const native_endian = @import("builtin").target.cpu.arch.endian();

fn hexFormat(comptime width: comptime_int) []const u8 {
    return fmt.comptimePrint("${{X:0>{}}}", .{width});
}

const Byte = union(enum) {
    const Self = @This();

    defined: u8,
    @"undefined",

    const Source = union(enum) {
        // const Self = @This();

        register: union(enum) {
            a,
            x,
            y,
        },
        memory: u16,

        fn print(self: Source, allocator: mem.Allocator) ![]const u8 {
            switch (self) {
                .register => {
                    const register = switch (self.register) {
                        .a => "A",
                        .x => "X",
                        .y => "Y",
                    };
                    return fmt.allocPrint(allocator, "register {s}", .{register});
                },
                .memory => |address| return fmt.allocPrint(allocator, "memory address " ++ hexFormat(4), .{address}),
            }
        }
    };

    fn get(self: *Self, allocator: mem.Allocator, source: Source) !u8 {
        switch (self.*) {
            .defined => |value| return value,
            .@"undefined" => return {
                try out.warn("attempting to read undefined value at {s}. initializing to 0", .{try source.print(allocator)});
                self.* = .{ .defined = 0 };
                return 0;
            },
        }
    }

    const u8_max_len = "255".len;

    fn printExtended(self: Self, allocator: mem.Allocator) ![]u8 {
        switch (self) {
            .defined => |value| {
                return fmt.allocPrint(allocator, hexFormat(2) ++ " (#{d})", .{ value, value });
            },
            .@"undefined" => {
                return fmt.allocPrint(allocator, "$?? (undefined)", .{});
            },
        }
    }

    const minimal_print_max_len = "$FF".len;
    fn printMinimal(self: Self, allocator: mem.Allocator) ![]u8 {
        switch (self) {
            .defined => |value| {
                return fmt.allocPrint(allocator, comptime hexFormat(2), .{value});
            },
            .@"undefined" => {
                return fmt.allocPrint(allocator, "$??", .{});
            },
        }
    }
};

var memory = [_]Byte{.@"undefined"} ** 0xfffff;
var stack: *Byte = memory[0x0100..0x01ff];
const registers = struct {
    var a: Byte = .@"undefined";
    var x: Byte = .@"undefined";
    var y: Byte = .@"undefined";
};
const Flags = packed struct {
    const Self = @This();

    negative: bool = false,
    overflow: bool = false,
    unused: bool = true, // This is hardwired to 1
    @"break": bool = true, // https://www.pagetable.com/?p=410
    bcd: bool = false,
    interrupt: bool = false,
    zero: bool = false,
    carry: bool = false,

    fn toByte(self: Self) u8 {
        return @bitReverse(u8, @bitCast(u8, self));
    }

    const Sign = enum { negative, positive };

    fn getSign(value: u8) Sign {
        if (value & 0b1000_0000 == 0b1000_0000) {
            return .negative;
        } else {
            return .positive;
        }
    }

    fn updateNZ(self: *Self, result: u8) u8 {
        self.zero = result == 0;

        // The first bit is the sign of an integer.
        // If it is set, the integer is negative.
        self.negative = getSign(result) == .negative;

        return result; // This is only for convenience
    }

    fn updateNVZ(self: *Self, a: u8, b: u8, result: u8) void {
        _ = self.updateNZ(result);

        // The overflow flag is set if the two inputs have the same sign and the output has a different sign.
        self.overflow = getSign(a) == getSign(b) and getSign(a) != getSign(result);
    }
};
var flags = Flags{};

/// Resets the CPU to the initial state.
fn reset() void {
    mem.set(Byte, &memory, .@"undefined");
    registers.a = .@"undefined";
    registers.x = .@"undefined";
    registers.y = .@"undefined";
    flags = Flags{};
}

// TODO: eventually move this into `printMemory` once possible
const row_len = 0xf;
fn printRow(allocator: mem.Allocator, index: usize, string: *std.ArrayList(u8)) !void {
    try string.appendSlice(out.intensity.dim);
    try string.appendSlice(try fmt.allocPrint(allocator, hexFormat(4) ++ ": ", .{index}));
    try string.appendSlice(out.intensity.none);
    var row_index: usize = 0;
    while (row_index < row_len) : (row_index += 1) {
        const row_byte = memory[index + row_index];
        try string.appendSlice(try row_byte.printMinimal(allocator));

        // Add a separator unless this is the last iteration
        if (row_index != row_len - 1) {
            try string.append(' ');
        }
    }
}

fn printMemory(allocator: mem.Allocator) ![]const u8 {
    var string = std.ArrayList(u8).init(allocator);
    var index: usize = 0;
    try string.appendSlice(" " ** "$FFFF: ".len);
    try string.appendSlice(out.intensity.dim);
    while (index < row_len) : (index += 1) {
        try string.appendSlice(try fmt.allocPrint(allocator, hexFormat(2) ++ " ", .{index}));
    }
    try string.appendSlice(out.intensity.none);
    try string.append('\n');

    index = 0;
    var memory_written = false;
    while (index < memory.len) : (index += 1) {
        const byte = memory[index];

        if (byte == .defined) {
            // Print the value along with some following bytes
            try printRow(allocator, index, &string);
            index += row_len;
            try string.append('\n');

            memory_written = true;
        }
    }

    if (memory_written) {
        return string.items;
    } else {
        // If the memory was never written to, there is no need to show any information about it.
        return "not accessed\n"[0..];
    }
}

const Operand = union(enum) { address: u16, value: u8 };

const OperandError = error{};

fn fetchIndirectZeroPageX(allocator: mem.Allocator, program: []const u8, pc: *u16) !Operand {
    pc.* += 1;
    return Operand{ .address = program[pc.*] + (try registers.x.get(allocator, .{ .register = .x })) };
}
fn fetchZeroPage(program: []const u8, pc: *u16) Operand {
    pc.* += 1;
    return .{ .address = program[pc.*] };
}
fn fetchImmediate(program: []const u8, pc: *u16) Operand {
    pc.* += 1;
    return .{ .value = program[pc.*] };
}
fn fetchAbsolute(program: []const u8, pc: *u16) Operand {
    // This is stored in little-endian
    pc.* += 1;
    const byte1 = program[pc.*];
    pc.* += 1;
    const byte2 = program[pc.*];
    return .{ .address = (@as(u16, byte2) << 8) | byte1 };
}
fn fetchAccumulator(allocator: mem.Allocator) !Operand {
    return Operand{ .value = (try registers.a.get(allocator, .{ .register = .a })) };
}

fn getOperand(allocator: mem.Allocator, program: []const u8, pc: *u16, addressing_mode: u3, kind: u2) !?Operand {
    switch (kind) {
        0b00 => return null, // No operand
        0b01 => {
            switch (addressing_mode) {
                0b000 => return try fetchIndirectZeroPageX(allocator, program, pc),
                0b001 => return fetchZeroPage(program, pc),
                0b010 => return fetchImmediate(program, pc),
                0b011 => return fetchAbsolute(program, pc),
                0b100, 0b101, 0b110, 0b111 => {
                    const addressing_mode_3bits = addressing_mode >> 2;
                    try out.err("unimplemented addressing mode %{b:0>3} (#{d})", .{ addressing_mode_3bits, addressing_mode_3bits });
                    return error.unknownAddressingMode;
                },
            }
        },
        0b10 => {
            switch (addressing_mode) {
                0b000 => return fetchImmediate(program, pc),
                0b001 => return fetchZeroPage(program, pc),
                0b010 => return try fetchAccumulator(allocator),
                0b011 => return fetchAbsolute(program, pc),
                0b101, 0b111 => {
                    const addressing_mode_3bits = addressing_mode >> 2;
                    try out.err("unimplemented addressing mode %{b:0>3} (#{d})", .{ addressing_mode_3bits, addressing_mode_3bits });
                    return error.unknownAddressingMode;
                },
                else => {
                    const addressing_mode_3bits = addressing_mode >> 2;
                    try out.err("unknown addressing mode %{b:0>3} (#{d})", .{ addressing_mode_3bits, addressing_mode_3bits });
                    return error.unknownAddressingMode;
                }
            }
        },
        else => {
            try out.err("unknown opcode kind %{b:0>2} (#{d})", .{ kind, kind });
            return error.unknownOpcodeKind;
        },
    }
}

fn executeInstruction(allocator: mem.Allocator, instruction: u8, operand: Operand) !void {
    switch (instruction) {
        0b000_000_01 => { // ORA
            registers.a = .{ .defined = flags.updateNZ((try registers.a.get(allocator, .{ .register = .a })) | operand.value) };
        },
        0b001_000_01 => { // AND
            registers.a = .{ .defined = flags.updateNZ((try registers.a.get(allocator, .{ .register = .a })) & operand.value) };
        },
        0b010_000_01 => { // EOR
            registers.a = .{ .defined = flags.updateNZ((try registers.a.get(allocator, .{ .register = .a })) ^ operand.value) };
        },
        0b011_000_01 => { // ADC
            const register_a = try registers.a.get(allocator, .{ .register = .a });

            var set_carry = true;

            const a = register_a;
            const b = std.math.add(u8, operand.value, @boolToInt(flags.carry)) catch result: {
                flags.carry = true;
                set_carry = false;
                break :result 0;
            };
            var result: u8 = undefined;
            const overflow = @addWithOverflow(u8, a, b, &result);

            if (set_carry) {
                flags.carry = overflow;
            }

            flags.updateNVZ(a, b, result);

            registers.a = .{ .defined = result };
        },
        0b100_000_01 => { // STA
            memory[operand.address] = registers.a;
        },
        0b101_000_01 => { // LDA
            const value = switch (operand) {
                .value => |value| value,
                .address => |address| try memory[address].get(allocator, .{ .memory = address }),
            };
            registers.a = .{ .defined = flags.updateNZ(value) };
        },
        0b110_000_01 => { // CMP
            const register_a = try registers.a.get(allocator, .{ .register = .a });
            flags.negative = register_a >= 0x80;
            flags.zero = register_a == operand.value;
            flags.carry = register_a >= operand.value;
        },
        0b111_000_01 => { // SBC
            const register_a = try registers.a.get(allocator, .{ .register = .a });

            var set_carry = true;

            const a = register_a;
            const b = std.math.add(u8, operand.value, @boolToInt(!flags.carry)) catch result: {
                flags.carry = false;
                set_carry = false;
                break :result 0;
            };

            flags.zero = a == b;
            if (set_carry)
                flags.carry = a >= b;

            var result: u8 = undefined;
            _ = @subWithOverflow(u8, a, b, &result);

            flags.negative = result >= 0x80;

            registers.a = .{ .defined = result };
        },
        0b101_000_10 => { // LDX
            registers.x = .{ .defined = flags.updateNZ(operand.value) };
        },
        else => {
            try out.err("unknown instruction %{b:0>8} (#{d})", .{ instruction, instruction });
            return error.unknownInstruction;
        },
    }
}

pub fn main() anyerror!u8 {
    var arena = heap.ArenaAllocator.init(heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const program = @embedFile("../world");
    executeProgram(allocator, program) catch {
        return 1;
    };

    try status(allocator);

    return 0;
}

fn executeSingleByteInstruction(instruction: u8) bool {
    switch (instruction) {
        0x18 => { // CLC
            flags.carry = false;
        },
        0x38 => { // SEC
            flags.carry = true;
        },
        else => return false,
    }
    return true;
}

fn executeProgram(allocator: mem.Allocator, program: []const u8) !void {
    var pc: u16 = 0;
    while (pc < program.len) : (pc += 1) {
        const byte = program[pc];
        const instruction: u8 = byte & 0b11100011;
        const addressing_mode: u3 = @intCast(u3, (byte & 0b00011100) >> 2);
        const kind: u2 = @intCast(u2, byte & 0b00000011);
        // debug.print("\n{b} = opcode {b} + addressing mode {b}\n", .{ byte, instruction, addressing_mode });

        // There does not seem to be a clear pattern for single-byte instructions
        // so go through these first.
        const instructionFound = executeSingleByteInstruction(byte);
        if (instructionFound) continue;

        const operand = (try getOperand(allocator, program, &pc, addressing_mode, kind)) orelse undefined;
        try executeInstruction(allocator, instruction, operand);
    }
}

fn status(allocator: mem.Allocator) !void {
    // try out.warn("undefined value read", .{});
    try out.info(allocator,
        \\Registers:
        \\A: {s}
        \\X: {s}
        \\Y: {s}
        \\
        \\Flags:
        \\ NV-BDIZC
        \\%{b:0>8}
        \\
        \\Memory:
        \\{s}
    , .{
        registers.a.printExtended(allocator),
        registers.x.printExtended(allocator),
        registers.y.printExtended(allocator),
        flags.toByte(),
        printMemory(allocator),
    });
}

fn assemble(allocator: mem.Allocator, source: []const u8, program_buffer: []u8) ![]u8 {
    const cwd = std.fs.cwd();

    const test_file_name = "650Z-test";
    const test_file = try cwd.createFile(test_file_name ++ ".s", .{});
    defer cwd.deleteFile(test_file_name ++ ".s") catch {
        // At least close it
        test_file.close();
    };

    try test_file.writeAll(source);

    const process = try std.ChildProcess.init(&[_][]const u8{ "cl65", "-t", "none", test_file_name ++ ".s" }, allocator);
    defer cwd.deleteFile(test_file_name ++ ".o") catch {};
    defer cwd.deleteFile(test_file_name) catch {};
    defer process.deinit();

    const termination = try process.spawnAndWait();
    try std.testing.expect(termination.Exited == 0);

    return cwd.readFile(test_file_name, program_buffer);
}

fn run(source: []const u8) !void {
    reset();

    const allocator = std.testing.allocator_instance.allocator();
    var program_buffer = [1]u8{0} ** 512;
    const program = try assemble(allocator, source, &program_buffer);
    try executeProgram(allocator, program);
}

const expectEqual = std.testing.expectEqual;

test "ora" {
    try run(
        \\lda #%11000011
        \\ora #%11001100
    );
    try expectEqual(@as(u8, 0b11001111), registers.a.defined);
    try expectEqual(@as(u8, 0b10110000), flags.toByte());
}

test "and" {
    try run(
        \\lda #%11011011
        \\and #%10111101
    );
    try expectEqual(@as(u8, 0b10011001), registers.a.defined);
    try expectEqual(@as(u8, 0b10110000), flags.toByte());

    try run(
        \\lda #%11011011
        \\and #%00000000
    );
    try expectEqual(@as(u8, 0b00000000), registers.a.defined);
    try expectEqual(@as(u8, 0b00110010), flags.toByte());
}

test "eor" {
    try run(
        \\lda #%11010111
        \\eor #%01100110
    );
    try expectEqual(@as(u8, 0b10110001), registers.a.defined);
    try expectEqual(@as(u8, 0b10110000), flags.toByte());
}

test "adc" {
    try run(
        \\lda #$ff
        \\adc #150
    );
    try expectEqual(@as(u8, 149), registers.a.defined);
    try expectEqual(@as(u8, 0b10110001), flags.toByte());

    try run(
        \\lda #100
        \\sec
        \\adc #50
    );
    try expectEqual(@as(u8, 151), registers.a.defined);
    try expectEqual(@as(u8, 0b11110000), flags.toByte());

    try run(
        \\lda #127
        \\adc #$ff
    );
    try expectEqual(@as(u8, 126), registers.a.defined);
    try expectEqual(@as(u8, 0b00110001), flags.toByte());

    try run(
        \\lda #5
        \\adc #5
    );
    try expectEqual(@as(u8, 10), registers.a.defined);
    try expectEqual(@as(u8, 0b00110000), flags.toByte());

    try run(
        \\lda #$80
        \\sec
        \\adc #$80
    );
    try expectEqual(@as(u8, 1), registers.a.defined);
    try expectEqual(@as(u8, 0b01110001), flags.toByte());

    try run(
        \\lda #$ff
        \\adc #150
    );
    try expectEqual(@as(u8, 149), registers.a.defined);
    try expectEqual(@as(u8, 0b10110001), flags.toByte());

    try run(
        \\lda #100
        \\sec
        \\adc #50
    );
    try expectEqual(@as(u8, 151), registers.a.defined);
    try expectEqual(@as(u8, 0b11110000), flags.toByte());

    try run(
        \\lda #127
        \\adc #$ff
    );
    try expectEqual(@as(u8, 126), registers.a.defined);
    try expectEqual(@as(u8, 0b00110001), flags.toByte());

    try run(
        \\lda #0
        \\sec
        \\adc #$ff
    );
    try expectEqual(@as(u8, 0), registers.a.defined);
    try expectEqual(@as(u8, 0b00110011), flags.toByte());
}

test "sta" {
    try run(
        \\lda #5
        \\sta $44
    );
    try expectEqual(@as(u8, 5), memory[0x0044].defined);
    try expectEqual(@as(u8, 0b00110000), flags.toByte());

    try run(
        \\lda #$ff
        \\sta $44ff
    );
    try expectEqual(@as(u8, 0xff), memory[0x44ff].defined);
    try expectEqual(@as(u8, 0b10110000), flags.toByte());

    try run(
        \\lda #$a
        \\ldx #5
        \\sta $4400, x
        \\lda $4400, x
    );
    try expectEqual(@as(u8, 0xa), registers.a.defined);
    try expectEqual(@as(u8, 0b00110000), flags.toByte());
}

test "lda" {
    try run(
        \\lda #0
    );
    try expectEqual(@as(u8, 0), registers.a.defined);
    try expectEqual(@as(u8, 0b00110010), flags.toByte());

    try run(
        \\lda #$80
    );
    try expectEqual(@as(u8, 0x80), registers.a.defined);
    try expectEqual(@as(u8, 0b10110000), flags.toByte());
}

test "cmp" {
    try run(
        \\lda #5
        \\cmp #5
    );
    try expectEqual(@as(u8, 0b00110011), flags.toByte());

    try run(
        \\lda #10
        \\cmp #5
    );
    try expectEqual(@as(u8, 0b00110001), flags.toByte());

    try run(
        \\lda #$ff
        \\cmp #$aa
    );
    try expectEqual(@as(u8, 0b10110001), flags.toByte());

    try run(
        \\lda #0
        \\cmp #$ff
    );
    try expectEqual(@as(u8, 0b00110000), flags.toByte());

    try run(
        \\lda #$80
        \\cmp #$80
    );
    try expectEqual(@as(u8, 0b10110011), flags.toByte());
}

test "sbc" {
    try run(
        \\lda #5
        \\sbc #5
    );
    try expectEqual(@as(u8, 0xff), registers.a.defined);
    try expectEqual(@as(u8, 0b10110000), flags.toByte());

    try run(
        \\lda #10
        \\sec
        \\sbc #5
    );
    try expectEqual(@as(u8, 5), registers.a.defined);
    try expectEqual(@as(u8, 0b00110001), flags.toByte());

    try run(
        \\lda #10
        \\sbc #5
    );
    try expectEqual(@as(u8, 4), registers.a.defined);
    try expectEqual(@as(u8, 0b00110001), flags.toByte());

    try run(
        \\lda #$ff
        \\sbc #$aa
    );
    try expectEqual(@as(u8, 0x54), registers.a.defined);
    try expectEqual(@as(u8, 0b00110001), flags.toByte());

    try run(
        \\lda #0
        \\sbc #$ff
    );
    try expectEqual(@as(u8, 0), registers.a.defined);
    try expectEqual(@as(u8, 0b00110010), flags.toByte());

    try run(
        \\lda #$80
        \\sbc #$80
    );
    try expectEqual(@as(u8, 0xff), registers.a.defined);
    try expectEqual(@as(u8, 0b10110000), flags.toByte());

    try run(
        \\lda #$80
        \\sec
        \\sbc #$80
    );
    try expectEqual(@as(u8, 0), registers.a.defined);
    try expectEqual(@as(u8, 0b00110011), flags.toByte());
}

test "asl" {

}

// https://llx.com/Neil/a2/opcodes.html
// https://skilldrick.github.io/easy6502/
// http://6502.org/tutorials/6502opcodes.html
// https://ziglang.org/documentation/0.9.1
