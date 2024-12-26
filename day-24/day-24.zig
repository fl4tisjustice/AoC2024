const std = @import("std");
const re = @cImport(@cInclude("/home/fl4t/Documents/AoC2024/day-14/regez.h"));

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const Tuple = std.meta.Tuple;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

const Operator = enum { AND, OR, XOR };

const Wire = union(enum) {
    literal: u1,
    eval: Tuple(&[_]type{ Operator, []const u8, []const u8 })
};

fn resolveWire(toEval: *Wire, wires: *StringHashMap(Wire)) u1 {
    const operator = toEval.*.eval[0];
    var op1: u1 = 0;
    var op2: u1 = 0;

    for ([_]*u1{ &op1, &op2 }, [_][]const u8{ toEval.*.eval[1], toEval.*.eval[2] }) |op, key| {
        const wire = wires.getPtr(key).?;
        switch (wire.*) {
            .literal => op.* = wire.*.literal,
            .eval => op.* = resolveWire(wire, wires)
        }
    }

    const result: u1 = switch (operator) {
        Operator.AND => op1 & op2,
        Operator.OR  => op1 | op2,
        Operator.XOR => op1 ^ op2 
    };
    
    toEval.* = Wire{ .literal = result };
    return result;
}

fn completeCircuit(wires: *StringHashMap(Wire), allocator: Allocator) u64 {
    var bits: u8 = 0;
    var keyIt = wires.keyIterator();
    while (keyIt.next()) |key| { if (std.mem.startsWith(u8, key.*, "z")) bits += 1; }

    var output: u64 = 0;

    for (0..bits) |bit| { 
        const key = std.fmt.allocPrint(allocator, "z{d:0>2}", .{ bits - bit - 1 }) catch unreachable;
        output = (output << 1) | resolveWire(wires.getPtr(key).?, wires);
    }

    return output;
}

fn getInput(allocator: Allocator) StringHashMap(Wire) {
    const file = std.fs.cwd().openFile("day-24/input.txt", .{ .mode = .read_only }) catch |err| {
        std.debug.print("Failed to open file: {s}", .{ @errorName(err) });
        std.process.exit(1);
    };
    defer file.close();

    const input = file.readToEndAlloc(allocator, std.math.maxInt(usize)) catch unreachable;
    defer allocator.free(input);
    var wires = StringHashMap(Wire).init(allocator);

    var inputIt = std.mem.splitSequence(u8, input, "\n\n");

    const liveWireRegex = re.regex_alloc();
    defer re.regex_free(liveWireRegex);
    _ = re.regcomp(liveWireRegex, "([xy][0-9]{2}): ([01])", re.REG_EXTENDED);
    defer re.regfree(liveWireRegex);

    var liveWire = std.mem.splitScalar(u8, inputIt.first(), '\n');
    while (liveWire.next()) |line| {
        var matches: [3]re.regmatch_t = undefined;
        _ = re.regexec(liveWireRegex, @ptrCast(line.ptr), matches.len, @ptrCast(&matches), 0);

        const key = allocator.dupe(u8, line[@intCast(matches[1].rm_so)..@intCast(matches[1].rm_eo)]) catch unreachable;
        const value = Wire { .literal = std.fmt.parseInt(u1, line[@intCast(matches[2].rm_so)..@intCast(matches[2].rm_eo)], 10) catch unreachable };

        wires.put(key, value) catch unreachable;
    }  

    const evalWireRegex = re.regex_alloc();
    defer re.regex_free(evalWireRegex);
    _ = re.regcomp(evalWireRegex, "([a-z0-9]{3}) (AND|OR|XOR) ([a-z0-9]{3}) -> ([a-z0-9]{3})", re.REG_EXTENDED);
    defer re.regfree(evalWireRegex);

    var evalWire = std.mem.splitScalar(u8, inputIt.next().?, '\n');
    while (evalWire.next()) |line| {
        var matches: [5]re.regmatch_t = undefined;
        _ = re.regexec(evalWireRegex, @ptrCast(line.ptr), matches.len, @ptrCast(&matches), 0);

        const key = allocator.dupe(u8, line[@intCast(matches[4].rm_so)..@intCast(matches[4].rm_eo)]) catch unreachable;
        
        const operator = std.meta.stringToEnum(Operator, line[@intCast(matches[2].rm_so)..@intCast(matches[2].rm_eo)]).?;
        const op1 = allocator.dupe(u8, line[@intCast(matches[1].rm_so)..@intCast(matches[1].rm_eo)]) catch unreachable;
        const op2 = allocator.dupe(u8, line[@intCast(matches[3].rm_so)..@intCast(matches[3].rm_eo)]) catch unreachable;

        wires.put(key, Wire { .eval = .{ operator, op1, op2 } }) catch unreachable;
    }

    return wires;
}

pub fn main() !void {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var wires = getInput(allocator);
    defer wires.deinit();
    std.debug.print("Part One: {d}\n", .{ completeCircuit(&wires, allocator) });
}