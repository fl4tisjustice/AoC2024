const std = @import("std");
const String = []const u8;
const ArrayList = std.ArrayList;
const allocator = std.heap.page_allocator;

const Calibration = struct {
    value: u64,
    operands: []const u64,

    fn isValid(self: *const Calibration, allow_concat: bool) bool {
        if (self.operands.len == 1) return self.value == self.operands[0];
        const new_slice = self.operands[0..self.operands.len - 1];
        const operand = self.operands[self.operands.len - 1];
        var valid = false;
        
        if (@rem(self.value, operand) == 0) valid = valid or
            isValid(&.{ .value = @divExact(self.value, operand), .operands = new_slice }, allow_concat);

        if (self.value < operand) return valid;

        valid = valid or isValid(&.{ .value = self.value - operand, .operands = new_slice }, allow_concat);

        if (!allow_concat) return valid;

        const divisor = std.math.pow(u64, 10, std.math.log10_int(operand) + 1);
        if (@rem(self.value - operand, divisor) == 0) valid = valid or
            isValid(&.{ .value = @divExact(self.value - operand, divisor), .operands = new_slice }, allow_concat);
            
        return valid;
    }
};

fn getInput() ![]const Calibration {
    const file = try std.fs.cwd().openFile("day-7/input.txt", .{ .mode = .read_only });
    defer file.close();

    const input = try file.reader().readAllAlloc(allocator, (try file.stat()).size);
    defer allocator.free(input);

    var calibrations = ArrayList(Calibration).init(allocator);
    errdefer calibrations.deinit();

    var it = std.mem.splitScalar(u8, input, '\n');

    while (it.next()) |cal_str| {
        if (cal_str.len == 0) break;
        var cal_it = std.mem.splitSequence(u8, cal_str, ": ");

        const value = try std.fmt.parseInt(u64, cal_it.first(), 10);

        var operands = ArrayList(u64).init(allocator);
        errdefer operands.deinit();

        var op_it = std.mem.splitScalar(u8, cal_it.next().?, ' ');
        while (op_it.next()) |operand|
            try operands.append(try std.fmt.parseInt(u64, operand, 10));

        try calibrations.append(.{ .value = value, .operands = try operands.toOwnedSlice() });
    }
    return try calibrations.toOwnedSlice();
}

fn countValid(calibrations: []const Calibration, allow_concat: bool) u64 {
    var total: u64 = 0;
    for (calibrations) |calibration| {
        if (calibration.isValid(allow_concat)) {
            total += calibration.value;
        }
    }
    return total;
}


pub fn main() !void {
    const collections = try getInput();
    defer allocator.free(collections);
    defer for (collections) |collection| allocator.free(collection.operands);

    std.debug.print("Part One: {d}\nPart Two: {d}\n", .{ countValid(collections, false), countValid(collections, true) });
}
