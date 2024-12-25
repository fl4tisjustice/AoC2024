const std = @import("std");
const re = @cImport(@cInclude("day-14/regez.h"));

fn Vector2D(comptime T: type) type {
    return struct {
        x: T,
        y: T,

        const Self = @This();

        fn add(self: *const Self, other: *const Self) Self {
            return Vector2D(T) { .x = self.x + other.x, .y = self.y + other.y };
        }

        fn sub(self: *const Self, other: *const Self) Self {
            return Vector2D(T) { .x = self.x - other.x, .y = self.y - other.y };
        }

    };
}

fn Robot(comptime T: type) type {
    return struct {
        position: Vector2D(T),
        velocity: Vector2D(T),

        const Self = @This();
        
        fn patrol(self: *Self, width: T, height: T) void {
            const next = Vector2D(T).add(&self.position, &self.velocity);
            self.position.x = @mod(next.x, width); self.position.y = @mod(next.y, height);
        }
    };
}

fn getInput(allocator: std.mem.Allocator) []const Robot(i64) {
    const file = std.fs.cwd().openFile("day-14/input.txt", .{ .mode = .read_only }) catch |err| {
        std.debug.print("Failed to open file: {s}\n", .{ @errorName(err) });
        std.process.exit(1);
    };

    defer file.close();

    const regex = re.regex_alloc();
    defer re.regex_free(regex);
    _ = re.regcomp(regex, "p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)", re.REG_EXTENDED);
    defer re.regfree(regex);

    var robots = std.ArrayList(Robot(i64)).init(allocator);
    defer robots.deinit();

    while (file.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.maxInt(usize)) catch unreachable) |line| {
        var matches: [5]re.regmatch_t = undefined;
        _ = re.regexec(regex, @ptrCast(line.ptr), matches.len, @ptrCast(&matches), 0);

        robots.append(Robot(i64) {
            .position = .{
                .x = std.fmt.parseInt(i64, line[@intCast(matches[1].rm_so)..@intCast(matches[1].rm_eo)], 10) catch unreachable,
                .y = std.fmt.parseInt(i64, line[@intCast(matches[2].rm_so)..@intCast(matches[2].rm_eo)], 10) catch unreachable
            },
            .velocity = .{
                .x = std.fmt.parseInt(i64, line[@intCast(matches[3].rm_so)..@intCast(matches[3].rm_eo)], 10) catch unreachable,
                .y = std.fmt.parseInt(i64, line[@intCast(matches[4].rm_so)..@intCast(matches[4].rm_eo)], 10) catch unreachable
            }
        }) catch unreachable;
    }

    return robots.toOwnedSlice() catch unreachable;
}

fn simulate(comptime T: type, robots: []Robot(T), width: T, height: T, seconds: u64) void{
    for (0..seconds) |_| {
        for (robots) |*robot| {
            robot.patrol(width, height);
        }
    }
}

fn safetyFactor(comptime T: type, robots: []const Robot(T), width: T, height: T) u64 {
    var quadrants =  [2][2]u64{ [_]u64{0, 0}, [_]u64{0, 0} };
    const exclude = Vector2D(T){ .x = @divFloor(width, 2), .y = @divFloor(height, 2) };

    for (robots) |robot| {
        if (robot.position.x == exclude.x or robot.position.y == exclude.y) continue;
        quadrants[@intFromBool(robot.position.x > exclude.x)][@intFromBool(robot.position.y > exclude.y)] += 1;
    }

    return quadrants[0][0] * quadrants[0][1] * quadrants[1][0] * quadrants[1][1];
}

fn partOne(comptime T: type, robots: []const Robot(T), allocator: std.mem.Allocator) u64 {
    const width: T = 101; const height: T = 103;

    var robots_copy = std.ArrayList(Robot(i64)).init(allocator);
    defer robots_copy.deinit();

    robots_copy.appendSlice(robots) catch unreachable;

    simulate(T, robots_copy.items, width, height, 100);
    return safetyFactor(T, robots_copy.items, width, height);
}

fn partTwo(comptime T: type, robots: []const Robot(T), allocator: std.mem.Allocator) u64 {
    const width: T = 101; const height: T = 103;

    var robots_copy = std.ArrayList(Robot(i64)).init(allocator);
    defer robots_copy.deinit();

    robots_copy.appendSlice(robots) catch unreachable;

    var occupied = std.AutoHashMap(T, u8).init(allocator);
    defer occupied.deinit();

    for (0..1_000_000) |iterations| {
        for (robots_copy.items) |robot| {
            const insert: u8 = occupied.get(robot.position.y * width + robot.position.x) orelse 0;
            occupied.put(robot.position.y * width + robot.position.x, insert + 1) catch unreachable;
        }

        var all_unique = true;
        var iter = occupied.valueIterator();
        
        while (iter.next()) |v| {
            if (v.* > 1) {
                all_unique = false;
                break;
            }
        }

        if (all_unique) return iterations;

        occupied.clearRetainingCapacity();
        simulate(i64, robots_copy.items, width, height, 1);
    }

    return 0;
}


pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    const robots = getInput(allocator);

    std.debug.print("Part One: {d}\nPart Two: {d}\n", .{
        partOne(i64, robots, allocator),
        partTwo(i64, robots, allocator)
    });
}