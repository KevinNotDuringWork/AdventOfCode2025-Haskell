// following @sphaerophoria stream.

const std = @import("std");

pub fn main() !void {
    const f = try std.fs.cwd().openFile("./example.txt", .{ .mode = .read_only });
    defer f.close();

    var buf: [4096]u8 = undefined;
    var reader = f.reader(&buf);
    const in = &reader.interface;

    var count: i16 = 50;
    var pass: i16 = 0;

    while (in.takeDelimiterExclusive('\n')) |line| {
        if (line.len == 0) {
            std.debug.print("empty\n", .{});
            break;
        }

        const dir: u8 = line[0];
        const amount = try std.fmt.parseInt(i16, line[1..], 10);

        switch (dir) {
            'L' => count -= amount,
            'R' => count += amount,
            else => unreachable,
        }

        count = @mod(count, 100);

        if (count == 0) {
            pass += 1;
        }

        std.debug.print("{c}{d}\t{d}\n", .{ dir, amount, count });
        in.toss(1);

        // reader.cl
    } else |err| {
        switch (err) {
            error.EndOfStream => {},
            error.ReadFailed => {
                std.debug.print("failed", .{});
            },
            error.StreamTooLong => {
                std.debug.print("Too Long", .{});
            },
        }
    }

    std.debug.print("{d}\n", .{pass});
}
