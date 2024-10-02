build:
	@zig build

test:
	@zig test src/test.zig 2>&1|cat

tokenize:
	@zig build run -- tokenize test.lox

parse:
	@zig build run -- parse test.lox

fmt:
	@zig fmt .
