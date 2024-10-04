build:
	@zig build

test:
	@zig test src/test.zig

tokenize:
	@zig build run -- tokenize test.lox

parse:
	@zig build run -- parse test.lox

eval:
	@zig build run -- evaluate test.lox

run:
	@zig build run -- run test.lox

fmt:
	@zig fmt .
