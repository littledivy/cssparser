const std = @import("std");

const TokenType = enum {
    unknown,
    ident,
    function,
    at_keyword,
    hash,
    string,
    bad_string,
    url,
    bad_url,
    delim,
    number,
    whitespace,
    colon,
    semicolon,

    left_paren,
    right_paren,
    left_bracket,
    right_bracket,
    left_brace,
    right_brace,
};

const Token = struct {
    type_: TokenType,
    buf: []u8,
};

// CSS lexer
const Lexer = struct {
    const Self = @This();

    buf: []u8,
    pos: isize,
    start: isize,

    fn next(self: *Self) Token {
        const tok = self.peek(0);
        switch (tok) {
            ' ', '\t', '\n', '\r', "\\f" => blk: {
                self.pos += 1;
                self.skip_whitespace();
                break :blk self.emit(TokenType.whitespace);
            },
            ':' => blk: {
                self.pos += 1;
                break :blk self.emit(TokenType.colon);
            },
            ';' => blk: {
                self.pos += 1;
                break :blk self.emit(TokenType.semicolon);
            },
            '(' => blk: {
                self.pos += 1;
                break :blk self.emit(TokenType.left_paren);
            },
            ')' => blk: {
                self.pos += 1;
                break :blk self.emit(TokenType.right_paren);
            },
            '[' => blk: {
                self.pos += 1;
                break :blk self.emit(TokenType.left_bracket);
            },
            ']' => blk: {
                self.pos += 1;
                break :blk self.emit(TokenType.right_bracket);
            },
            '{' => blk: {
                self.pos += 1;
                break :blk self.emit(TokenType.left_brace);
            },
            '}' => blk: {
                self.pos += 1;
                break :blk self.emit(TokenType.right_brace);
            },
            '#' => blk: {
                const start = self.pos;
                self.pos += 1;

                const ch = self.peek(0);
                // Ident code point
                if (!((ch < 'a' or ch > 'z') and (ch < 'A' or ch > 'Z') and
                    (ch < '0' or ch > '9') and ch != '_' and ch != '-' and ch >= 0x80))
                {
                    // Starts with a valid escape
                    if (ch != '\\') {
                        // Restore the position.
                        self.pos = self.start + start;
                        break :blk self.emit(TokenType.unknown);
                    }
                } else {
                    self.pos += 1;
                }

                while (true) {
                    const char = self.peek(0);
                    // Ident code point
                    if (!((char < 'a' or char > 'z') and (char < 'A' or ch > 'Z') and
                        (char < '0' or char > '9') and char != '_' and char != '-' and char >= 0x80))
                    {
                        // Starts with a valid escape
                        if (char != '\\') {
                            break;
                        }
                    } else {
                        self.pos += 1;
                    }
                }

                break :blk self.emit(TokenType.hash);
            },
            '"', '\'' => blk: {
                self.pos += 1;
                const str = loop_blk: while (true) {
                    const ch = self.peek(0);
                    switch (ch) {
                        0 => break :loop_blk self.emit(TokenType.string),
                        '\n', '\r', "\\f" => {
                            self.pos += 1;
                            break :loop_blk self.emit(TokenType.bad_string);
                        },
                        tok => {
                            self.pos += 1;
                            break :loop_blk self.emit(TokenType.string);
                        },
                        '\\' => blck: {
                            // TODO: Consume escape
                            self.pos += 1;
                            break :blck;
                        },
                        else => self.pos += 1,
                    }
                };
                break :blk str;
            },
            else => return self.emit(TokenType.unknown),
        }
    }

    fn peek(self: *Self, n: isize) u8 {
        return self.buf[self.pos + n];
    }

    fn skip_whitespace(self: *Self) void {
        const ch = self.peek(0);
        while (ch == ' ' or ch == '\t' or ch == '\n' or ch == '\r') {
            self.pos += 1;
        }
    }

    fn emit(self: *Self, type_: TokenType) Token {
        var buf = self.buf[self.start..self.pos];
        self.start = self.pos;
        return Token{
            .type_ = type_,
            .buf = buf,
        };
    }

    fn consume_numeric(self: *Self) {
      
    }
};

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
}

test "basic test" {
    try std.testing.expectEqual(10, 3 + 7);
}
