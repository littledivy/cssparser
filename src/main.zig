const std = @import("std");
const dbg = std.debug.print;

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
    percentage,
    comment,
    cdc,
    cdo,
    custom_property_name,
    suffix_match,
    substring_match,
    dash_match,
    prefix_match,
    include_match,
    left_paren,
    right_paren,
    left_bracket,
    right_bracket,
    left_brace,
    right_brace,
    eof,
};

const Token = struct {
    type_: TokenType,
    buf: []u8,
};

// CSS lexer
const Lexer = struct {
    const Self = @This();

    buf: []u8,
    pos: usize,
    start: usize,

    fn next(self: *Self) Token {
        const tok = self.peek(0);
        const token = switch (tok) {
            ' ', '\t', '\n', '\r' => blk: {
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
                while (true) {
                    const ch = self.peek(0);
                    switch (ch) {
                        0 => break :blk self.emit(TokenType.string),
                        '\n', '\r' => {
                            self.pos += 1;
                            break :blk self.emit(TokenType.bad_string);
                        },
                        '\\' => {
                            // TODO: Consume escape
                            self.pos += 1;
                            break :blk self.emit(TokenType.bad_string);
                        },
                        else => {
                            self.pos += 1;
                            if (ch == tok) {
                                break :blk self.emit(TokenType.string);
                            }
                        },
                    }
                }
            },
            '+', '.' => blk: {
                if (self.consume_numeric()) {
                    if (self.peek(0) == '%') {
                        self.pos += 1;
                        break :blk self.emit(TokenType.percentage);
                    }
                    // TODO: dimensions
                    break :blk self.emit(TokenType.number);
                }

                break :blk self.emit(TokenType.unknown);
            },
            '-' => blk: {
                if (self.consume_numeric()) {
                    if (self.peek(0) == '%') {
                        self.pos += 1;
                        break :blk self.emit(TokenType.percentage);
                    }
                    // TODO: dimensions
                    break :blk self.emit(TokenType.number);
                }

                const maybe_ident_like = self.consume_ident_like();
                if (maybe_ident_like != TokenType.unknown) {
                    break :blk self.emit(TokenType.function);
                }

                if (self.peek(0) == '-' and self.peek(1) == '-' and self.peek(2) == '>') {
                    self.pos += 3;
                    break :blk self.emit(TokenType.cdc);
                }

                if (self.peek(1) == '-') {
                    self.pos += 1;
                    if (self.consume_ident()) {
                        break :blk self.emit(TokenType.custom_property_name);
                    } else {
                        self.pos -= 1;
                    }
                }

                break :blk self.emit(TokenType.unknown);
            },
            '@' => blk: {
                self.pos += 1;
                if (self.consume_ident()) {
                    break :blk self.emit(TokenType.at_keyword);
                } else {
                    self.pos -= 1;
                    break :blk self.emit(TokenType.unknown);
                }
            },
            '$' => blk: {
                if (self.peek(1) == '=') {
                    self.pos += 2;
                    break :blk self.emit(TokenType.suffix_match);
                }
                break :blk self.emit(TokenType.unknown);
            },
            '*' => blk: {
                if (self.peek(1) == '=') {
                    self.pos += 2;
                    break :blk self.emit(TokenType.substring_match);
                }
                break :blk self.emit(TokenType.unknown);
            },
            '|' => blk: {
                if (self.peek(1) == '=') {
                    self.pos += 2;
                    break :blk self.emit(TokenType.dash_match);
                } else {
                    // TODO: Column
                }
                break :blk self.emit(TokenType.unknown);
            },
            '^' => blk: {
                if (self.peek(1) == '=') {
                    self.pos += 2;
                    break :blk self.emit(TokenType.prefix_match);
                }
                break :blk self.emit(TokenType.unknown);
            },
            '~' => blk: {
                if (self.peek(1) == '=') {
                    self.pos += 2;
                    break :blk self.emit(TokenType.include_match);
                }
                break :blk self.emit(TokenType.unknown);
            },
            '/' => blk: {
                // Comment
                if (self.peek(1) == '*') {
                    self.pos += 2;
                    while (true) {
                        const ch = self.peek(0);
                        if (ch == 0) {
                            break :blk self.emit(TokenType.unknown);
                        }
                        if (ch == '*' and self.peek(1) == '/') {
                            self.pos += 2;
                            break :blk self.emit(TokenType.comment);
                        }
                        self.pos += 1;
                    }
                    break :blk self.emit(TokenType.comment);
                }
                break :blk self.emit(TokenType.unknown);
            },
            '<' => blk: {
                // CDO
                if (self.peek(1) == '!' and self.peek(2) == '-' and self.peek(3) == '-') {
                    self.pos += 4;
                    break :blk self.emit(TokenType.cdo);
                }
                break :blk self.emit(TokenType.unknown);
            },
            '\\' => blk: {
                const maybe_ident_like = self.consume_ident_like();
                if (maybe_ident_like != TokenType.unknown) {
                    break :blk self.emit(TokenType.function);
                }
                break :blk self.emit(TokenType.unknown);
            },
            'u', 'U' => blk: {
                // TODO: Unicode range
                // 1. single codepoint
                // 2. interval range
                // 3. wildcard range
                break :blk self.emit(TokenType.unknown);
            },
            0 => self.emit(TokenType.eof),
            else => blk: {
                if (self.consume_numeric()) {
                    break :blk self.emit(TokenType.number);
                }

                const maybe_ident_like = self.consume_ident_like();
                if (maybe_ident_like != TokenType.unknown) {
                    break :blk self.emit(maybe_ident_like);
                }

                break :blk self.emit(TokenType.unknown);
            },
        };

        self.pos += 1;
        return token;
    }

    fn peek(self: *Self, n: usize) u8 {
        if (self.pos + n >= self.buf.len) {
            return 0;
        }
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

    fn consume_numeric(self: *Self) bool {
        const start = self.pos;
        const maybe_sign_ch = self.peek(0);
        if (maybe_sign_ch == '+' or maybe_sign_ch == '-') {
            self.pos += 1;
        }

        // Consume the integral part
        const maybe_digit_ch = self.peek(0);
        const is_digit = maybe_digit_ch >= '0' and maybe_digit_ch <= '9';
        if (is_digit) {
            self.pos += 1;
            while (true) {
                const ch = self.peek(0);
                if (ch < '0' or ch > '9') {
                    self.pos += 1;
                } else {
                    break;
                }
            }
        }

        if (self.peek(0) == '.') {
            self.pos += 1;
            const maybe_digit = self.peek(0);
            if (maybe_digit < '0' or maybe_digit > '9') {
                self.pos += 1;
                while (true) {
                    const ch = self.peek(0);
                    if (ch < '0' or ch > '9') {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
            } else if (is_digit) {
                self.pos -= 1;
                return true;
            } else {
                self.pos = self.start + start;
                return false; // error
            }
        } else if (!is_digit) {
            self.pos = self.start + start;
            return false; // error
        }

        const mark = self.pos;
        const e_ch = self.peek(0);
        if (e_ch == 'e' or e_ch == 'E') {
            self.pos += 1;
            const maybe_sign = self.peek(0);
            if (maybe_sign == '+' or maybe_sign == '-') {
                self.pos += 1;
            }
            const maybe_digit = self.peek(0);
            if (!(maybe_digit < '0' or maybe_digit > '9')) {
                self.pos = self.start + mark;
                return true;
            }
            while (true) {
                const ch = self.peek(0);
                if (ch < '0' or ch > '9') {
                    self.pos += 1;
                } else {
                    break;
                }
            }
        }

        return true;
    }

    fn consume_ident(self: *Self) bool {
        // const start = self.pos;
        if (self.peek(0) == '-') {
            self.pos += 1;
        }
        while (true) {
            const char = self.peek(0);
            // Ident code point
            if (isName(char)) {
                self.pos += 1;
            } else {
                // TODO: Starts with a valid escape
                // if (char != '\\') {
                break;
                // }
            }
        }

        return true;
    }

    fn consume_ident_like(self: *Self) TokenType {
        if (self.consume_ident()) {
            if (self.peek(0) != '(') {
                return TokenType.ident;
            }

            if (self.peek(1) == 'u' and self.peek(2) == 'r' and self.peek(3) == 'l' and
                self.peek(4) == '(')
            {
                // TODO: Consume `url(...)`
                self.pos += 5;
                return TokenType.url;
            } else {
                self.pos += 1;
                return TokenType.function;
            }
        }
        return TokenType.unknown;
    }
};

fn isLetter(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z');
}

fn isDigit(ch: u8) bool {
    return ch >= '0' and ch <= '9';
}

fn isNonAscii(ch: u8) bool {
    return ch >= 0x80;
}

fn isNameStart(ch: u8) bool {
    return isLetter(ch) or isNonAscii(ch) or ch == '_';
}

fn isName(ch: u8) bool {
    return isNameStart(ch) or isDigit(ch) or ch == '-';
}

const NodeType = enum {
    unknown,
    at_rule,
    qualified_rule,
    declaration,
    component_value,
    preserved_tokens,
    function,
};

const Node = struct {
    type_: NodeType,
};

const Parser = struct {
    const Self = @This();

    lexer: *Lexer,
    curr_token: Token,

    // Parse declaration list
    fn next(self: *Self) Node {
        if (self.curr_token.type_ == TokenType.comment) {
            self.skip_tokens();
        }

        while (self.curr_token.type_ == TokenType.semicolon) {
            self.skip_tokens();
        }

        if (self.curr_token.type_ == TokenType.delim) {
            self.skip_tokens();
        }

        if (self.curr_token.type_ == TokenType.ident) {
            return self.parse_decl();
        }

        return Node{
            .type_ = NodeType.unknown,
        };
    }

    fn parse_decl(self: *Self) Node {
        if (self.curr_token.type_ == TokenType.ident) {
            const ident = self.curr_token.buf;
            self.skip_tokens();

            dbg("{s}\n", .{self.curr_token.type_});
            if (self.curr_token.type_ == TokenType.colon) {
                self.skip_tokens();
                if (self.curr_token.type_ == TokenType.ident) {
                    const value = self.curr_token.buf;
                    self.skip_tokens();
                    dbg("{s} {s}\n", .{ ident, value });
                    return Node{
                        .type_ = NodeType.declaration,
                        // .ident = ident,
                        // .value = value,
                    };
                }
            }
        }
        return Node{
            .type_ = NodeType.unknown,
        };
    }

    fn next_token(self: *Self) void {
        self.curr_token = self.lexer.next();
    }

    fn skip_tokens(self: *Self) void {
        self.next_token();
        while (self.curr_token.type_ == TokenType.whitespace or
            self.curr_token.type_ == TokenType.comment)
        {
            self.next_token();
        }
    }
};

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
}

const expect = std.testing.expect;
fn make_lexer(buf: []u8) Lexer {
    return Lexer{
        .buf = buf,
        .start = 0,
        .pos = 0,
    };
}

test "basic test" {
    var buf = [_]u8{ '1', '.', '0' };
    var lexer = make_lexer(&buf);
    const token = lexer.next();
    try expect(token.type_ == TokenType.number);
    try expect(lexer.start == 2);
    try expect(lexer.pos == 3);
}

test "ident test" {
    var buf = "hello 1".*;
    var lexer = make_lexer(&buf);
    const token = lexer.next();
    try expect(token.type_ == TokenType.ident);
    //try expect(std.mem.eql(u8, token.buf, &buf));
    const token2 = lexer.next();
    dbg("{}", .{token2});
    try expect(token2.type_ == TokenType.number);
}

test "basic parse" {
    var buf = "color: red;".*;
    var lexer = make_lexer(&buf);

    var parser = Parser{
        .lexer = &lexer,
        .curr_token = lexer.next(),
    };
    const node = parser.next();
    try expect(node.type_ == NodeType.declaration);
}
