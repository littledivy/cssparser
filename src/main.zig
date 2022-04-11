const std = @import("std");
const dbg = std.debug.print;

const TokenType = enum {
    Ident,
    Function,
    AtKeyword,
    Hash,
    String,
    BadString,
    URL,
    BadURL,
    Delim,
    Number,
    Percentage,
    Dimension,
    UnicodeRange,
    IncludeMatch,
    DashMatch,
    PrefixMatch,
    SuffixMatch,
    SubstringMatch,
    Column,
    WhiteSpace,
    CDO,
    CDC,
    Colon,
    Semicolon,
    Comma,
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    EOF,
};

const Token = struct {
    type_: TokenType,
    start: usize,
    end: usize,
};

const Lexer = struct {
    const Self = @This();

    buf: []u8,
    start: usize,
    pos: usize,

    fn read(self: *Self) ?u8 {
        if (self.pos >= self.buf.len) {
            return null;
        }
        const ch = self.buf[self.pos];
        self.pos += 1;
        return ch;
    }

    fn unread(self: *Self) void {
        if (self.pos > 0) {
            self.pos -= 1;
        }
    }

    fn peek(self: *Self, n: usize) ?u8 {
        if (self.pos + n >= self.buf.len) {
            return null;
        }
        return self.buf[self.pos];
    }

    fn next(self: *Self) ?Token {
        while (true) {
            const ch = self.read() orelse return null;
            if (isWhiteSpace(ch)) {
                self.skip_whitespace();
            }

            if (isDigit(ch)) {
                self.unread();
                return self.read_numeric();
            }
        }
    }

    fn skip_whitespace(self: *Self) void {
        while (true) {
            const ch = self.read() orelse return;
            if (isWhiteSpace(ch)) {
                continue;
            }
            self.unread();
            break;
        }
    }

    fn skip_escape(self: *Self) void {
        const ch = self.read() orelse return;
        if (ch == '\r') {
            const ch2 = self.read() orelse return;
            if (ch2 != '\n') {
                self.unread();
            }
        }
        self.unread();
    }

    fn peek_ident(self: *Self) bool {
        const ch = self.peek(0) orelse unreachable;
        if (ch == '-') {
            const next_ch = self.peek(1) orelse return false;
            return isNameStart(next_ch) or self.peek_escape();
        } else if (isNameStart(ch)) {
            return true;
        } else if (ch == '\\' and self.peek_escape()) {
            return true;
        }
        return false;
    }

    fn peek_escape(self: *Self) bool {
        const ch = self.peek(0) orelse return false;
        if (ch != '\\') {
            return false;
        }
        const next_ch = self.peek(1) orelse return false;
        return next_ch != '\n';
    }

    fn read_name(self: *Self) ?Token {
        while (true) {
            const ch = self.read() orelse return null;
            if (isName(ch)) {
                continue;
            } else if (self.peek_escape()) {
                self.skip_escape();
            } else {
                self.unread();
                break;
            }
        }

        return Token{
            .type_ = TokenType.Ident,
            .start = self.start,
            .end = self.pos,
        };
    }

    fn read_numeric(self: *Self) ?Token {
        self.read_number() orelse return null;
        const maybe_ch = self.read();
        if (maybe_ch) |ch| {
            if (self.peek_ident()) {
                self.read_name() orelse return null;
                return Token{
                    .type_ = TokenType.Dimension,
                    .start = self.start,
                    .end = self.end,
                };
            } else if (ch == '%') {
                return Token{
                    .type_ = TokenType.Percentage,
                    .start = self.start,
                    .end = self.pos,
                };
            } else {
                self.unread();
            }
        }

        return Token{
            .type_ = TokenType.Number,
            .start = self.start,
            .end = self.pos,
        };
    }

    fn read_number(self: *Self) ?Token {
        var ch = self.read() orelse return null;
        if (ch == '+' or ch == '-') {
            ch = self.read() orelse return null;
        }

        // Read the integral part
        self.skip_digits();
        const maybe_dec = self.read();
        if (maybe_dec) |dec| {
            // Read after the decimal point
            if (dec == '.') {
                self.skip_digits();
            } else {
                self.unread();
            }
        } else {
            // No decimal point, so we're done
            return Token{
                .type_ = TokenType.Number,
                .start = self.start,
                .end = self.pos,
            };
        }

        // Read scientific notation
        const maybe_exp = self.read();
        if (maybe_exp) |exp| {
            if (exp == 'e' or exp == 'E') {
                const maybe_sign = self.read();
                if (maybe_sign) |sign| {
                    if (sign == '+' or sign == '-') {
                        self.skip_digits();
                    } else if (isDigit(sign)) {
                        self.skip_digits();
                    }
                } else {
                    self.unread();
                }
            } else {
                self.unread();
            }
        }

        return Token{
            .type_ = TokenType.Number,
            .start = self.start,
            .end = self.pos,
        };
    }

    fn skip_digits(self: *Self) void {
        while (true) {
            const ch = self.read() orelse return;
            if (isDigit(ch)) {
                continue;
            }
            self.unread();
            break;
        }
    }
};

fn isWhiteSpace(ch: u8) bool {
    return ch == ' ' or ch == '\t' or ch == '\r' or ch == '\n';
}

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

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
}

const expect = std.testing.expect;

fn assert_tokens(src: []u8, expected: []TokenType) anyerror!void {
    var lexer = Lexer{
        .buf = src,
        .start = 0,
        .pos = 0,
    };

    var i: usize = 0;
    while (i < expected.len) : (i += 1) {
        const token = lexer.next() orelse unreachable;
        dbg("{}\n", .{token});
        dbg("{s}\n", .{src[token.start..token.end]});
        try expect(token.type_ == expected[i]);
    }

    try expect(lexer.next() == null);
}

test "basic lex" {
    {
        var buf = "  1.2".*;
        var expected = [_]TokenType{
            TokenType.Number,
        };
        try assert_tokens(&buf, &expected);
    }

    {
        var buf = "  1.2  1.2 1 3.14 1e3".*;
        var expected = [_]TokenType{
            TokenType.Number,
            TokenType.Number,
            TokenType.Number,
            TokenType.Number,
            TokenType.Number,
        };
        try assert_tokens(&buf, &expected);
    }
}
