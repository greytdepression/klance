use core::num;
use std::{cmp::{min, max}, str::Chars};
use crate::{Span, Error};

#[derive(Debug, Clone, Copy)]
pub enum Token<'src> {
    Unknown(Span),
    Identifier(&'src str, Span),
    NumberLiteral(u128, Span),
    StringLiteral(&'src str, Span),
    BooleanLiteral(bool, Span),

    // keywords
    Function(Span),
    Let(Span),
    Mut(Span),
    If(Span),
    Else(Span),
    And(Span),
    Or(Span),
    Not(Span),
    Xor(Span),

    // special characters and operators
    LParen(Span),
    RParen(Span),
    LBrace(Span),
    RBrace(Span),
    LBracket(Span),
    RBracket(Span),
    Equals(Span),
    Semicolon(Span),
    Colon(Span),
    LessThan(Span),
    GreaterThan(Span),
    LessThanOrEqual(Span),
    GreaterThanOrEqual(Span),
    Plus(Span),
    Minus(Span),
    Asterisk(Span),
    Ampersand(Span),
    Slash(Span),
    Percent(Span),
}

pub struct Lexer<'src> {
    source: &'src str,
    source_chars: Vec<char>,
    char_index: usize,
    byte_index: usize,
    line_index: usize,
    column_index: usize,
    source_len: usize,
    errors: Vec<Error>,
}

// Implementations

impl<'src> Token<'src> {
    pub fn span(&self) -> Span {
        use Token::*;
        match self {
            Unknown(span) |
            Identifier(_, span) |
            NumberLiteral(_, span) |
            StringLiteral(_, span) |
            BooleanLiteral(_, span) |
            Function(span) |
            Let(span) |
            Mut(span) |
            If(span) |
            Else(span) |
            And(span) |
            Or(span) |
            Not(span) |
            Xor(span) |
            LParen(span) |
            RParen(span) |
            LBrace(span) |
            RBrace(span) |
            LBracket(span) |
            RBracket(span) |
            Equals(span) |
            Semicolon(span) |
            Colon(span) |
            LessThan(span) |
            GreaterThan(span) |
            LessThanOrEqual(span) |
            GreaterThanOrEqual(span) |
            Plus(span) |
            Minus(span) |
            Asterisk(span) |
            Ampersand(span) |
            Slash(span) |
            Percent(span) => *span
        }
    }
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        let chars: Vec<_> = source.chars().collect();
        let num_chars = chars.len();
        Lexer {
            source,
            source_chars: chars,
            char_index: 0,
            byte_index: 0,
            line_index: 1,
            column_index: 1,
            source_len: num_chars,
            errors: Vec::new(),
        }
    }

    fn span(&self) -> Span {
        Span {
            start: self.char_index,
            end: self.char_index + 1,
            after: false,
        }
    }

    fn span_excl(&self) -> Span {
        Span {
            start: self.char_index,
            end: self.char_index,
            after: false,
        }
    }

    fn char(&self) -> char {
        self.source_chars[self.char_index]
    }

    fn peek(&self, n: usize) -> Option<char> {
        if self.char_index + n < self.source_len {
            Some(self.source_chars[self.char_index + n])
        } else {
            None
        }
    }

    fn inc_index(&mut self) {
        let mut done = false;

        while self.char_index < self.source_len && self.char() == '\n' {
            self.column_index = 1;
            self.line_index += 1;
            self.byte_index += self.char().len_utf8();
            self.char_index += 1;

            done = true;
        }

        if !done && self.char_index < self.source_len {
            self.byte_index += self.char().len_utf8();
            self.char_index += 1;
            self.column_index += 1;
        }

        while self.char_index < self.source_len && self.char() == '\n' {
            self.column_index = 1;
            self.line_index += 1;
            self.byte_index += self.char().len_utf8();
            self.char_index += 1;
        }
    }

    pub fn lex(&mut self) -> Vec<Token<'src>> {
        let mut tokens = Vec::with_capacity(self.source.len());

        assert!(self.source_len == self.source_chars.len());

        while self.char_index < self.source_len {
            while self.char().is_whitespace() {
                self.inc_index();
            }

            let prev_index = self.char_index;

            use Token::*;
            let token = match self.char() {
                '(' => LParen(self.span()),
                ')' => RParen(self.span()),
                '[' => LBracket(self.span()),
                ']' => RBracket(self.span()),
                '{' => LBrace(self.span()),
                '}' => RBrace(self.span()),
                ';' => Semicolon(self.span()),
                '=' => Equals(self.span()),
                '"' => self.lex_string(),
                '+' | '-' | '*' | '/' | '<' | '>' | '&' | '%' => self.lex_operator(),
                _ => self.lex_number_or_name(),
            };

            // In the first couple of cases we need to manually increment the counter
            if self.char_index == prev_index {
                self.inc_index();
            }

            tokens.push(token);
        }

        tokens
    }

    fn lex_string(&mut self) -> Token<'src> {
        assert!(self.char() == '"');

        self.inc_index();


        let start_byte = self.byte_index;
        let start_span = self.span();

        let mut escape = false;
        while self.char_index < self.source_len {
            if self.char() == '"' && !escape {
                break;
            }

            escape = if self.char() == '\\' {
                !escape // if we already escape, then we just escaped the backslash itself
            } else {
                false
            };

            self.inc_index();
        }

        let end_byte = self.byte_index;

        self.inc_index();

        let span = start_span.merge(self.span_excl());
        let string = &self.source[start_byte..end_byte];

        Token::StringLiteral(string, span)
    }

    fn lex_operator(&mut self) -> Token<'src> {
        use Token::*;

        let ch = self.char();
        let span = self.span();

        self.inc_index();

        match ch {
            '+' => match self.char() {
                _ => Plus(span),
            },
            '-' => match self.char() {
                _ => Minus(span),
            },
            '*' => match self.char() {
                _ => Asterisk(span),
            },
            '/' => match self.char() {
                _ => Slash(span),
            },
            '&' => match self.char() {
                _ => Ampersand(span),
            },
            '%' => match self.char() {
                _ => Percent(span),
            },
            '<' => match self.char() {
                '=' => {
                    self.inc_index();
                    LessThanOrEqual(span.merge(self.span_excl()))
                },
                _ => LessThan(span),
            },
            '>' => match self.char() {
                '=' => {
                    self.inc_index();
                    GreaterThanOrEqual(span.merge(self.span_excl()))
                },
                _ => GreaterThan(span),
            },
            _ => Token::Unknown(span),
        }
    }

    fn lex_number_or_name(&mut self) -> Token<'src> {
        if self.char().is_ascii() && !self.char().is_alphanumeric() {
            return Token::Unknown(self.span())
        }

        if self.char().is_numeric() {
            // We have a number
            return self.lex_number();
        }

        // we have a keyword or an identifier
        self.lex_keyword_or_identifier()
    }

    fn lex_number(&mut self) -> Token<'src> {
        // TODO: add support for binary and hex digits

        let start_span = self.span();

        let mut value: u128 = 0;

        loop {

            if self.char() == '_' {
                self.inc_index();
                continue;
            }

            let literal = self.char().to_digit(10).unwrap() as u128;

            if let Some(value_times_10) = value.checked_mul(10) {
                value = value_times_10;
            } else {
                let span = start_span.merge(self.span());
                self.errors.push(Error::WithHint(
                    "Integer literal overflow.".into(),
                    span,
                    "Integer literals must currently fit within a 128 bit unsigned integer type.".into(),
                    span
                ));
            }

            if let Some(plus_digit) = value.checked_add(literal) {
                value = plus_digit;
            } else {
                let span = start_span.merge(self.span());
                self.errors.push(Error::WithHint(
                    "Integer literal overflow.".into(),
                    span,
                    "Integer literals must currently fit within a 128 bit unsigned integer type.".into(),
                    span
                ));
            }

            // We need to peek here to avoid adding a newline character to out number
            let next = self.peek(1);

            if let Some(next) = next {
                if !(next.is_numeric() || next == '_') {
                    break;
                }
            } else {
                break;
            }

            self.inc_index();
        }

        let span = start_span.merge(self.span());

        self.inc_index();

        Token::NumberLiteral(value, span)
    }

    fn lex_keyword_or_identifier(&mut self) -> Token<'src> {
        if !is_identifier_character(self.char(), true) {
            return Token::Unknown(self.span());
        }

        let span_start = self.span();
        let str_start = self.byte_index;

        self.inc_index();

        while is_identifier_character(self.char(), false) {
            self.inc_index();
        }

        let span_end = self.span_excl();
        let str_end = self.byte_index;

        let string = &self.source[str_start..str_end];
        let span = span_start.merge(span_end);

        if let Some(tk) = self.lex_keyword(string, span) {
            return tk
        }

        Token::Identifier(string, span_start.merge(span_end))
    }

    fn lex_keyword(&self, identifier: &'src str, span: Span) -> Option<Token<'src>> {
        use Token::*;
        match identifier {
            "fn" => Some(Function(span)),
            "let" => Some(Let(span)),
            "mut" => Some(Mut(span)),
            "if" => Some(If(span)),
            "else" => Some(Else(span)),
            "and" => Some(And(span)),
            "or" => Some(Or(span)),
            "not" => Some(Not(span)),
            "xor" => Some(Xor(span)),
            "true" => Some(BooleanLiteral(true, span)),
            "false" => Some(BooleanLiteral(false, span)),
            _ => None,
        }
    }
}

fn is_identifier_character(c: char, first_char: bool) -> bool {
    if c.is_numeric() {
        return !first_char;
    }

    if c.is_whitespace() {
        return false;
    }

    match c {
        '(' | ')' | '[' | ']' | '{' | '}' | '<' | '>' |
        '"' | '\'' | '!' | '@' | '#' | '$' | '%' | '^' | '&' |
        '*' | '-' | '+' | '=' | '|' | '\\' | '/' | '?' | '.' |
        ',' | ';' | ':' | '`' | '~' => false,
        _ => true,
    }
}