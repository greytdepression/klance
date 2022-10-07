use core::num;
use std::{cmp::{min, max}, str::Chars};



#[derive(Debug, Clone, Copy)]
pub struct Span {
    start: usize,
    end: usize,
    start_line: usize,
    end_line: usize,
    start_column: usize,
    end_column: usize,
}

#[derive(Debug, Clone)]
pub enum Token<'a> {
    Unknown(Span),
    Identifier(&'a str, Span),
    NumberLiteral(Vec<u8>, Span),

    // keywords
    Function(Span),
    Let(Span),
    Mut(Span),

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
    GreateThan(Span),
    LessThanOrEqual(Span),
    GreaterThanOrEqual(Span),
    Plus(Span),
    Minus(Span),
    Asterisk(Span),
    Slash(Span),
}

#[derive(Debug, Clone, Copy)]
pub enum LexingError {
    Generic(Span),
}

pub struct Lexer<'a> {
    source: &'a str,
    source_chars: Vec<char>,
    char_index: usize,
    byte_index: usize,
    line_index: usize,
    column_index: usize,
    source_len: usize,
    errors: Vec<LexingError>,
}

// Implementations

impl Span {
    pub fn merge(self, other: Span) -> Span {
        let (start, start_line, start_column) = if self.start <= other.start {
            (self.start, self.start_line, self.start_column)
        } else {
            (other.start, other.start_line, other.start_column)
        };

        let (end, end_line, end_column) = if self.end >= other.end {
            (self.end, self.end_line, self.end_column)
        } else {
            (other.end, other.end_line, other.end_column)
        };

        Span {
            start,
            end,
            start_line,
            end_line,
            start_column,
            end_column,
        }
    }
}

impl<'a> Token<'a> {
    pub fn span(&self) -> Span {
        use Token::*;
        match self {
            Unknown(span) |
            Identifier(_, span) |
            NumberLiteral(_, span) |
            Function(span) |
            Let(span) |
            Mut(span) |
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
            GreateThan(span) |
            LessThanOrEqual(span) |
            GreaterThanOrEqual(span) |
            Plus(span) |
            Minus(span) |
            Asterisk(span) |
            Slash(span) => *span
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
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
            start_line: self.line_index,
            end_line: self.line_index,
            start_column: self.column_index,
            end_column: self.column_index + 1,
        }
    }

    fn span_excl(&self) -> Span {
        Span {
            start: self.char_index,
            end: self.char_index,
            start_line: self.line_index,
            end_line: self.line_index,
            start_column: self.column_index,
            end_column: self.column_index,
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
        if self.char_index < self.source_len {
            self.byte_index += self.char().len_utf8();
            self.char_index += 1;
            self.column_index += 1;
        }
    }

    pub fn lex(&mut self) -> (Vec<Token<'a>>, Vec<LexingError>) {
        let mut tokens = Vec::with_capacity(self.source.len());

        assert!(self.source_len == self.source_chars.len());

        while self.char_index < self.source_len {
            if self.char().is_whitespace() {

                if self.char() == '\n' {
                    self.line_index += 1;
                    self.column_index = 1;
                }

                self.inc_index();
                continue;
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
                '+' | '-' | '*' | '/' | '<' | '>' => self.lex_operator(),
                _ => self.lex_number_or_name(),
            };

            // In the first couple of cases we need to manually increment the counter
            if self.char_index == prev_index {
                self.inc_index();
            }

            tokens.push(token);
        }

        (tokens, self.errors.clone())
    }

    fn lex_operator(&mut self) -> Token<'a> {
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
                _ => GreateThan(span),
            },
            _ => Token::Unknown(span),
        }
    }

    fn lex_number_or_name(&mut self) -> Token<'a> {
        if self.char().is_ascii() && !self.char().is_alphanumeric() {
            return Token::Unknown(self.span())
        }

        if self.char().is_numeric() {
            // We have a number

            // TODO: add support for binary and hex digits

            let mut digits = vec![self.char().to_digit(10).unwrap() as u8];
            self.inc_index();

            while self.char().is_numeric() {
                digits.push(self.char().to_digit(10).unwrap() as u8);

                self.inc_index();
            }

            return Token::NumberLiteral(digits, self.span());
        }

        // we have a keyword or an identifier
        self.lex_keyword_or_identifier()
    }

    fn lex_keyword_or_identifier(&mut self) -> Token<'a> {
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

    fn lex_keyword(&self, identifier: &'a str, span: Span) -> Option<Token<'a>> {
        match identifier {
            "fn" => Some(Token::Function(span)),
            "let" => Some(Token::Let(span)),
            "mut" => Some(Token::Mut(span)),
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