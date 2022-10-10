use std::cmp::max;

use colored::Colorize;




#[derive(Debug, Clone, Copy, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub start_line: usize,
    pub end_line: usize,
    pub start_column: usize,
    pub end_column: usize,
}

#[derive(Debug, Clone)]
pub enum Error {
    Error(String, Span),
    WithHint(String, Span, String, Span),
}

pub fn print_errors(source: &str, errors: &Vec<Error>) {
    let lines = find_lines(source);

    if errors.is_empty() {
        println!("No errors o.o");
        return;
    }

    let max_line_number = errors.iter()
        .map(|e| {
            match e {
                Error::Error(_, span) => span.start_line,
                Error::WithHint(_, span, _, hint_span) => max(span.start_line, hint_span.start_line),
            }
        })
        .max()
        .unwrap();

    for error in errors {
        match error {
            Error::Error(msg, span) => {
                println!("{}: {}", "Error".red().bold(), msg.bold());
                println!("{}", format!("  --> {}:{}:{}", "samples/test1.klc", span.start_line, span.start_column).cyan());
                print_line(source, &lines, *span, max_line_number);
                println!("");
            },
            Error::WithHint(msg, span, hint_msg, hint_span) => {
                println!("{}: {}", "Error".red().bold(), msg.bold());
                println!("{}", format!("  --> {}:{}:{}", "samples/test1.klc", span.start_line, span.start_column).cyan());
                print_line(source, &lines, *span, max_line_number);

                println!("{}: {}", "Hint".yellow(), hint_msg);
                println!("{}", format!("  --> {}:{}:{}", "samples/test1.klc", hint_span.start_line, hint_span.start_column).cyan());
                print_line(source, &lines, *hint_span, max_line_number);
                println!("");
            },
        }
    }
}

fn find_lines(source: &str) -> Vec<usize> {
    let mut lines = vec![0];

    for (index, &char) in source.as_bytes().iter().enumerate() {
        if char == b'\n' {
            lines.push(index+1);
        }
    }

    lines.push(source.len());

    lines
}

// Lines start at index 1
fn print_line(source: &str, lines: &Vec<usize>, span: Span, max_line_number: usize) {
    let buffer = max_line_number.to_string().len();

    let line = span.start_line;
    let column = span.start_column;

    let line_number_str = line.to_string();
    let prefix_len = 1 + buffer - line_number_str.len();

    let prefix = " ".repeat(prefix_len);

    let empty_line_number = " ".repeat(1 + buffer + 1);

    println!("{}|", &empty_line_number);
    println!("{}{} | {}", prefix, line_number_str, &source[lines[line - 1]..lines[line]-1]);

    let prefix_columns = " ".repeat(column - 1);

    let mut span_len = if span.start_line == span.end_line {
        span.end_column - span.start_column
    } else {
        lines[line] - lines[line - 1] - 1 - span.start_column
    };

    if span_len == 0 {
        span_len = 1;
    }

    let underline = "^".repeat(span_len);
    if line + 1 < lines.len() {
        println!("{}| {}{}", &empty_line_number, prefix_columns, underline.red());
    }
}



//--------------------------------------------------
// Implementations
//--------------------------------------------------

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

    pub fn merge_into(&mut self, other: Span) {
        *self = self.merge(other);
    }

    pub fn after(&self) -> Span {
        Span {
            start: self.end,
            end: self.end,
            start_line: self.end_line,
            end_line: self.end_line,
            start_column: self.end_column,
            end_column: self.end_column,
        }
    }
}