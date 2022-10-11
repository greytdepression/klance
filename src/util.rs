use std::cmp::max;

use colored::Colorize;


#[derive(Debug, Clone, Copy, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub after: bool,
}

#[derive(Debug, Clone)]
pub enum Error {
    Error(String, Span),
    WithHint(String, Span, String, Span),
}

pub fn print_errors(source: &str, errors: &Vec<Error>) {
    let chars: Vec<char> = source.chars().collect();

    let lines = find_lines(&chars);

    if errors.is_empty() {
        println!("No errors o.o");
        return;
    }

    let find_line = |index| {
        match lines.binary_search(&index) {
            Ok(line) => line + 1,
            Err(line) => line,
        }
    };

    let max_line_number = errors.iter()
        .map(|e| {
            match e {
                Error::Error(_, span) => max(find_line(span.start), find_line(span.end)),
                Error::WithHint(_, span, _, hint_span) => max(
                    max(find_line(span.start), find_line(hint_span.start)),
                    max(find_line(span.end), find_line(hint_span.end))
                ),
            }
        })
        .max()
        .unwrap();

    let pos = |span: Span| {
        let line = find_line(span.start);
        let column = if !span.after {
            span.start - lines[line - 1] + 1
        } else {
            span.end - lines[line - 1] + 1
        };

        format!("{}:{}", line, column)
    };

    for error in errors {
        match error {
            Error::Error(msg, span) => {
                println!("{}: {}", "Error".red().bold(), msg.bold());
                println!("{}", format!("  --> {}:{}", "samples/test1.klc", pos(*span)).cyan()); // FIXME: use actual source location
                print_line(source, &lines, *span, max_line_number, Color::Red);
                println!("");
            },
            Error::WithHint(msg, span, hint_msg, hint_span) => {
                println!("{}: {}", "Error".red().bold(), msg.bold());
                println!("{}", format!("  --> {}:{}", "samples/test1.klc", pos(*span)).cyan());
                print_line(source, &lines, *span, max_line_number, Color::Red);

                println!("{}: {}", "Hint".yellow(), hint_msg);
                println!("{}", format!("  --> {}:{}", "samples/test1.klc", pos(*hint_span)).cyan());
                print_line(source, &lines, *hint_span, max_line_number, Color::Yellow);
                println!("");
            },
        }
    }
}

fn find_lines(source: &Vec<char>) -> Vec<usize> {
    let mut lines = vec![0];

    for (index, &char) in source.iter().enumerate() {
        if char == '\n' {
            lines.push(index+1);
        }
    }

    lines.push(source.len());

    lines
}

#[derive(Clone, Copy)]
enum Color {
    Red,
    Yellow,
}

// Lines start at index 1
fn print_line(source: &str, lines: &Vec<usize>, span: Span, max_line_number: usize, color: Color) {
    let buffer = max_line_number.to_string().len();

    let find_line = |index: usize| {
        match lines.binary_search(&index) {
            Ok(line) => line + 1,
            Err(line) => line,
        }
    };
    let pos = |span: Span| {
        let line = find_line(span.start);
        let column = if !span.after {
            span.start - lines[line - 1] + 1
        } else {
            span.end - lines[line - 1] + 1
        };

        (line, column)
    };

    let (line, column) = pos(span);

    let line_number_str = line.to_string();
    let prefix_len = 1 + buffer - line_number_str.len();

    let prefix = " ".repeat(prefix_len);

    let empty_line_number = " ".repeat(1 + buffer + 1);

    println!("{}|", &empty_line_number);
    println!("{}{} | {}", prefix, line_number_str, &source[lines[line - 1]..lines[line]-1]);

    let prefix_columns = " ".repeat(column - 1);

    let mut span_len = span.end - span.start;

    if span_len == 0 {
        span_len = 1;
    }

    let underline = "^".repeat(span_len);
    if line + 1 < lines.len() {
        let underline = if matches!(color, Color::Red) {
            underline.red()
        } else {
            underline.yellow()
        };

        println!("{}| {}{}", &empty_line_number, prefix_columns, underline);
    }
}



//--------------------------------------------------
// Implementations
//--------------------------------------------------

impl Span {
    pub fn merge(self, other: Span) -> Span {
        let start = if self.start <= other.start {
            self.start
        } else {
            other.start
        };

        let end = if self.end >= other.end {
            self.end
        } else {
            other.end
        };

        Span {
            start,
            end,
            after: false,
        }
    }

    pub fn merge_into(&mut self, other: Span) {
        *self = self.merge(other);
    }

    pub fn after(&self) -> Span {
        let start = if self.end > self.start { self.end - 1 } else { self.start };
        Span {
            start,
            end: start + 1,
            after: true,
        }
    }
}