


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