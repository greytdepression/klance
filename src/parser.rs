use crate::lexer::{Span, Token};

#[derive(Debug, Clone)]
pub struct ParsedStruct<'src> {
    is_file_struct: bool,
    fields: Vec<ParsedField<'src>>,
    constants: Vec<ParsedConstantDefinition<'src>>,
    functions: Vec<ParsedFunction<'src>>,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct ParsedField<'src> {
    name: &'src str,
    name_span: Span,
    type_name: &'src str,
    type_span: Span,
    default_value: Option<ParsedExpression>,
    default_value_span: Option<Span>,
    visibility: Visibility,
    visibility_span: Span,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct ParsedConstantDefinition<'src> {
    name: &'src str,
    name_span: Span,
    type_name: &'src str,
    type_span: Span,
    value: ParsedComptimeExpression<'src>,
    span: Span,
}

#[derive(Debug, Clone)]
pub enum ParsedExpression {
    Junk(Span),
}

#[derive(Debug, Clone)]
pub enum ParsedComptimeExpression<'src> {
    StructDefinition(ParsedStruct<'src>, Span),
}

#[derive(Debug, Clone)]
pub struct ParsedFunction<'src> {
    name: &'src str,
    name_span: Span,
    parameters: Vec<ParsedParameter<'src>>,
    return_type_name: Option<&'src str>,
    return_type_span: Option<Span>,
    throws: bool,
    error_type_name: Option<&'src str>,
    error_type_span: Option<Span>,
    visibility: Visibility,
    code_block: ParsedCodeBlock<'src>,
}

#[derive(Debug, Clone)]
pub struct ParsedParameter<'src> {
    name: &'src str,
    name_span: Span,
    type_name: &'src str,
    type_span: Span,
    default_value: Option<ParsedExpression>,
    default_value_span: Option<Span>,
    anonymity: Anonymity,
    anonymity_span: Span,
    span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, Copy)]
pub enum Anonymity {
    Anonymous,
    Labeled,
}

#[derive(Debug, Clone)]
pub struct ParsedCodeBlock<'src> {
    statements: Vec<ParsedStatement<'src>>,
    span: Span,
}

#[derive(Debug, Clone)]
pub enum ParsedStatement<'src> {
    Expression(ParsedExpression),
    VariableDefinition(ParsedVariableDefinition<'src>),
    ConstantDefinition(ParsedConstantDefinition<'src>),
}

#[derive(Debug, Clone)]
pub struct ParsedVariableDefinition<'src> {
    name: &'src str,
    name_span: Span,
    type_name: &'src str,
    type_span: Span,
    mutability: Mutability,
    mutability_span: Span,
    span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum Mutability {
    Mutable,
    Immutable,
}

//--------------------------------------------------
// Parser
//--------------------------------------------------

#[derive(Debug, Clone)]
pub enum ParserError {
    Error(String, Span),
    WithHint(String, Span, String, Span),
}

pub struct Parser<'src> {
    tokens: Vec<Token<'src>>,
    token_index: usize,
    errors: Vec<ParserError>,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: Vec<Token<'src>>) -> Parser<'src> {
        Parser {
            tokens,
            token_index: 0,
            errors: vec![],
        }
    }

    pub fn errors<'a>(&'a self) -> &'a Vec<ParserError> {
        &self.errors
    }

    pub fn parse(&mut self) -> ParsedStruct<'src> {
        self.parse_struct(true).unwrap()
    }

    fn token(&self) -> Option<&Token<'src>> {
        if self.token_index < self.tokens.len() {
            Some(&self.tokens[self.token_index])
        } else {
            None
        }
    }

    fn peek(&self, n: usize) -> Option<&Token<'src>> {
        if self.token_index + n < self.tokens.len() {
            Some(&self.tokens[self.token_index + n])
        } else {
            None
        }
    }

    fn has_token(&self) -> bool {
        self.token_index < self.tokens.len()
    }

    fn inc(&mut self) -> bool {
        self.token_index += 1;
        self.token_index < self.tokens.len()
    }

    fn span(&self) -> Span {
        if let Some(tk) = self.token() {
            tk.span()
        } else {
            Span::default()
        }
    }

    // We are always going to make the parse function return options, but the
    // `parse_struct` function in particular will always return a valid parsed
    // struct.
    fn parse_struct(&mut self, is_file_struct: bool) -> Option<ParsedStruct<'src>> {
        let mut fields = vec![];
        let mut constants = vec![];
        let mut functions = vec![];

        let start_span = if let Some(tk) = self.token() {
            tk.span().clone()
        } else {
            Span::default()
        };

        while self.has_token() {


            // try parse a function
            if let Some(func) = self.parse_function() {
                functions.push(func);
            }
        }


        let end_span = if let Some(tk) = self.token() {
            tk.span()
        } else {
            start_span
        };

        Some(ParsedStruct {
            is_file_struct,
            fields,
            constants,
            functions,
            span: start_span.merge(end_span),
        })
    }

    fn expect_inc(&mut self) -> bool {
        if !self.inc() {

            self.errors.push(ParserError::Error(
                "Unexpected end of token stream in function definition.".into(),
                self.tokens[self.tokens.len() - 1].span().after()
            ));

            return false;
        }

        true
    }

    fn parse_function(&mut self) -> Option<ParsedFunction<'src>> {
        let inital_index = self.token_index;

        if !self.has_token() {
            return None;
        }

        use Token::*;

        let mut fun_span = self.span();

        let mut success = true;

        // TODO: 0. Visibility

        // 1. `fn` keyword
        if let Function(_) = self.token().unwrap() {} else {
            return None;
        }
        if !self.expect_inc() {
            return None;
        }

        // TODO: 2. Generics

        // 3. Identifier/Name
        const TEMP_NAME: &'static str = "#TEMPORARY_IDENTIFIER#";
        let mut name: &'src str = TEMP_NAME;
        let name_span = self.span();
        {
            if let &Identifier(nm, _) = self.token().unwrap() {
                name = nm;

                if !self.expect_inc() {
                    return None;
                }
            } else {
                self.errors.push(ParserError::WithHint(
                    "Unexpected token in function definition.".into(),
                    name_span,
                    format!("Expected Identifier token for function name; instead got:\n{:?}", self.token().unwrap()),
                    name_span
                ));
                success = false;
            }
        }

        fun_span = fun_span.merge(name_span);

        // 4. Parameters

        {
            if let LParen(_) = self.token().unwrap() {
                if !self.expect_inc() {
                    return None;
                }
            }

            // TODO: actually parse parameters not just parentheses

            if let RParen(_) = self.token().unwrap() {
                if !self.expect_inc() {
                    return None;
                }
            }
        }


        // TODO: 5. Error type

        // TODO: 6. Return type

        // 7. Code block
        let mut code_block: Option<ParsedCodeBlock> = None;
        if let Some(code_blk) = self.parse_code_block() {
            fun_span = fun_span.merge(code_blk.span);
            code_block = Some(code_blk);
        } else {
            self.errors.push(ParserError::Error(
                "Function definition lacks executable block.".into(),
                fun_span.after()
            ));

            success = false;
        }


        if success {
            Some(ParsedFunction {
                name: name,
                name_span,
                parameters: vec![],
                return_type_name: None,
                return_type_span: None,
                throws: false,
                error_type_name: None,
                error_type_span: None,
                visibility: Visibility::Private,
                code_block: code_block.unwrap(),
            })
        } else {
            None
        }
    }

    fn parse_code_block(&mut self) -> Option<ParsedCodeBlock<'src>> {
        if !self.has_token() {
            return None;
        }

        use Token::*;

        // 1. Open brace
        if let LBrace(_) = self.token().unwrap() {
            if !self.expect_inc() {
                return None;
            }
        }

        // TODO: 2. Code ig
        while !matches!(self.token(), Some(RBrace(_))) {
            if !self.expect_inc() {
                return None;
            }
        }

        // 3. Closing brace
        if let RBrace(_) = self.token().unwrap() {
            self.inc();

            return Some(ParsedCodeBlock {
                statements: vec![],
                span: self.span(),
            })
        }

        None
    }
}