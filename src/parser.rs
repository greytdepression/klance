use crate::{Span, Error};
use crate::lexer::Token;

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
    Expression(ParsedExpression, HasSemicolon),
    VariableDefinition(ParsedVariableDefinition<'src>),
    ConstantDefinition(ParsedConstantDefinition<'src>),
}

#[derive(Debug, Clone, Copy)]
pub enum HasSemicolon {
    Yes,
    No,
}

#[derive(Debug, Clone)]
pub struct ParsedVariableDefinition<'src> {
    name: &'src str,
    name_span: Span,
    type_name: Option<&'src str>,
    type_span: Option<Span>,
    mutability: Mutability,
    mutability_span: Option<Span>,
    initial_value: ParsedExpression,
    span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Debug, Clone)]
pub enum ParsedExpression {
    Junk(Span),
    Atom(ParsedExpressionAtom, Span),
    BinaryOperation(Box<ParsedExpression>, ParsedOperator, Box<ParsedExpression>, Span),
    UnaryPrefixOperation(ParsedOperator, Box<ParsedExpression>, Span),
    AccessOperation(Box<ParsedExpression>, Box<ParsedExpression>, Span),
}

impl ParsedExpression {
    pub fn span(&self) -> Span {
        match self {
            ParsedExpression::Junk(span) |
            ParsedExpression::Atom(_, span) |
            ParsedExpression::BinaryOperation(_, _, _, span) |
            ParsedExpression::UnaryPrefixOperation(_, _, span) |
            ParsedExpression::AccessOperation(_, _, span) => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParsedExpressionAtom {
    IntegerConstant(u128, Span),
}

impl ParsedExpressionAtom {
    pub fn span(&self) -> Span {
        match self {
            ParsedExpressionAtom::IntegerConstant(_, span) => *span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParsedOperator {
    Plus(Span),
    Minus(Span),
    Asterisk(Span),
    Slash(Span),
}

impl ParsedOperator {
    pub fn precedence(&self) -> i32 {
        match self {
            ParsedOperator::Plus(_) => 0,
            ParsedOperator::Minus(_) => 0,
            ParsedOperator::Asterisk(_) => 10,
            ParsedOperator::Slash(_) => 11,
        }
    }
}

//--------------------------------------------------
// Parser
//--------------------------------------------------

pub struct Parser<'src> {
    tokens: Vec<Token<'src>>,
    token_index: usize,
    errors: Vec<Error>,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: Vec<Token<'src>>) -> Parser<'src> {
        Parser {
            tokens,
            token_index: 0,
            errors: vec![],
        }
    }

    pub fn errors<'a>(&'a self) -> &'a Vec<Error> {
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

            self.errors.push(Error::Error(
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
                self.errors.push(Error::WithHint(
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
            self.errors.push(Error::Error(
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
        let mut statements = vec![];
        while !matches!(self.token(), Some(RBrace(_))) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            } else {
                break;
            }
        }

        // 3. Closing brace
        if let RBrace(_) = self.token().unwrap() {
            self.inc();

            return Some(ParsedCodeBlock {
                statements,
                span: self.span(),
            })
        }

        None
    }

    fn parse_statement(&mut self) -> Option<ParsedStatement<'src>> {
        use Token::*;

        if !self.has_token() {
            return None;
        }

        let mut maybe_stmt: Option<ParsedStatement<'src>> = None;
        let start_span = self.token().unwrap().span();
        let mut maybe_span = None;

        // Check if there is a variable definition
        if let Some(defn) = self.parse_variable_definition() {
            maybe_span = Some(defn.span);
            maybe_stmt = Some(ParsedStatement::VariableDefinition(defn));
        }

        // TODO: Check for other types of statements

        // We now still expect a semicolon
        if maybe_span.is_none() {
            maybe_span = Some(start_span);
        }

        if maybe_stmt.is_none() {
            return None;
        }

        if !matches!(self.token(), Some(Semicolon(_))) {
            self.errors.push(Error::WithHint(
                "Statement misses semicolon".into(),
                maybe_span.unwrap(),
                "Add a semicolon at the end of the statement.".into(),
                maybe_span.unwrap().after(),
            ));

            maybe_stmt = None;
        } else {
            self.inc();
        }

        maybe_stmt
    }

    fn parse_variable_definition(&mut self) -> Option<ParsedVariableDefinition<'src>> {
        use Token::*;

        // 1. `let` keyword
        if !matches!(self.token(), Some(Let(_))) {
            return None;
        }

        let mut success = true;
        let mut total_span = self.token().unwrap().span();

        let let_span = self.token().unwrap().span();

        if !self.expect_inc() {
            return None;
        }

        // 2. `mut` keyword?
        let (mutability, mutable_span) = if let Some(&Mut(span)) = self.token() {

            if !self.expect_inc() {
                return None;
            }

            total_span = total_span.merge(span);

            (Mutability::Mutable, Some(span))
        } else {
            (Mutability::Immutable, None)
        };

        // 3. Identifier
        let (var_name, var_name_span) = if let Some(&Identifier(name, span)) = self.token() {
            total_span = total_span.merge(span);

            (Some(name), Some(span))
        } else {
            // At this point we do not want to return None, since we already had a `let` keyword.
            // So this clearly is a variable assignment, if an incorrect one.
            success = false;
            (None, None)
        };

        if !self.expect_inc() {
            return None;
        }

        // TODO: 4. Type hint?

        // 5. Assignment equal sign
        if !matches!(self.token(), Some(Equals(_))) {
            self.errors.push(Error::WithHint(
                "Variable definition requires assignment!".into(),
                total_span,
                format!(
                    "Add a value assignment `let {}{} = value;`.",
                    if mutable_span.is_some() { "mut " } else { "" },
                    if var_name.is_some() { var_name.unwrap() } else { "foo" },
                ),
                total_span.after(),
            ));

            success = false;
        } else {
            self.inc();
        }

        // 6. Assignment value expression
        let value_expression = self.parse_expression();
        success &= value_expression.is_some();

        if success {
            Some(ParsedVariableDefinition{
                name: var_name.unwrap(),
                name_span: var_name_span.unwrap(),
                type_name: None,
                type_span: None,
                mutability,
                mutability_span: mutable_span,
                initial_value: value_expression.unwrap(),
                span: total_span,
            })
        } else {
            None
        }
    }

    fn parse_expression(&mut self) -> Option<ParsedExpression> {
        use Token::*;

        let mut last_precedence = 10000;

        let mut expressions: Vec<ParsedExpression> = vec![];
        let mut operators: Vec<ParsedOperator> = vec![];

        let mut span = self.span();

        loop {
            if !self.has_token() {
                return None;
            }

            if matches!(self.token(), Some(LParen(_))) {
                self.inc();

                let mut paren_span = self.span();

                if let Some(expr) = self.parse_expression() {
                    span = span.merge(expr.span());
                    paren_span = paren_span.merge(expr.span());
                    expressions.push(expr);
                }

                if !matches!(self.token(), Some(RParen(_))) {

                    // FIXME: Figure out if it is always safe to throw this error of if it might occur in
                    //        unwatned situations.
                    self.errors.push(Error::WithHint(
                        "Open paranthesis needs a closing partner.".into(),
                        paren_span,
                        "Expected closing `)` at this point. (This error might show up unwanted??)".into(),
                        paren_span.after(),
                    ));
                } else {
                    self.inc();
                }
            }

            if let Some(atom) = self.parse_expression_atom() {
                let span = atom.span();

                expressions.push(ParsedExpression::Atom(atom, span));
            }

            // TODO: Operators

            // TODO: break conditions

        }

        None
    }

    fn parse_expression_atom(&mut self) -> Option<ParsedExpressionAtom> {
        if !self.has_token() {
            return None;
        }

        use Token::*;
        match self.token().unwrap() {
            &NumberLiteral(value, span) => {
                self.inc();

                Some(ParsedExpressionAtom::IntegerConstant(value, span))
            },
            _ => None,
        }
    }

    fn parse_unary_prefix_operator(&mut self) -> Option<ParsedOperator> {
        if !self.has_token() {
            return None;
        }

        use Token::*;

        match self.token().unwrap() {
            &Plus(span) => Some(ParsedOperator::Plus(span)),
            &Minus(span) => Some(ParsedOperator::Minus(span)),
            &Asterisk(span) => Some(ParsedOperator::Asterisk(span)),
            _ => None,
        }
    }
}