use std::cmp::Ordering;
use std::fmt::Debug;

use crate::{Span, Error};
use crate::lexer::Token;
use crate::datastructure::{BinaryTree, Side};

#[derive(Debug, Clone)]
pub struct ParsedStruct<'src> {
    pub is_file_struct: bool,
    pub fields: Vec<ParsedField<'src>>,
    pub constants: Vec<ParsedConstantDefinition<'src>>,
    pub functions: Vec<ParsedFunction<'src>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ParsedField<'src> {
    pub name: &'src str,
    pub name_span: Span,
    pub type_name: &'src str,
    pub type_span: Span,
    pub default_value: Option<ExpressionId>,
    pub default_value_span: Option<Span>,
    pub visibility: Visibility,
    pub visibility_span: Span,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ParsedConstantDefinition<'src> {
    pub name: &'src str,
    pub name_span: Span,
    pub type_name: &'src str,
    pub type_span: Span,
    pub value: ParsedComptimeExpression<'src>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParsedComptimeExpression<'src> {
    StructDefinition(ParsedStruct<'src>, Span),
}

#[derive(Debug, Clone)]
pub struct ParsedFunction<'src> {
    pub name: &'src str,
    pub name_span: Span,
    pub parameters: Vec<ParsedParameter<'src>>,
    pub return_type_name: Option<&'src str>,
    pub return_type_span: Option<Span>,
    pub throws: bool,
    pub error_type_name: Option<&'src str>,
    pub error_type_span: Option<Span>,
    pub visibility: Visibility,
    pub code_block: ParsedCodeBlock<'src>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ParsedParameter<'src> {
    pub name: &'src str,
    pub name_span: Span,
    pub type_name: &'src str,
    pub type_span: Span,
    pub default_value: Option<ExpressionId>,
    pub default_value_span: Option<Span>,
    pub anonymity: Anonymity,
    pub anonymity_span: Span,
    pub span: Span,
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
    pub statements: Vec<ParsedStatement<'src>>,
    pub span: Span,
}

impl<'src> ParsedCodeBlock<'src> {
    fn empty(span: Span) -> Self {
        Self {
            statements: vec![],
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParsedStatement<'src> {
    NoOp(Span),
    Expression(ExpressionId, HasSemicolon),
    VariableDeclaration(ParsedVariableDeclaration<'src>),
    ConstantDefinition(ParsedConstantDefinition<'src>),
    IfStatement(ParsedIfStatement<'src>),
    Block(ParsedCodeBlock<'src>),
}

#[derive(Debug, Clone, Copy)]
pub enum HasSemicolon {
    Yes,
    No,
}

#[derive(Debug, Clone)]
pub struct ParsedIfStatement<'src> {
    pub condition: ExpressionId,
    pub then_block: ParsedCodeBlock<'src>,
    pub else_statment: Option<ParsedElseStatement<'src>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParsedElseStatement<'src> {
    Unconditional(ParsedCodeBlock<'src>, Span),
    Conditional(Box<ParsedIfStatement<'src>>, Span),
}

#[derive(Debug, Clone)]
pub struct ParsedVariableDeclaration<'src> {
    pub name: &'src str,
    pub name_span: Span,
    pub type_name: Option<&'src str>,
    pub type_span: Option<Span>,
    pub mutability: Mutability,
    pub mutability_span: Option<Span>,
    pub initial_value: ExpressionId,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Debug, Clone)]
pub enum ParsedExpression {
    Junk(Span),
    Atom(ExpressionAtomId, Span),
    BinaryOperation(ExpressionId, ParsedOperator, ExpressionId, Span),
}

impl ParsedExpression {
    pub fn span(&self) -> Span {
        use ParsedExpression::*;
        match self {
            Junk(span) |
            Atom(_, span) |
            BinaryOperation(_, _, _, span) => *span,
        }
    }

    fn to_string<'a>(&self, parser: &Parser<'a>) -> String {
        match self {
            ParsedExpression::Junk(_) => "Junk".into(),
            ParsedExpression::Atom(atom, _) => parser.get_atom(*atom).to_string(parser),
            ParsedExpression::BinaryOperation(lhs, op, rhs, _)
                => format!("({} {} {})", parser.get_expr(*lhs).to_string(parser), op.to_string(), parser.get_expr(*rhs).to_string(parser)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParsedExpressionAtom<'src> {
    Junk(Span),
    Unit(Span),
    IntegerConstant(u128, Span),
    BooleanConstant(bool, Span),
    EnclosedExpression(ExpressionId, Span),
    UnaryPrefixedAtom(ParsedOperator, ExpressionAtomId, Span),
    Variable(&'src str, Span),
    FunctionInvocation(&'src str, (), Span), //    pub TODO: add parameter type
}

impl<'src> ParsedExpressionAtom<'src> {
    pub fn span(&self) -> Span {
        use ParsedExpressionAtom::*;
        match self {
            Junk(span) |
            Unit(span) |
            IntegerConstant(_, span) |
            BooleanConstant(_, span) |
            EnclosedExpression(_, span) |
            UnaryPrefixedAtom(_, _, span) |
            Variable(_, span) |
            FunctionInvocation(_, _, span) => *span,
        }
    }

    fn to_string<'a>(&self, parser: &Parser<'a>) -> String {
        use ParsedExpressionAtom::*;
        match self {
            Junk(_) => "🚮".into(),
            Unit(_) => "()".into(),
            IntegerConstant(val, _) => format!("{}", val),
            BooleanConstant(val, _) => format!("{}", val),
            EnclosedExpression(expr, _) => format!("({})", parser.get_expr(*expr).to_string(parser)),
            UnaryPrefixedAtom(op, atom, _) => format!("({}{})", op.to_string(), parser.get_atom(*atom).to_string(parser)),
            &Variable(identifier, _) => String::from(identifier),
            &FunctionInvocation(name, _, _) => format!("{}()", name),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParsedOperator {
    // Debug
    Junk(Span),

    // Arithmetic
    UnaryPlus(Span),
    UnaryMinus(Span),
    BinaryAddition(Span),
    BinarySubtraction(Span),
    BinaryMultiplication(Span),
    BinaryDivision(Span),
    BinaryModulo(Span),

    // Boolean logic
    UnaryLogicalNot(Span),
    BinaryLogicalAnd(Span),
    BinaryLogicalOr(Span),
    BinaryLogicalXor(Span),

    // Pointers
    UnaryReference(Span),
    UnaryDereference(Span),
}

impl ToString for ParsedOperator {
    fn to_string(&self) -> String {
        use ParsedOperator::*;
        match self {
            Junk(_) => "🚮".into(),
            UnaryPlus(_) => "+".into(),
            UnaryMinus(_) => "-".into(),
            BinaryAddition(_) => "+".into(),
            BinarySubtraction(_) => "-".into(),
            BinaryMultiplication(_) => "*".into(),
            BinaryDivision(_) => "/".into(),
            BinaryModulo(_) => "%".into(),
            UnaryLogicalNot(_) => "not ".into(),
            BinaryLogicalAnd(_) => "and".into(),
            BinaryLogicalOr(_) => "or".into(),
            BinaryLogicalXor(_) => "xor".into(),
            UnaryReference(_) => "&".into(),
            UnaryDereference(_) => "*".into(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OperatorAssociativity {
    LeftToRight,
    RightToLeft,
}

impl ParsedOperator {
    pub fn span(&self) -> Span {
        use ParsedOperator::*;
        match self {
            Junk(span) |
            UnaryPlus(span) |
            UnaryMinus(span) |
            BinaryAddition(span) |
            BinarySubtraction(span) |
            BinaryMultiplication(span) |
            BinaryDivision(span) |
            BinaryModulo(span) |
            UnaryLogicalNot(span) |
            BinaryLogicalAnd(span) |
            BinaryLogicalOr(span) |
            BinaryLogicalXor(span) |
            UnaryReference(span) |
            UnaryDereference(span) => *span,
        }
    }

    /// Reference: https://en.cppreference.com/w/cpp/language/operator_precedence
    /// Everything is multiplied by 10 so we can insert our own categories.
    pub fn precedence(&self) -> i32 {
        use ParsedOperator::*;
        match self {
            UnaryLogicalNot(_) | UnaryPlus(_) | UnaryMinus(_) | UnaryReference(_) | UnaryDereference(_) => 30,
            BinaryMultiplication(_) | BinaryDivision(_) | BinaryModulo(_) => 50,
            BinaryAddition(_) | BinarySubtraction(_) => 60,
            BinaryLogicalAnd(_) => 140,
            BinaryLogicalXor(_) => 145,
            BinaryLogicalOr(_) => 150,
            Junk(_) => 10000,
        }
    }

    /// Reference: https://en.cppreference.com/w/cpp/language/operator_precedence
    pub fn associativity(&self) -> OperatorAssociativity {
        use ParsedOperator::*;
        use OperatorAssociativity::*;
        match self {
            UnaryLogicalNot(_) | UnaryPlus(_) | UnaryMinus(_) | UnaryReference(_) | UnaryDereference(_) => RightToLeft,
            BinaryMultiplication(_) | BinaryDivision(_) | BinaryModulo(_) => LeftToRight,
            BinaryAddition(_) | BinarySubtraction(_) => LeftToRight,
            BinaryLogicalAnd(_) => LeftToRight,
            BinaryLogicalXor(_) => LeftToRight,
            BinaryLogicalOr(_) => LeftToRight,
            Junk(_) => LeftToRight,
        }
    }
}

impl ParsedOperator {
    fn cmp(&self, rhs: &ParsedOperator) -> Ordering {
        use Ordering::*;
        match self.precedence().cmp(&rhs.precedence()) {
            Ordering::Less => Less,
            Ordering::Greater => Greater,
            Ordering::Equal => match self.associativity() {
                OperatorAssociativity::LeftToRight => Less,
                OperatorAssociativity::RightToLeft => Greater,
            },
        }
    }
}

//--------------------------------------------------
// Parser
//--------------------------------------------------

pub struct Parser<'src> {
    pub tokens: Vec<Token<'src>>,
    pub token_index: usize,
    pub errors: Vec<Error>,

    // data
    pub expressions: Vec<ParsedExpression>,
    pub expression_atoms: Vec<ParsedExpressionAtom<'src>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ExpressionId(usize);

#[derive(Debug, Clone, Copy)]
pub struct ExpressionAtomId(usize);

use ParsingResult::*;

impl<'src> Parser<'src> {
    pub fn new(tokens: Vec<Token<'src>>) -> Parser<'src> {
        Parser {
            tokens,
            token_index: 0,
            errors: vec![],
            expressions: vec![],
            expression_atoms: vec![],
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

    fn eof(&self) -> bool {
        !self.has_token()
    }

    fn inc(&mut self) -> bool {
        self.token_index += 1;
        self.token_index < self.tokens.len()
    }

    fn span(&self) -> Span {
        if let Some(tk) = self.token() {
            tk.span()
        } else if let Some(tk) = self.tokens.last() {
            tk.span().after()
        } else {
            Span::default()
        }
    }

    fn current_expr(&self) -> ExpressionId {
        ExpressionId(self.expressions.len() - 1)
    }

    fn current_expr_atom(&self) -> ExpressionAtomId {
        ExpressionAtomId(self.expression_atoms.len() - 1)
    }

    fn get_expr(&self, id: ExpressionId) -> &ParsedExpression {
        &self.expressions[id.0]
    }

    fn get_atom(&self, id: ExpressionAtomId) -> &ParsedExpressionAtom {
        &self.expression_atoms[id.0]
    }

    fn expr_span(&self, id: ExpressionId) -> Span {
        self.get_expr(id).span()
    }

    fn atom_span(&self, id: ExpressionAtomId) -> Span {
        self.get_atom(id).span()
    }

    fn stmt_span(&self, stmt: &ParsedStatement) -> Span {
        use ParsedStatement::*;
        match stmt {
            &NoOp(span) => span,
            &Expression(id, _) => self.expr_span(id),
            VariableDeclaration(var_defn) => var_defn.span,
            ConstantDefinition(const_defn) => const_defn.span,
            IfStatement(if_stmt) => if_stmt.span,
            Block(block) => block.span,
        }
    }

    fn junk_expr(&mut self, maybe_span: Option<Span>) -> ExpressionId {
        let span = if let Some(span) = maybe_span {
            span
        } else {
            self.span()
        };

        self.expressions.push(ParsedExpression::Junk(span));

        self.current_expr()
    }

    fn junk_atom(&mut self, maybe_span: Option<Span>) -> ExpressionAtomId {
        let span = if let Some(span) = maybe_span {
            span
        } else {
            self.span()
        };

        self.expression_atoms.push(ParsedExpressionAtom::Junk(span));

        self.current_expr_atom()
    }

    // We are always going to make the parse function return options, but the
    // `parse_struct` function in particular will always return a valid parsed
    // struct.
    fn parse_struct(&mut self, is_file_struct: bool) -> ParsingResult<ParsedStruct<'src>> {
        let mut fields = vec![];
        let mut constants = vec![];
        let mut functions = vec![];

        let start_span = if let Some(tk) = self.token() {
            tk.span().clone()
        } else {
            Span::default()
        };

        let mut success = true;

        while self.has_token() && success {


            // try parse a function
            if let Success(func) = self.parse_function() {
                functions.push(func);
            } else {
                success = false;
            }
        }


        let end_span = if let Some(tk) = self.token() {
            tk.span()
        } else {
            start_span
        };

        Success(ParsedStruct {
            is_file_struct,
            fields,
            constants,
            functions,
            span: start_span.merge(end_span),
        })
    }

    fn parse_function(&mut self) -> ParsingResult<ParsedFunction<'src>> {
        if !self.has_token() {
            return EOF(None, None);
        }

        use Token::*;

        let mut fun_span = self.span();

        let mut success = true;
        let mut maybe_err_span: Option<Span> = None;

        // TODO: 0. Visibility

        // 1. `fn` keyword
        if !matches!(self.token(), Some(Function(_))) {
            return WrongType;
        }
        fun_span.merge_into(self.token().unwrap().span());
        self.inc();

        // TODO: 2. Generics

        // 3. Identifier/Name
        let mut name: &'src str = "(missing identifier)";
        let name_span = self.span();
        {
            if let Some(&Identifier(nm, span)) = self.token() {
                name = nm;

                fun_span.merge_into(span);
            } else {
                self.errors.push(Error::WithHint(
                    "Function definition misses identifier.".into(),
                    fun_span,
                    "Expected name of function at this place.".into(),
                    name_span
                ));

                maybe_err_span = Some(fun_span.after());

                success = false;
            }
        }

        fun_span = fun_span.merge(name_span);
        self.inc();

        // 4. Parameters
        {
            if let Some(&LParen(span)) = self.token() {
                fun_span.merge_into(span);
            } else {
                self.errors.push(Error::WithHint(
                    "Function definition misses paranthesis pair.".into(),
                    fun_span,
                    "Expected opening parenthesis `(` at this place.".into(),
                    fun_span.after(),
                ));

                maybe_err_span = Some(fun_span.after());

                success = false;
            }
            self.inc();

            // TODO: actually parse parameters not just parentheses

            if let Some(&RParen(span)) = self.token() {
                fun_span.merge_into(span);
            } else {
                self.errors.push(Error::WithHint(
                    "Function definition misses paranthesis pair.".into(),
                    fun_span,
                    "Expected closing parenthesis `)` at this place.".into(),
                    fun_span.after(),
                ));

                maybe_err_span = Some(fun_span.after());

                success = false;
            }
            self.inc();
        }


        // TODO: 5. Error type

        // TODO: 6. Return type

        // 7. Code block
        let dummy_block = ParsedCodeBlock::empty(fun_span.after());
        let mut has_code_block = true;
        let code_block = match self.parse_code_block() {
            Success(t) => {
                fun_span.merge_into(t.span);

                t
            },
            WrongType | EOF(None, None) => {
                success = false;
                has_code_block = false;
                maybe_err_span = Some(fun_span.after());

                dummy_block
            },
            SyntaxError(maybe_t, err_span) | EOF(maybe_t, Some(err_span)) => {
                success = false;
                has_code_block = true;
                maybe_err_span = Some(err_span);

                maybe_t.unwrap_or(dummy_block)
            },
            EOF(Some(t), None) => {
                success = false;
                has_code_block = true;
                maybe_err_span = Some(fun_span.after());

                t
            },
        };

        if !has_code_block {
            self.errors.push(Error::WithHint(
                "Function definition misses executable block.".into(),
                fun_span,
                "Expected code block here".into(),
                fun_span.after(),
            ));
        }

        let parsed_fun = ParsedFunction {
            name: name,
            name_span,
            parameters: vec![],
            return_type_name: None,
            return_type_span: None,
            throws: false,
            error_type_name: None,
            error_type_span: None,
            visibility: Visibility::Private,
            code_block: code_block,
            span: fun_span,
        };

        if success {
            Success(parsed_fun)
        } else {
            if self.eof() {
                EOF(Some(parsed_fun), maybe_err_span)
            } else {
                SyntaxError(Some(parsed_fun), maybe_err_span.unwrap())
            }
        }
    }

    fn parse_code_block(&mut self) -> ParsingResult<ParsedCodeBlock<'src>> {
        if !self.has_token() {
            return EOF(None, None);
        }

        use Token::*;


        let mut block_span = self.token().unwrap().span();

        // 1. Open brace
        if let Some(&LBrace(_)) = self.token() {
            self.inc();
        } else {
            return WrongType;
        }

        let mut success = true;

        let mut maybe_err_span: Option<Span> = None;


        // TODO: 2. Code ig
        let mut statements = vec![];
        while !matches!(self.token(), Some(RBrace(_))) {
            match self.parse_statement() {
                Success(t) => {
                    block_span.merge_into(self.stmt_span(&t));
                    statements.push(t);
                },
                WrongType => break, // FIXME: maybe we don't want to break here?
                SyntaxError(Some(t), err_span) => {
                    block_span.merge(self.stmt_span(&t));
                    statements.push(t);

                    block_span.merge_into(err_span);
                    maybe_err_span = Some(err_span);
                },
                EOF(maybe_t, mb_err_span) => {
                    if let Some(t) = maybe_t {
                        block_span.merge(self.stmt_span(&t));
                        statements.push(t);
                    }

                    if let Some(err_span) = mb_err_span {
                        maybe_err_span = Some(err_span);
                        block_span.merge_into(err_span);
                    }

                    // EOF, so we can break anyway
                    break;
                },
                SyntaxError(None, err_span) => {
                    maybe_err_span = Some(err_span);
                    block_span.merge_into(err_span);

                    break; // FIXME: same as above
                },
            }
        }

        // 3. Closing brace
        if let Some(&RBrace(span)) = self.token() {
            self.inc();

            block_span.merge_into(span);
        } else {
            self.errors.push(Error::WithHint(
                "Block misses closing brace.".into(),
                block_span,
                "Expected `}` here.".into(),
                block_span.after(),
            ));

            success = false;
        }

        let block = ParsedCodeBlock {
            statements,
            span: block_span,
        };
        if success {
            Success(block)
        } else {
            if self.eof() {
                EOF(Some(block), maybe_err_span)
            } else {
                SyntaxError(Some(block), maybe_err_span.unwrap_or(block_span))
            }
        }
    }

    fn parse_statement(&mut self) -> ParsingResult<ParsedStatement<'src>> {
        use Token::*;

        if !self.has_token() {
            return EOF(None, None);
        }

        let mut maybe_stmt: Option<ParsedStatement<'src>> = None;
        let start_span = self.token().unwrap().span();
        let mut maybe_span = None;

        let mut maybe_err_span: Option<Span> = None;

        let mut needs_semicolon = true;

        let mut success = true;

        // This loop is just so that we can break out at any point if we want to
        loop {
            // 0. Check if there is a lonely semicolon
            if matches!(self.token(), Some(Semicolon(_))) {
                maybe_span = Some(self.token().unwrap().span());
                maybe_stmt = Some(ParsedStatement::NoOp(self.token().unwrap().span()));

                // FIXME: Emit warning about this semicolon

                self.inc();

                needs_semicolon = false;
                break;
            }

            // 1. Check if there is a variable definition
            let var_decl = self.parse_variable_declaration();
            match var_decl {
                Success(t) => {
                    maybe_span = Some(t.span);
                    maybe_stmt = Some(ParsedStatement::VariableDeclaration(t));

                    needs_semicolon = true;
                    break;
                },
                WrongType => {},
                EOF(None, None) => break,
                SyntaxError(None, err_span) | EOF(None, Some(err_span)) => {
                    let dummy = ParsedVariableDeclaration {
                        name: "(missing identifier)",
                        name_span: err_span,
                        type_name: None,
                        type_span: None,
                        mutability: Mutability::Immutable,
                        mutability_span: None,
                        initial_value: self.junk_expr(Some(err_span)),
                        span: err_span,
                    };
                    maybe_stmt = Some(ParsedStatement::VariableDeclaration(dummy));
                    maybe_span = Some(err_span);
                    maybe_err_span = Some(err_span);

                    success = false;
                    break;
                },
                SyntaxError(Some(t), err_span) => {
                    maybe_span = Some(t.span);
                    maybe_stmt = Some(ParsedStatement::VariableDeclaration(t));
                    maybe_err_span = Some(err_span);

                    success = false;
                    break;
                },
                EOF(Some(t), eof_maybe_span) => {
                    maybe_span = Some(t.span);
                    maybe_stmt = Some(ParsedStatement::VariableDeclaration(t));
                    maybe_err_span = eof_maybe_span;

                    success = false;
                    break;
                },
            }

            // 2. Check if there is an if statement
            match self.parse_if_statement() {
                Success(if_stmt) => {
                    maybe_span = Some(if_stmt.span);
                    maybe_stmt = Some(ParsedStatement::IfStatement(if_stmt));

                    needs_semicolon = false;
                    break;
                },
                WrongType => {},
                EOF(None, None) => break,
                SyntaxError(None, err_span) | EOF(None, Some(err_span)) => {
                    let dummy = ParsedIfStatement {
                        condition: self.junk_expr(Some(err_span)),
                        then_block: ParsedCodeBlock::empty(err_span),
                        else_statment: None,
                        span: err_span,
                    };
                    maybe_stmt = Some(ParsedStatement::IfStatement(dummy));
                    maybe_span = Some(err_span);
                    maybe_err_span = Some(err_span);

                    success = false;
                    break;
                },
                SyntaxError(Some(t), err_span) => {
                    maybe_span = Some(t.span);
                    maybe_stmt = Some(ParsedStatement::IfStatement(t));
                    maybe_err_span = Some(err_span);

                    success = false;
                    break;
                },
                EOF(Some(t), eof_maybe_span) => {
                    maybe_span = Some(t.span);
                    maybe_stmt = Some(ParsedStatement::IfStatement(t));
                    maybe_err_span = eof_maybe_span;

                    success = false;
                    break;
                },
            }

            // 3. Check if there is a code block
            match self.parse_code_block() {
                Success(block) => {
                    maybe_span = Some(block.span);
                    maybe_stmt = Some(ParsedStatement::Block(block));

                    needs_semicolon = false;
                    break;
                },
                WrongType => {},
                EOF(None, None) => break,
                SyntaxError(None, err_span) | EOF(None, Some(err_span)) => {
                    let dummy = ParsedCodeBlock::empty(err_span);
                    maybe_stmt = Some(ParsedStatement::Block(dummy));
                    maybe_span = Some(err_span);
                    maybe_err_span = Some(err_span);

                    success = false;
                    break;
                },
                SyntaxError(Some(t), err_span) => {
                    maybe_span = Some(t.span);
                    maybe_stmt = Some(ParsedStatement::Block(t));
                    maybe_err_span = Some(err_span);

                    success = false;
                    break;
                },
                EOF(Some(t), efo_maybe_span) => {
                    maybe_span = Some(t.span);
                    maybe_stmt = Some(ParsedStatement::Block(t));
                    maybe_err_span = efo_maybe_span;

                    success = false;
                    break;
                },
            }

            // TODO: Check for other types of statements

            break;
        }

        // We now still expect a semicolon
        if maybe_span.is_none() {
            maybe_span = Some(start_span);
        }

        let span = maybe_span.unwrap();

        if success && maybe_stmt.is_none() {
            return WrongType;
        }

        if !success && maybe_stmt.is_none() {
            // this should probably not even happen
            return if self.eof() {
                EOF(None, Some(span))
            } else {
                SyntaxError(None, span)
            };
        }

        let stmt = maybe_stmt.unwrap();

        if needs_semicolon {
            if !matches!(self.token(), Some(Semicolon(_))) {
                self.errors.push(Error::WithHint(
                    "Statement misses semicolon".into(),
                    maybe_span.unwrap(),
                    "Add a semicolon at the end of the statement.".into(),
                    maybe_span.unwrap().after(),
                ));

                success = false;
            } else {
                self.inc();
            }
        }

        if success {
            Success(stmt)
        } else if self.eof() {
            EOF(Some(stmt), maybe_err_span)
        } else {
            SyntaxError(Some(stmt), maybe_err_span.unwrap_or(span))
        }
    }

    fn parse_if_statement(&mut self) -> ParsingResult<ParsedIfStatement<'src>> {
        if !self.has_token() {
            return EOF(None, None);
        }

        use Token::*;

        // 1. `if` keyword
        if !matches!(self.token().unwrap(), If(_)) {
            return WrongType;
        }

        let mut total_span = self.span();
        let mut success = true;

        let mut maybe_err_span: Option<Span> = None;

        self.inc();

        // 2. Condition expression
        let mut has_cond = true;
        let cond = match self.parse_expression() {
            Success(t) => t,
            WrongType | EOF(None, None) => {
                success = false;
                has_cond = false;
                maybe_err_span = Some(total_span.after());

                self.junk_expr(Some(total_span.after()))
            },
            EOF(None, Some(err_span)) | SyntaxError(None, err_span) => {
                success = false;
                has_cond = false;
                maybe_err_span = Some(err_span);

                self.junk_expr(Some(total_span.after()))
            },
            SyntaxError(Some(t), err_span) | EOF(Some(t), Some(err_span)) => {
                success = false;
                maybe_err_span = Some(err_span);

                t
            },
            EOF(Some(t), None) => {
                success = false;
                maybe_err_span = Some(total_span.after());

                t
            },
        };

        if !has_cond {
            self.errors.push(Error::WithHint(
                "If statement misses condition expression.".into(),
                total_span,
                "Expected boolean expression here.".into(),
                total_span.after(),
            ));
        }

        total_span.merge_into(self.expr_span(cond));

        // 3. Code block
        let mut has_code_block = true;
        let block = match self.parse_code_block() {
            Success(t) => t,
            WrongType | EOF(None, None) => {
                success = false;
                has_code_block = false;
                maybe_err_span = Some(total_span.after());

                ParsedCodeBlock::empty(total_span.after())
            },
            EOF(None, Some(err_span)) | SyntaxError(None, err_span) => {
                success = false;
                has_code_block = false;
                maybe_err_span = Some(err_span);

                ParsedCodeBlock::empty(total_span.after())
            },
            SyntaxError(Some(t), err_span) | EOF(Some(t), Some(err_span)) => {
                success = false;
                maybe_err_span = Some(err_span);

                t
            },
            EOF(Some(t), None) => {
                success = false;
                maybe_err_span = Some(total_span.after());

                t
            },
        };

        if !has_code_block {
            self.errors.push(Error::WithHint(
                "If statement misses then block.".into(),
                total_span,
                "If statements need to be of the form `if condition { ... }`".into(),
                total_span.after(),
            ));
        }

        total_span.merge_into(block.span);

        // 4. Else block?
        let else_stmt = if matches!(self.token(), Some(Else(_))) {
            let else_span = self.token().unwrap().span();
            self.inc();

            let if_stmt = match self.parse_if_statement() {
                Success(t) => Some(t),
                WrongType | EOF(None, None) => None,
                SyntaxError(Some(t), err_span) | EOF(Some(t), Some(err_span)) => {
                    success = false;

                    Some(t)
                },
                EOF(Some(t), None) => {
                    success = false;

                    maybe_err_span = Some(total_span.after());

                    Some(t)
                },
                SyntaxError(None, err_span) | EOF(None, Some(err_span)) => {
                    success = false;

                    maybe_err_span = Some(err_span);

                    Some(ParsedIfStatement {
                        condition: self.junk_expr(Some(err_span)),
                        then_block: ParsedCodeBlock::empty(err_span),
                        else_statment: None,
                        span: err_span,
                    })
                },
            };

            if if_stmt.is_some() {
                Some(ParsedElseStatement::Conditional(Box::new(if_stmt.unwrap()), else_span))
            } else {
                let block = match self.parse_code_block() {
                    Success(t) => t,
                    WrongType | EOF(None, None) => {
                        success = false;
                        has_code_block = false;
                        maybe_err_span = Some(total_span.after());

                        ParsedCodeBlock::empty(total_span.after())
                    },
                    EOF(None, Some(err_span)) | SyntaxError(None, err_span) => {
                        success = false;
                        has_code_block = false;
                        maybe_err_span = Some(err_span);

                        ParsedCodeBlock::empty(total_span.after())
                    },
                    SyntaxError(Some(t), err_span) | EOF(Some(t), Some(err_span)) => {
                        success = false;
                        maybe_err_span = Some(err_span);

                        t
                    },
                    EOF(Some(t), None) => {
                        success = false;
                        maybe_err_span = Some(total_span.after());

                        t
                    },
                };

                Some(ParsedElseStatement::Unconditional(block, else_span))
            }
        } else {
            None
        };

        if success {
            Success(ParsedIfStatement {
                condition: cond,
                then_block: block,
                else_statment: else_stmt,
                span: total_span,
            })
        } else {
            let dummy = ParsedIfStatement {
                condition: cond,
                then_block: block,
                else_statment: else_stmt,
                span: total_span,
            };

            if self.eof() {
                EOF(Some(dummy), maybe_err_span)
            } else {
                SyntaxError(Some(dummy), maybe_err_span.unwrap_or(total_span))
            }
        }
    }

    fn parse_variable_declaration(&mut self) -> ParsingResult<ParsedVariableDeclaration<'src>> {
        if !self.has_token() {
            return EOF(None, None);
        }

        use Token::*;

        // 1. `let` keyword
        if !matches!(self.token(), Some(Let(_))) {
            return WrongType;
        }
        let mut total_span = self.token().unwrap().span();

        self.inc();

        let mut success = true;

        let mut maybe_err_span: Option<Span> = None;

        // 2. `mut` keyword?
        let (mutability, mutable_span) = if let Some(&Mut(span)) = self.token() {

            self.inc();

            total_span.merge_into(span);

            (Mutability::Mutable, Some(span))
        } else {
            (Mutability::Immutable, None)
        };

        // 3. Identifier
        let (var_name, var_name_span) = if let Some(&Identifier(name, span)) = self.token() {
            total_span.merge_into(span);

            (Some(name), Some(span))
        } else {
            // At this point we do not want to return WrongType, since we already had a `let` keyword.
            // So this clearly is a variable assignment, if an incorrect one.
            success = false;

            self.errors.push(Error::WithHint(
                "Variable definition misses variable identifier.".into(),
                total_span,
                "Expected variable name here.".into(),
                total_span.after(),
            ));

            maybe_err_span = Some(total_span.after());

            (None, None)
        };

        self.inc();

        // TODO: 4. Type hint?

        // 5. Assignment equal sign
        let mut has_equal_sign = true;
        let mut has_assignment = true;
        if !matches!(self.token(), Some(Equals(_))) {
            has_equal_sign = false;

            maybe_err_span = Some(total_span.after());

            success = false;
        } else {
            total_span.merge_into(self.token().unwrap().span());
        }


        self.inc();

        // 6. Assignment value expression
        let value_expression = match self.parse_expression() {
            Success(t) => t,
            WrongType | EOF(None, None) => {
                has_assignment = false;
                success = false;

                self.junk_expr(Some(total_span.after()))
            },
            SyntaxError(None, err_span) | EOF(None, Some(err_span)) => {
                has_assignment = false;
                success = false;

                maybe_err_span = Some(err_span);

                self.junk_expr(Some(total_span.after()))
            },
            SyntaxError(Some(t), err_span) => {
                success = false;

                maybe_err_span = Some(err_span);

                t
            },
            EOF(Some(t), _) => {
                success = false;

                t
            },
        };

        total_span.merge_into(self.expr_span(value_expression));

        if !has_assignment {
            self.errors.push(Error::WithHint(
                "Variable declaration misses value assignment.".into(),
                total_span,
                format!(
                    "Add a value assignment `let {}{} = value;`.",
                    if mutable_span.is_some() { "mut " } else { "" },
                    if var_name.is_some() { var_name.unwrap() } else { "foo" },
                ),
                total_span.after(),
            ));
        } else if !has_equal_sign {
            self.errors.push(Error::WithHint(
                "Value assignment requires an equal sign.".into(),
                total_span,
                format!(
                    "Value assignments should look as follows `let {}{} = value;`.",
                    if mutable_span.is_some() { "mut " } else { "" },
                    if var_name.is_some() { var_name.unwrap() } else { "foo" },
                ),
                total_span.after(),
            ));
        }

        if success {
            Success(ParsedVariableDeclaration {
                name: var_name.unwrap(),
                name_span: var_name_span.unwrap(),
                type_name: None,
                type_span: None,
                mutability,
                mutability_span: mutable_span,
                initial_value: value_expression,
                span: total_span,
            })
        } else {

            // See if we can create a dummy ParsedVariableDeclaration
            let dummy_value = if var_name.is_some() {
                Some(ParsedVariableDeclaration {
                    name: var_name.unwrap(),
                    name_span: var_name_span.unwrap_or(total_span),
                    type_name: None,
                    type_span: None,
                    mutability,
                    mutability_span: mutable_span,
                    initial_value: value_expression,
                    span: total_span,
                })
            } else {
                None
            };

            if self.eof() {
                EOF(dummy_value, maybe_err_span)
            } else {
                SyntaxError(dummy_value, maybe_err_span.unwrap_or(total_span))
            }
        }
    }

    fn parse_expression(&mut self) -> ParsingResult<ExpressionId> {
        let mut span = self.span();

        if !self.has_token() {
            return EOF(None, None);
        }

        let len_atoms_initial = self.expression_atoms.len();

        let mut success = true;


        // First expression atom
        let lhs_atom = match self.parse_expression_atom() {
            Success(t) => t,
            WrongType => return WrongType,
            SyntaxError(Some(t), span) => {
                success = false;

                t
            },
            SyntaxError(None, span) => {
                success = false;

                self.junk_atom(Some(span))
            }
            EOF(Some(t), maybe_err_span) => {
                self.expressions.push(ParsedExpression::Atom(t, span));

                return EOF(Some(self.current_expr()), maybe_err_span);
            },
            EOF(None, maybe_err_span) => return EOF(Some(self.junk_expr(maybe_err_span)), maybe_err_span),
        };

        span = span.merge(self.atom_span(lhs_atom));

        // Only create tree once we know we have at least one expression atom
        let mut tree: BinaryTree<ExpressionTreeData, _> = BinaryTree::new(|lhs, rhs| {
            use ExpressionTreeData::*;
            match (lhs, rhs) {
                (Atom(_), _) | (_, Atom(_)) => Ordering::Equal,
                (BinOp(lhs), BinOp(rhs)) => lhs.cmp(&rhs),
            }
        });

        let lhs_data = ExpressionTreeData::Atom(lhs_atom);
        _ = tree.insert_root(lhs_data, Side::Left, None);

        loop {
            // Binary operator

            let op = match self.parse_binary_operator() {
                // Successful cases
                Success(t) => t,
                WrongType | EOF(None, _) => break,

                // Unsuccessful cases
                SyntaxError(Some(t), err_span) => {
                    success = false;

                    t
                },
                SyntaxError(None, err_span) => {
                    success = false;

                    ParsedOperator::Junk(err_span)
                },
                EOF(Some(t), maybe_err_span) => {
                    // Insert junk rhs
                    // Add tree data
                    let op_data = ExpressionTreeData::BinOp(t);
                    let rhs_atom_data = ExpressionTreeData::Atom(self.junk_atom(Some(span.after())));

                    _ = tree.insert_root(
                        op_data,
                        Side::Left,
                        Some(rhs_atom_data)
                    );

                    break;
                },
            };

            span.merge_into(op.span());

            // RHS atom
            let rhs_atom = match self.parse_expression_atom() {
                Success(t) => t,
                WrongType => {
                    self.errors.push(Error::WithHint(
                        "Incomplete expression.".into(),
                        span,
                        "Binary operator misses expression to act upon.".into(),
                        op.span(),
                    ));

                    success = false;

                    self.junk_atom(Some(span.after()))
                },
                SyntaxError(Some(t), _) | EOF(Some(t), _) => {
                    success = false;

                    t
                },
                SyntaxError(None, err_span) => {
                    success = false;

                    self.junk_atom(Some(err_span))
                },
                EOF(None, maybe_err_span) => {
                    success = false;

                    self.junk_atom(None)
                },
            };

            // Add data to tree

            let op_data = ExpressionTreeData::BinOp(op);
            let rhs_atom_data = ExpressionTreeData::Atom(rhs_atom);

            _ = tree.insert_root(
                op_data,
                Side::Left,
                Some(rhs_atom_data)
            );
        }

        fn print_tree<'a, F>(
            parser: &Parser<'a>,
            tree: &BinaryTree<ExpressionTreeData, F>,
            start_index: usize
        ) -> String
            where F: Fn(ExpressionTreeData, ExpressionTreeData) -> Ordering
        {
            use ExpressionTreeData::*;
            let node = tree.get_node(start_index);

            if let &Atom(id) = node.data() {
                let atom = parser.get_atom(id).clone();

                return atom.to_string(parser);
            }

            assert!(matches!(node.data(), &BinOp(_)));

            let op = if let &BinOp(op) = node.data() {
                op
            } else {
                panic!("unreachable code (Parser::parse_expression()::build_expression_tree()): node.data() not BinOp");
            };

            let lhs = tree.get_node(start_index).lhs().expect("Binary operator node does not have an lhs!");
            let rhs = tree.get_node(start_index).rhs().expect("Binary operator node does not have an rhs!");

            let lhs = print_tree(parser, tree, lhs);
            let rhs = print_tree(parser, tree, rhs);

            format!("({} {} {})", lhs, op.to_string(), rhs)
        }

        fn build_expression_tree<'a, F>(
            parser: &mut Parser<'a>,
            tree: &BinaryTree<ExpressionTreeData, F>,
            start_index: usize
        ) -> ExpressionId
            where F: Fn(ExpressionTreeData, ExpressionTreeData) -> Ordering
        {
            use ExpressionTreeData::*;
            let node = tree.get_node(start_index);

            if let &Atom(id) = node.data() {
                let span = parser.get_atom(id).span();

                parser.expressions.push(ParsedExpression::Atom(id, span));

                return parser.current_expr();
            }

            assert!(matches!(node.data(), &BinOp(_)));

            let op = if let &BinOp(op) = node.data() {
                op
            } else {
                panic!("unreachable code (Parser::parse_expression()::build_expression_tree()): node.data() not BinOp");
            };

            let lhs = tree.get_node(start_index).lhs().expect("Binary operator node does not have an lhs!");
            let rhs = tree.get_node(start_index).rhs().expect("Binary operator node does not have an rhs!");

            let lhs = build_expression_tree(parser, tree, lhs);
            let rhs = build_expression_tree(parser, tree, rhs);

            let lhs_span = parser.expr_span(lhs);
            let rhs_span = parser.expr_span(rhs);

            let span = lhs_span.merge(rhs_span);

            parser.expressions.push(ParsedExpression::BinaryOperation(
                lhs,
                op,
                rhs,
                span,
            ));

            parser.current_expr()
        }

        Success(build_expression_tree(self, &tree, tree.root()))
    }



    fn parse_expression_atom(&mut self) -> ParsingResult<ExpressionAtomId> {
        if !self.has_token() {
            return EOF(None, None);
        }

        use Token::*;
        use ParsedExpressionAtom::*;
        let tk = *self.token().unwrap();
        match tk {
            NumberLiteral(value, span) => {
                self.inc();

                self.expression_atoms.push(IntegerConstant(value, span));

                Success(self.current_expr_atom())
            },
            BooleanLiteral(value, span) => {
                self.inc();

                self.expression_atoms.push(BooleanConstant(value, span));

                Success(self.current_expr_atom())
            },
            Identifier(name, span) => {

                // Check if we actually have a function invocation
                match self.parse_function_invocation() {
                    Success(t) => return Success(t),
                    WrongType => {},
                    SyntaxError(maybe_t, span) => return SyntaxError(maybe_t, span),
                    EOF(maybe_t, maybe_span) => return EOF(maybe_t, maybe_span),
                }

                self.inc();

                // If it's not a function, it's a variable
                self.expression_atoms.push(Variable(name, span));

                Success(self.current_expr_atom())
            },
            LParen(mut span) => {
                if !self.inc() {
                    self.errors.push(Error::WithHint(
                        "Expression misses closing parenthesis.".into(),
                        span,
                        "This open parenthesis misses a closing partner.".into(),
                        span,
                    ));

                    self.expression_atoms.push(ParsedExpressionAtom::Unit(span));

                    return EOF(Some(self.current_expr_atom()), Some(span));
                }

                let lparen_span = span;

                let expr = self.parse_expression();

                let expr_span = match expr {
                    Success(t) => self.expr_span(t),
                    WrongType => span,
                    SyntaxError(_, sp) => sp,
                    EOF(_, Some(sp)) => sp,
                    _ => self.span(),
                };

                span.merge_into(expr_span);

                if !matches!(self.token(), Some(RParen(_))) {
                    self.errors.push(Error::WithHint(
                        "Expression misses closing parenthesis.".into(),
                        span,
                        "This open parenthesis misses a closing partner.".into(),
                        lparen_span,
                    ));

                    let atom = match expr.to_opt() {
                        Some(t) => ParsedExpressionAtom::EnclosedExpression(t, span),
                        None => ParsedExpressionAtom::Junk(span),
                    };

                    self.expression_atoms.push(atom);

                    let atom = self.current_expr_atom();

                    return match expr {
                        EOF(_, _) => EOF(Some(atom), Some(span)),
                        _ => SyntaxError(Some(atom), span),
                    };
                }

                span = span.merge(self.span());

                self.inc();

                match expr {
                    Success(t) => {
                        self.expression_atoms.push(EnclosedExpression(t, span));

                        Success(self.current_expr_atom())
                    },
                    WrongType => {
                        self.expression_atoms.push(Unit(span));

                        Success(self.current_expr_atom())
                    },
                    SyntaxError(Some(t), err_span) => {
                        self.expression_atoms.push(EnclosedExpression(t, span));

                        SyntaxError(Some(self.current_expr_atom()), err_span)
                    },
                    SyntaxError(None, err_span) => {
                        self.expression_atoms.push(Unit(span));

                        SyntaxError(Some(self.current_expr_atom()), err_span)
                    },
                    EOF(Some(t), maybe_err_span) => {
                        self.expression_atoms.push(EnclosedExpression(t, span));

                        EOF(Some(self.current_expr_atom()), maybe_err_span)
                    },
                    EOF(None, maybe_err_span) => {
                        self.expression_atoms.push(Unit(span));

                        EOF(Some(self.current_expr_atom()), maybe_err_span)
                    },
                }
            },
            // TODO: Precedence considering prefix and suffix operators
            _ if let Success(op) = self.parse_unary_prefix_operator() => {
                match self.parse_expression_atom() {
                    Success(t) => {
                        self.expression_atoms.push(ParsedExpressionAtom::UnaryPrefixedAtom(
                            op,
                            t,
                            op.span().merge(self.atom_span(t))
                        ));

                        Success(self.current_expr_atom())
                    },
                    WrongType => {
                        let junk = self.junk_atom(Some(op.span().after()));
                        self.expression_atoms.push(ParsedExpressionAtom::UnaryPrefixedAtom(
                            op,
                            junk,
                            op.span()
                        ));

                        SyntaxError(Some(self.current_expr_atom()), op.span().after())
                    },
                    SyntaxError(Some(t), err_span) => {
                        self.expression_atoms.push(ParsedExpressionAtom::UnaryPrefixedAtom(
                            op,
                            t,
                            op.span().merge(self.atom_span(t))
                        ));

                        SyntaxError(Some(self.current_expr_atom()), err_span)
                    },
                    SyntaxError(None, err_span) => {
                        let junk = self.junk_atom(Some(op.span().after()));
                        self.expression_atoms.push(ParsedExpressionAtom::UnaryPrefixedAtom(
                            op,
                            junk,
                            op.span()
                        ));

                        SyntaxError(Some(self.current_expr_atom()), err_span)
                    },
                    EOF(Some(t), maybe_err_span) => {
                        self.expression_atoms.push(ParsedExpressionAtom::UnaryPrefixedAtom(
                            op,
                            t,
                            op.span().merge(self.atom_span(t))
                        ));

                        EOF(Some(self.current_expr_atom()), maybe_err_span)
                    },
                    EOF(None, maybe_err_span) => {
                        let junk = self.junk_atom(Some(op.span().after()));
                        self.expression_atoms.push(ParsedExpressionAtom::UnaryPrefixedAtom(
                            op,
                            junk,
                            op.span()
                        ));

                        EOF(Some(self.current_expr_atom()), maybe_err_span)
                    },
                }
            },
            _ => WrongType,
        }
    }

    fn parse_function_invocation(&mut self) -> ParsingResult<ExpressionAtomId> {
        if !self.has_token() {
            return EOF(None, None);
        }

        let initial_index = self.token_index;

        use Token::*;

        // 1. Identifier
        let (name, name_span) = if let Some(&Identifier(name, span)) = self.token() {
            (name, span)
        } else {
            return WrongType;
        };


        let mut span = name_span;
        let mut success = true;
        let mut error_span = name_span;

        self.inc();

        // 2. Open parenthesis
        if !matches!(self.token(), Some(LParen(_))) {
            self.token_index = initial_index;
            return WrongType;
        }

        span.merge_into(self.token().unwrap().span());

        self.inc();



        // TODO: 3. Parameters?

        // 4. Closing parenthesis
        if !matches!(self.token(), Some(RParen(_))) {
            self.token_index = initial_index;

            self.errors.push(Error::WithHint(
                "Function invocation misses closing parenthesis.".into(),
                span,
                "Expected `)` here.".into(),
                span.after(),
            ));

            error_span = span.after();

            success = false;
        } else {
            span.merge_into(self.token().unwrap().span());
            self.inc();
        }

        self.expression_atoms.push(ParsedExpressionAtom::FunctionInvocation(
            name,
            (),
            name_span.merge(span),
        ));

        if success {
            Success(self.current_expr_atom())
        } else {
            SyntaxError(Some(self.current_expr_atom()), error_span)
        }
    }

    fn parse_unary_prefix_operator(&mut self) -> ParsingResult<ParsedOperator> {
        if !self.has_token() {
            return EOF(None, None);
        }

        use Token::*;
        use ParsedOperator::*;

        let op = match self.token().unwrap() {
            &Plus(span) => Success(UnaryPlus(span)),
            &Minus(span) => Success(UnaryMinus(span)),
            &Asterisk(span) => Success(UnaryDereference(span)),
            &Not(span) => Success(UnaryLogicalNot(span)),
            &Ampersand(span) => Success(UnaryReference(span)),
            _ => WrongType,
        };

        if op.is_some() {
            self.inc();
        }

        op
    }

    fn parse_binary_operator(&mut self) -> ParsingResult<ParsedOperator> {
        if !self.has_token() {
            return EOF(None, None);
        }

        use Token::*;
        use ParsedOperator::*;

        let op = match *self.token().unwrap() {
            Plus(span) => Success(BinaryAddition(span)),
            Minus(span) => Success(BinaryMultiplication(span)),
            Asterisk(span) => Success(BinaryMultiplication(span)),
            Slash(span) => Success(BinaryDivision(span)),
            Percent(span) => Success(BinaryModulo(span)),
            And(span) => Success(BinaryLogicalAnd(span)),
            Or(span) => Success(BinaryLogicalOr(span)),
            Xor(span) => Success(BinaryLogicalXor(span)),
            _ => WrongType,
        };

        if op.is_some() {
            self.inc();
        }

        op
    }
}

//--------------------------------------------------
// Helpers
//--------------------------------------------------

#[derive(Debug, Clone, Copy)]
enum ExpressionTreeData {
    Atom(ExpressionAtomId),
    BinOp(ParsedOperator),
}

#[derive(Clone, Debug)]
enum ParsingResult<T: Clone + Debug> {
    Success(T),
    WrongType,

    /// Indicates that the function found the requested object, but encountered a syntactical error.
    ///
    /// A function returning `SyntaxError` guarantees that it has already raised an error.
    ///
    /// It will contain Junk elements wherever necessary.
    SyntaxError(Option<T>, Span),

    /// Indicated that the function found the requested object, but encountered the end of the source before it could finish parsing.
    ///
    /// A function returning `SyntaxError` guarantees that it has already raised an error.
    ///
    /// It will contain Junk elements wherever necessary.
    EOF(Option<T>, Option<Span>),
}

impl<T: Clone + Debug> ParsingResult<T> {
    fn is_some(&self) -> bool {
        matches!(self, Success(_))
    }

    fn is_none(&self) -> bool {
        !self.is_some()
    }

    fn unwrap(self) -> T {
        match self {
            Success(t) => t,
            SyntaxError(maybe_t, _) => maybe_t.unwrap(),
            _ => panic!("Unwrapped ParsingResult that did not contain a value!"),
        }
    }

    fn span(&self) -> Option<Span> {
        match self {
            &SyntaxError(_, span) => Some(span),
            &EOF(_, maybe_span) => maybe_span,
            _ => None,
        }
    }

    fn is_eof(&self) -> bool {
        matches!(self, EOF(_, _))
    }

    fn to_opt(self) -> Option<T> {
        self.into()
    }
}

impl<T: Clone + Debug> Into<Option<T>> for ParsingResult<T> {
    fn into(self) -> Option<T> {
        match self {
            Success(t) => Some(t),
            SyntaxError(maybe_t, _) => maybe_t,
            EOF(maybe_t, _) => maybe_t,
            _ => None,
        }
    }
}

impl<T: Clone + Copy + Debug> Copy for ParsingResult<T> {}