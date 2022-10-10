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

#[derive(Debug, Clone)]
pub enum ParsedStatement<'src> {
    NoOp(Span),
    Expression(ExpressionId, HasSemicolon),
    VariableDefinition(ParsedVariableDefinition<'src>),
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
pub struct ParsedVariableDefinition<'src> {
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
    Unit(Span),
    IntegerConstant(u128, Span),
    BooleanConstant(bool, Span),
    EnclosedExpression(ExpressionId, Span),
    UnaryPrefixedAtom(ParsedOperator, ExpressionAtomId, Span),
    Variable(&'src str, Span),
}

impl<'src> ParsedExpressionAtom<'src> {
    pub fn span(&self) -> Span {
        use ParsedExpressionAtom::*;
        match self {
            Unit(span) |
            IntegerConstant(_, span) |
            BooleanConstant(_, span) |
            EnclosedExpression(_, span) |
            UnaryPrefixedAtom(_, _, span) |
            Variable(_, span) => *span,
        }
    }

    fn to_string<'a>(&self, parser: &Parser<'a>) -> String {
        use ParsedExpressionAtom::*;
        match self {
            Unit(_) => "()".into(),
            IntegerConstant(val, _) => format!("{}", val),
            BooleanConstant(val, _) => format!("{}", val),
            EnclosedExpression(expr, _) => format!("({})", parser.get_expr(*expr).to_string(parser)),
            UnaryPrefixedAtom(op, atom, _) => format!("({}{})", op.to_string(), parser.get_atom(*atom).to_string(parser)),
            &Variable(identifier, _) => String::from(identifier),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParsedOperator {

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
        match self {
            ParsedOperator::UnaryPlus(_) => "+".into(),
            ParsedOperator::UnaryMinus(_) => "-".into(),
            ParsedOperator::BinaryAddition(_) => "+".into(),
            ParsedOperator::BinarySubtraction(_) => "-".into(),
            ParsedOperator::BinaryMultiplication(_) => "*".into(),
            ParsedOperator::BinaryDivision(_) => "/".into(),
            ParsedOperator::BinaryModulo(_) => "%".into(),
            ParsedOperator::UnaryLogicalNot(_) => "not ".into(),
            ParsedOperator::BinaryLogicalAnd(_) => "and".into(),
            ParsedOperator::BinaryLogicalOr(_) => "or".into(),
            ParsedOperator::BinaryLogicalXor(_) => "xor".into(),
            ParsedOperator::UnaryReference(_) => "&".into(),
            ParsedOperator::UnaryDereference(_) => "*".into(),
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

        let mut success = true;

        while self.has_token() && success {


            // try parse a function
            if let Some(func) = self.parse_function() {
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
                span: fun_span,
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
        if matches!(self.token(), Some(LBrace(_))) {
            if !self.expect_inc() {
                return None;
            }
        } else {
            return None;
        }

        // TODO: 2. Code ig
        let mut found_statement = true;
        let mut statements = vec![];
        while found_statement && !matches!(self.token(), Some(RBrace(_))) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            } else {
                found_statement = false;
            }
        }

        // 3. Closing brace
        if matches!(self.token(), Some(RBrace(_))) {
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

        let mut needs_semicolon = true;

        // 0. Check if there is a lonely semicolon
        if matches!(self.token(), Some(Semicolon(_))) {
            maybe_span = Some(self.token().unwrap().span());
            maybe_stmt = Some(ParsedStatement::NoOp(self.token().unwrap().span()));

            // FIXME: Emit warning about this semicolon

            self.inc();

            needs_semicolon = false;
        }

        // 1. Check if there is a variable definition
        else if let Some(defn) = self.parse_variable_definition() {
            maybe_span = Some(defn.span);
            maybe_stmt = Some(ParsedStatement::VariableDefinition(defn));

            needs_semicolon = true;
        }

        // 2. Check if there is an if statement
        else if let Some(if_stmt) = self.parse_if_statement() {
            maybe_span = Some(if_stmt.span);
            maybe_stmt = Some(ParsedStatement::IfStatement(if_stmt));

            needs_semicolon = false;
        }

        // 3. Check if there is a code block
        else if let Some(block) = self.parse_code_block() {
            maybe_span = Some(block.span);
            maybe_stmt = Some(ParsedStatement::Block(block));

            needs_semicolon = false;
        }

        // TODO: Check for other types of statements

        // We now still expect a semicolon
        if maybe_span.is_none() {
            maybe_span = Some(start_span);
        }

        if maybe_stmt.is_none() {
            return None;
        }

        if needs_semicolon {
            if !matches!(self.token(), Some(Semicolon(_))) {
                self.errors.push(Error::WithHint(
                    "Statement misses semicolon".into(),
                    maybe_span.unwrap(),
                    "Add a semicolon at the end of the statement.".into(),
                    maybe_span.unwrap().after(),
                ));

                return None;
            } else {
                self.inc();
            }
        }

        maybe_stmt
    }

    fn parse_if_statement(&mut self) -> Option<ParsedIfStatement<'src>> {
        if !self.has_token() {
            return None;
        }

        use Token::*;

        // 1. `if` keyword
        if !matches!(self.token().unwrap(), If(_)) {
            return None;
        }

        let mut total_span = self.span();

        if !self.expect_inc() {
            return None;
        }

        // 2. Condition expression
        let cond = self.parse_expression();

        if cond.is_none() {
            self.errors.push(Error::WithHint(
                "If statement lacks condition expression.".into(),
                total_span,
                "Expected boolean expression here.".into(),
                total_span.after(),
            ));

            return None;
        }

        let cond = cond.unwrap();

        total_span.merge_into(self.expr_span(cond));

        // 3. Code block
        let block = self.parse_code_block();

        if block.is_none() {
            self.errors.push(Error::WithHint(
                "If statement lacks then block.".into(),
                total_span,
                "If statements need to be of the form `if condition { ... }`".into(),
                total_span.after(),
            ));

            return None;
        }

        let block = block.unwrap();

        total_span.merge_into(block.span);

        // 4. Else block?
        let else_stmt = if matches!(self.token(), Some(Else(_))) {
            let else_span = self.token().unwrap().span();
            self.inc();

            let if_stmt = self.parse_if_statement();

            if if_stmt.is_some() {
                Some(ParsedElseStatement::Conditional(Box::new(if_stmt.unwrap()), else_span))
            } else {
                let code_block = self.parse_code_block();

                if code_block.is_none() {
                    self.errors.push(Error::WithHint(
                        "Else statement lacks then block.".into(),
                        else_span,
                        "Expected code block or another if statement here.".into(),
                        else_span.after(),
                    ));

                    return None;
                }

                let code_block = code_block.unwrap();

                Some(ParsedElseStatement::Unconditional(code_block, else_span))
            }
        } else {
            None
        };

        Some(ParsedIfStatement {
            condition: cond,
            then_block: block,
            else_statment: else_stmt,
            span: total_span,
        })
    }

    fn parse_variable_definition(&mut self) -> Option<ParsedVariableDefinition<'src>> {
        if !self.has_token() {
            return None;
        }

        use Token::*;

        // 1. `let` keyword
        if !matches!(self.token(), Some(Let(_))) {
            return None;
        }

        let mut success = true;
        let mut total_span = self.token().unwrap().span();

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

    fn parse_expression(&mut self) -> Option<ExpressionId> {
        let mut span = self.span();

        if !self.has_token() {
            return None;
        }

        let len_atoms_initial = self.expression_atoms.len();

        let mut tree: BinaryTree<ExpressionTreeData, _> = BinaryTree::new(|lhs, rhs| {
            use ExpressionTreeData::*;
            match (lhs, rhs) {
                (Atom(_), _) | (_, Atom(_)) => Ordering::Equal,
                (ExpressionTreeData::BinOp(lhs), ExpressionTreeData::BinOp(rhs)) => lhs.cmp(&rhs),
            }
        });


        // Next expression atom
        let lhs_atom = if let Some(atom) = self.parse_expression_atom() {
            atom
        } else {
            return None;
        };

        span = span.merge(self.atom_span(lhs_atom));

        let lhs_data = ExpressionTreeData::Atom(lhs_atom);
        _ = tree.insert_root(lhs_data, Side::Left, None);

        loop {
            // Binary operation
            let op = self.parse_binary_operator();

            if op.is_none() {
                break;
            }

            let op = op.unwrap();

            // RHS atom
            let rhs_atom = self.parse_expression_atom();

            if rhs_atom.is_none() {
                self.errors.push(Error::WithHint(
                    "Incomplete expression.".into(),
                    span,
                    "Binary operator lacks expression to act upon.".into(),
                    op.span(),
                ));

                // drop the newly added atoms
                self.expression_atoms.truncate(len_atoms_initial);

                return None;
            }

            let rhs_atom = rhs_atom.unwrap();

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

        Some(build_expression_tree(self, &tree, tree.root()))
    }



    fn parse_expression_atom(&mut self) -> Option<ExpressionAtomId> {
        if !self.has_token() {
            return None;
        }

        use Token::*;
        use ParsedExpressionAtom::*;
        let tk = *self.token().unwrap();
        match tk {
            NumberLiteral(value, span) => {
                self.inc();

                self.expression_atoms.push(IntegerConstant(value, span));

                Some(self.current_expr_atom())
            },
            BooleanLiteral(value, span) => {
                self.inc();

                self.expression_atoms.push(BooleanConstant(value, span));

                Some(self.current_expr_atom())
            },
            Identifier(name, span) => {
                self.inc();

                // TODO: Check if followed by `()`, because then it should be a function invocation.

                self.expression_atoms.push(Variable(name, span));

                Some(self.current_expr_atom())
            },
            LParen(mut span) => {
                if !self.expect_inc() {
                    return None;
                }

                let expr = self.parse_expression();

                if !matches!(self.token(), Some(RParen(_))) {
                    return None;
                }

                span = span.merge(self.span());

                self.inc();

                if expr.is_some() {
                    self.expression_atoms.push(EnclosedExpression(expr.unwrap(), span));
                } else {
                    self.expression_atoms.push(Unit(span));
                }

                Some(self.current_expr_atom())
            },
            // TODO: Precedence considering prefix and suffix operators
            _ if let Some(op) = self.parse_unary_prefix_operator() => {
                let atom = self.parse_expression_atom();

                if atom.is_none() {

                    self.errors.push(Error::WithHint(
                        "Incomplete expression.".into(),
                        op.span(),
                        "Unary operator lacks expression atom to act upon.".into(),
                        op.span(),
                    ));

                    return None;
                }

                let atom = atom.unwrap();

                let span = op.span().merge(self.atom_span(atom));

                self.expression_atoms.push(UnaryPrefixedAtom(op, atom, span));

                Some(self.current_expr_atom())
            },
            _ => None,
        }
    }

    fn parse_unary_prefix_operator(&mut self) -> Option<ParsedOperator> {
        if !self.has_token() {
            return None;
        }

        use Token::*;
        use ParsedOperator::*;

        let op = match self.token().unwrap() {
            &Plus(span) => Some(UnaryPlus(span)),
            &Minus(span) => Some(UnaryMinus(span)),
            &Asterisk(span) => Some(UnaryDereference(span)),
            &Not(span) => Some(UnaryLogicalNot(span)),
            &Ampersand(span) => Some(UnaryReference(span)),
            _ => None,
        };

        if op.is_some() {
            self.inc();
        }

        op
    }

    fn parse_binary_operator(&mut self) -> Option<ParsedOperator> {
        if !self.has_token() {
            return None;
        }

        use Token::*;
        use ParsedOperator::*;

        let op = match *self.token().unwrap() {
            Plus(span) => Some(BinaryAddition(span)),
            Minus(span) => Some(BinaryMultiplication(span)),
            Asterisk(span) => Some(BinaryMultiplication(span)),
            Slash(span) => Some(BinaryDivision(span)),
            Percent(span) => Some(BinaryModulo(span)),
            And(span) => Some(BinaryLogicalAnd(span)),
            Or(span) => Some(BinaryLogicalOr(span)),
            Xor(span) => Some(BinaryLogicalXor(span)),
            _ => None,
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