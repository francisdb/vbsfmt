// In parser/expressions.rs

use crate::lexer::{Token, TokenKind};
use crate::parser::ast::Lit;
use crate::parser::{ast, Parser};
use crate::T;

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn expression(&mut self) -> ast::Expr {
        self.parse_expression(0)
    }

    pub fn parse_expression(&mut self, binding_power: u8) -> ast::Expr {
        let mut lhs = match self.peek() {
            lit @ T![integer_literal]
            | lit @ T![real_literal]
            | lit @ T![string_literal]
            | lit @ T![true]
            | lit @ T![false] => {
                let literal_text = {
                    // the calls on `self` need to be split, because `next` takes
                    // `&mut self` if `peek` is not `T![EOF]`, then there must be
                    // a next token
                    let literal_token = self.next().unwrap();
                    self.text(literal_token)
                };
                // We are using parse here which is for parsing rust literals, we might have to
                // implement our own parser for VBScript literals
                let lit = match lit {
                    T![integer_literal] => {
                        ast::Lit::Int(literal_text.parse().unwrap_or_else(|_| {
                            panic!("invalid integer literal: `{literal_text}`")
                        }))
                    }
                    T![real_literal] => Lit::Float(literal_text.parse().unwrap_or_else(|_| {
                        panic!("invalid floating point literal: `{literal_text}`")
                    })),
                    T![string_literal] => Lit::Str(
                        // trim the quotation marks
                        literal_text[1..(literal_text.len() - 1)].to_string(),
                    ),
                    T![true] => Lit::Bool(true),
                    T![false] => Lit::Bool(false),
                    _ => unreachable!(),
                };
                ast::Expr::Literal(lit)
            }
            T![ident] => {
                let full_ident = self.ident_deep();
                // let name = {
                //     let ident_token = self.next().unwrap();
                //     self.text(ident_token).to_string() // <- now we need a copy
                // };
                // let property_accesses = Vec::new();
                // while self.at(T![property_access]) {
                //     // property access
                //     self.consume(T![.]);
                //     while self.at(T![ident]) {
                //         let property_name = {
                //             let ident_token = self.next().unwrap();
                //             self.text(ident_token).to_string()
                //         };
                //         property_accesses.push(property_name);
                //         if self.at(T![.]) {
                //             self.consume(T![.]);
                //         }
                //     }
                // }
                if !self.at(T!['(']) {
                    // plain identifier or sub call

                    // TODO handle sub call

                    ast::Expr::Ident(full_ident)
                } else {
                    //  function call
                    let mut args = Vec::new();
                    self.consume(T!['(']);
                    while !self.at(T![')']) {
                        let arg = self.parse_expression(0);
                        args.push(arg);
                        if self.at(T![,]) {
                            self.consume(T![,]);
                        }
                    }
                    self.consume(T![')']);
                    ast::Expr::FnCall {
                        fn_name: full_ident,
                        args,
                    }
                }
            }
            T!['('] => {
                // There is no AST node for grouped expressions.
                // Parentheses just influence the tree structure.
                self.consume(T!['(']);
                let expr = self.parse_expression(0);
                self.consume(T![')']);
                expr
            }
            op @ T![+] | op @ T![-] | op @ T![not] => {
                self.consume(op);
                let ((), right_binding_power) = op.prefix_binding_power();
                // NEW!
                let expr = self.parse_expression(right_binding_power);
                ast::Expr::PrefixOp {
                    op,
                    expr: Box::new(expr),
                }
            }
            kind => panic!("Unknown start of expression:: {kind}"),
        };
        loop {
            let op = match self.peek() {
                op @ T![+]
                | op @ T![-]
                | op @ T![*]
                | op @ T![/]
                | op @ T!['\\']
                | op @ T![^]
                | op @ T![=]
                | op @ T![<>]
                | op @ T![and]
                | op @ T![or]
                | op @ T![<]
                | op @ T![<=]
                | op @ T![>]
                | op @ T![>=]
                | op @ T![not] => op,
                T![EOF] => break,
                T![')'] | T![,] => break,
                ending if ending.is_ending_expression() => break,
                kind => panic!("Unknown operator: `{}`", kind),
            };

            // if let Some((left_binding_power, ())) =
            //     op.postfix_binding_power()
            // {
            //     if left_binding_power < binding_power {
            //         // previous operator has higher binding power than
            //         // new one --> end of expression
            //         break;
            //     }
            //
            //     self.consume(op);
            //     // no recursive call here, because we have already
            //     // parsed our operand `lhs`
            //     lhs = ast::Expr::PostfixOp {
            //         op,
            //         expr: Box::new(lhs),
            //     };
            //     // parsed an operator --> go round the loop again
            //     continue;
            // }

            if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                // <- NEW!

                if left_binding_power < binding_power {
                    // previous operator has higher binding power than
                    // new one --> end of expression
                    break;
                }

                self.consume(op);
                let rhs = self.parse_expression(right_binding_power);
                lhs = ast::Expr::InfixOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                // parsed an operator --> go round the loop again
                continue;
            }
            break; // Not an operator --> end of expression
        }

        lhs
    }
}

trait Operator {
    /// Prefix operators bind their operand to the right.
    fn prefix_binding_power(&self) -> ((), u8);

    /// Infix operators bind two operands, lhs and rhs.
    fn infix_binding_power(&self) -> Option<(u8, u8)>;

    // /// Postfix operators bind their operand to the left.
    // fn postfix_binding_power(&self) -> Option<(u8, ())>;
}

impl Operator for TokenKind {
    fn prefix_binding_power(&self) -> ((), u8) {
        match self {
            T![+] | T![-] | T![not] => ((), 51),
            // Prefixes are the only operators we have already seen
            // when we call this, so we know the token must be
            // one of the above
            _ => unreachable!("Not a prefix operator: {:?}", self),
        }
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        let result = match self {
            T![or] => (1, 2),
            T![and] => (3, 4),
            T![=] | T![<>] => (5, 6),
            T![<] | T![>] | T![<=] | T![>=] => (7, 8),
            T![+] | T![-] => (9, 10),
            T![*] | T![/] | T!['\\'] => (11, 12),
            T![^] => (22, 21), // <- This binds stronger to the left!
            _ => return None,
        };
        Some(result)
    }

    // fn postfix_binding_power(&self) -> Option<(u8, ())> {
    //     let result = match self {
    //         T![!] => (101, ()),
    //         _ => return None,
    //     };
    //     Some(result)
    // }
}
