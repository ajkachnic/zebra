use miette::Diagnostic;
use thiserror::Error;

use super::typed_ir;

use crate::frontend::lex::Span;
use crate::frontend::parse::ast::{self, Spanned, Import};
use crate::types::{self, TypedVariable};

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum TypeError {
    #[error("Uninitialized variable must be declared as nullable")]
    #[diagnostic(code(backend::check::uninitialized_must_be_nullable))]
    UninitializedMustBeNullable {
        #[source_code]
        src: String,

        #[label("Here the variable is left uninitialized")]
        variable: Span,

        #[label("But, it's type does is not nullable")]
        ty: Span,
    },

    #[error("The declared type of this variable doesn't match with the actual type of it")]
    #[diagnostic(code(backend::check::declared_doesnt_match_actual))]
    DeclaredDoesntMatchActual {
        #[source_code]
        src: String,

        #[label("This expression is a different type than the variable it's being assigned to")]
        expr: Span,
    },

    #[error("Variable must either be initialized or have an explicit type")]
    UntypedUninitializedVariable {
        #[source_code]
        src: String,

        #[label("This variable is uninitialized, but it's type is not declared")]
        variable: Span,
    },

    #[error("Type mismatch in binary expression")]
    BinaryMismatch {
        #[source_code]
        src: String,

        lhs: Span,
        rhs: Span,
    },
}
pub struct Checker {
    pub errors: Vec<TypeError>,
    pub src: String,

    pub modules: Vec<typed_ir::Module>,
    pub current_module: usize,

    pub resolve_chain: Vec<String>,
}

impl Checker {
    fn find_module(&self, name: &str) -> Option<usize> {
        // TODO: Maybe use a bloom filter to check what modules exist already
        self.modules.iter().enumerate().find(|(_, m)| m.name == name).map(|(i, _)| i)
    }

    fn resolve_module(&mut self, import: Import) -> Result<usize, TypeError> {
        if self.modules[self.current_module].name == import.path {
            return Ok(self.current_module)
        }

        if let Some(module) = self.find_module(&import.path) {
            return Ok(module)
        } 

        Ok(0)
    }

    /// Iterate over the `ast::Module` and create the symbol table,
    /// needed for further type checking
    fn generate_symbol_table(&mut self, module: usize, parsed_module: ast::Module) -> Result<(), TypeError> {
        let module = &mut self.modules[module - 1];

        for item in &parsed_module.items {
            match item {
                ast::Item::Function(func) => {
                    module.symbols.insert(
                        func.proto.name.clone(),
                        typed_ir::SymbolType::FunctionProto(func.proto.clone()),
                    );
                }
                _ => todo!(),
            }
        }
        Ok(())
    }

    pub fn check(&mut self, parsed_module: ast::Module, src: String) -> Result<(), TypeError> {
        let module = typed_ir::Module {
            name: parsed_module.name.clone(),
            items: Vec::new(),
            symbols: Default::default(),
        };

        self.src = src;
        self.modules[self.current_module] = module;
        self.current_module += 1;

        // First-pass, fill the module symbol table
        self.generate_symbol_table(self.current_module - 1, parsed_module)?;

        Ok(())
    }

    fn check_function(&mut self, func: &ast::Function) -> Result<typed_ir::Function, TypeError> {
        // let header = self.check_function_header(func)?;

        let body = self.check_function_body(&func)?;
        Ok(typed_ir::Function {
            proto: func.proto.clone(),
            body,
        })
    }

    fn check_function_body(
        &mut self,
        func: &ast::Function,
    ) -> Result<Vec<typed_ir::Statement>, TypeError> {
        let mut body = Vec::new();
        for stmt in &func.body {
            body.push(self.check_statement(&stmt)?);
        }
        Ok(body)
    }

    fn check_statement(&mut self, stmt: &ast::Statement) -> Result<typed_ir::Statement, TypeError> {
        match &stmt.inner {
            ast::StatementKind::Expression(expr) => Ok(typed_ir::Statement::Expression(
                self.check_expression(&Spanned::new(stmt.span, expr.clone()))?,
            )),
            ast::StatementKind::VariableDeclaration { name, ty, expr } => {
                Ok(self.check_variable_declaration(name, ty, expr)?)
            }
            ast::StatementKind::Return(expr) => Ok(typed_ir::Statement::Return(match expr {
                Some(expr) => Some(self.check_expression(expr)?),
                None => None,
            })),
            _ => todo!(),
        }
    }

    fn check_variable_declaration(
        &mut self,
        name: &ast::Spanned<String>,
        ty: &Option<ast::Spanned<types::Type>>,
        expr: &Option<ast::Expression>,
    ) -> Result<typed_ir::Statement, TypeError> {
        if let Some(expr) = &expr {
            let expr = self.check_expression(expr)?;

            if let Some(ty) = ty && ty.inner != expr.ty {
            return Err(TypeError::DeclaredDoesntMatchActual { src: self.src.clone(), expr: expr.span })
          } else {
            Ok(typed_ir::Statement::VariableDeclaration { name: TypedVariable::new(name.inner.clone(), expr.ty.clone()), expr: Some(expr.clone()) })
          }
        } else {
            if matches!(ty.clone().map(|t| t.inner), Some(types::Type::Nullable(_))) {
                Ok(typed_ir::Statement::VariableDeclaration {
                    name: TypedVariable::new(name.inner.clone(), ty.clone().unwrap().inner),
                    expr: None,
                })
            } else if let Some(ty) = ty {
                // If the declared type is not nullable but it exists
                return Err(TypeError::UninitializedMustBeNullable {
                    src: self.src.clone(),
                    variable: name.span,
                    ty: ty.span,
                });
            } else {
                // If the declared type doesn't exist and there's no initialized value
                return Err(TypeError::UntypedUninitializedVariable {
                    src: self.src.clone(),
                    variable: name.span,
                });
            }
        }
        // let expr = match expr {
        //     Some(expr) => Some(self.check_expression(expr)?),
        //     None => None,
        // };
        // Ok(typed_ir::Statement::VariableDeclaration {
        //     name: TypedVariable::new(var.name, var.ty),
        //     expr,
        // })
    }

    fn check_expression(
        &mut self,
        expr: &ast::Expression,
    ) -> Result<typed_ir::Expression, TypeError> {
        let (kind, _ty) = match &expr.inner {
            ast::ExpressionKind::Binary { lhs, rhs, op } => {
                let lhs = self.check_expression(&lhs)?;
                let rhs = self.check_expression(&rhs)?;

                if lhs.ty != rhs.ty {
                    return Err(TypeError::BinaryMismatch {
                        src: self.src.clone(),
                        lhs: lhs.span,
                        rhs: rhs.span,
                    });
                }

                let kind = typed_ir::ExpressionKind::Binary {
                    lhs: Box::new(lhs.clone()),
                    rhs: Box::new(rhs),
                    op: *op,
                };

                (kind, lhs.ty)
            }
            _ => todo!(),
            // ast::ExpressionKind::Unary { expr, op } => Ok(typed_ir::ExpressionKind::Unary {
            //     expr: Box::new(self.check_expression(*expr)?),
            //     op,
            // }),
            // ast::ExpressionKind::Variable { name } => {
            //     Ok(typed_ir::ExpressionKind::Variable { name })
            // }
            // ast::ExpressionKind::Call { name, args } => {
            //     let mut checked_args = Vec::new();
            //     for arg in args {
            //         checked_args.push(self.check_expression(arg)?);
            //     }
            //     Ok(typed_ir::ExpressionKind::Call {
            //         name,
            //         args: checked_args,
            //     })
            // }
            // ast::ExpressionKind::Number(num) => Ok(typed_ir::ExpressionKind::Number(num)),
            // ast::ExpressionKind::String(string) => Ok(typed_ir::ExpressionKind::String(string)),
            // ast::ExpressionKind::Boolean(bool) => Ok(typed_ir::ExpressionKind::Boolean(bool)),
        };

        Ok(typed_ir::Expression {
            inner: kind,
            span: expr.span,
            ty: types::Type::Base(types::BaseType::Number),
        })
    }
}
