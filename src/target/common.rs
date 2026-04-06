use std::collections::HashMap;

use lmntalc_core::model::guard::{self, ProcessConstraint};

use crate::ir::{BinaryOperator, Literal, Operation, UnaryOperator, VarSource};

use super::BackendError;

#[derive(Debug, Default)]
pub struct BackendState {
    pub var_type: HashMap<usize, ProcessConstraint>,
    pub name_map: HashMap<String, usize>,
}

impl BackendState {
    pub fn get_name(&mut self, name: &str) -> usize {
        if let Some(id) = self.name_map.get(name) {
            *id
        } else {
            let id = self.name_map.len();
            self.name_map.insert(name.to_string(), id);
            id
        }
    }
}

pub trait Dialect {
    fn string_literal(&self, value: &str) -> String;
    fn char_literal(&self, value: char) -> String;
    fn type_name(&self, ty: ProcessConstraint) -> Result<&'static str, BackendError>;
    fn port_value_expr(
        &self,
        atom_id: usize,
        port: usize,
        ty: ProcessConstraint,
    ) -> Result<String, BackendError>;
    fn temp_variable_expr(&self, id: usize) -> String {
        format!("var_{}", id)
    }
    fn binary_operator(&self, op: BinaryOperator) -> Result<&'static str, BackendError>;
    fn unary_operator(&self, op: UnaryOperator) -> &'static str;
    fn reserved_function(
        &self,
        name: &str,
        args: &[String],
        ty: ProcessConstraint,
    ) -> Result<String, BackendError>;
}

pub fn print_operation<D: Dialect>(dialect: &D, op: &Operation) -> Result<String, BackendError> {
    match op {
        Operation::Literal(literal) => Ok(match literal {
            Literal::Int(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::Char(c) => dialect.char_literal(*c),
            Literal::String(s) => dialect.string_literal(s),
        }),
        Operation::Variable { source, ty_ } => match source {
            VarSource::Head(id, port) | VarSource::Body(id, port) => {
                dialect.port_value_expr(*id, *port, *ty_)
            }
            VarSource::Variable(id) => Ok(dialect.temp_variable_expr(*id)),
        },
        Operation::BinaryOP { op, lhs, rhs } => Ok(format!(
            "({} {} {})",
            print_operation(dialect, lhs)?,
            dialect.binary_operator(*op)?,
            print_operation(dialect, rhs)?
        )),
        Operation::UnaryOP { op, operand } => Ok(format!(
            "{}{}",
            dialect.unary_operator(*op),
            print_operation(dialect, operand)?
        )),
        Operation::FunctionCall { name, args, ty_ } => {
            if guard::RESERVED_FUNC.iter().any(|func| func.name == name) {
                let rendered = args
                    .iter()
                    .map(|arg| print_operation(dialect, arg))
                    .collect::<Result<Vec<_>, _>>()?;
                dialect.reserved_function(name, &rendered, *ty_)
            } else {
                Err(BackendError::UnsupportedFunction {
                    function: name.clone(),
                })
            }
        }
    }
}
