use crate::{
    codegen::{
        rule::{Case, RuleIR},
        IRSet,
    },
    ir::{BinaryOperator, LMNtalIR, UnaryOperator},
    model::{guard::ProcessConstraint, Data},
};

use super::{
    common::{print_operation, BackendState, Dialect},
    Backend, BackendError,
};

static LMNTAL: &[u8] = include_bytes!("../../assets/lib/cpp/lmntal.hpp");

pub struct CppBackend {
    state: BackendState,
}

impl Backend for CppBackend {
    fn new() -> Self {
        Self {
            state: BackendState::default(),
        }
    }

    fn emit(&mut self, ir_set: &IRSet) -> Result<String, BackendError> {
        let mut code = String::new();
        code.push_str(std::str::from_utf8(LMNTAL).expect("embedded runtime must be utf-8"));
        let rules = self.print_rules(&ir_set.rules)?;
        let main = self.print_main(ir_set)?;
        code.push_str(&self.print_name_map());
        code.push_str(&rules);
        code.push_str(&main);
        Ok(code)
    }
}

impl Dialect for CppBackend {
    fn string_literal(&self, value: &str) -> String {
        format!("std::string(\"{}\")", value)
    }

    fn char_literal(&self, value: char) -> String {
        format!("'{}'", value)
    }

    fn type_name(&self, ty: ProcessConstraint) -> Result<&'static str, BackendError> {
        match ty {
            ProcessConstraint::Int => Ok("int"),
            ProcessConstraint::Float => Ok("float"),
            ProcessConstraint::String => Ok("string"),
            ProcessConstraint::Unary => Ok("unary"),
            ProcessConstraint::Hyperlink
            | ProcessConstraint::Unique
            | ProcessConstraint::Ground => {
                Err(BackendError::UnsupportedConstraint { constraint: ty })
            }
        }
    }

    fn port_value_expr(
        &self,
        atom_id: usize,
        port: usize,
        ty: ProcessConstraint,
    ) -> Result<String, BackendError> {
        if ty == ProcessConstraint::Hyperlink {
            Ok(format!("get_hlink_at_port(atom_{}, {})", atom_id, port))
        } else {
            Ok(format!(
                "atom_{}->at({}).get_{}()",
                atom_id,
                port,
                self.type_name(ty)?
            ))
        }
    }

    fn binary_operator(&self, op: BinaryOperator) -> Result<&'static str, BackendError> {
        match op {
            BinaryOperator::Pow => Err(BackendError::UnsupportedOperator { operator: "**" }),
            BinaryOperator::Add => Ok("+"),
            BinaryOperator::Sub => Ok("-"),
            BinaryOperator::Mul => Ok("*"),
            BinaryOperator::Div => Ok("/"),
            BinaryOperator::Mod => Ok("%"),
            BinaryOperator::Eq => Ok("=="),
            BinaryOperator::Ne => Ok("!="),
            BinaryOperator::Lt => Ok("<"),
            BinaryOperator::Le => Ok("<="),
            BinaryOperator::Gt => Ok(">"),
            BinaryOperator::Ge => Ok(">="),
        }
    }

    fn unary_operator(&self, op: UnaryOperator) -> &'static str {
        match op {
            UnaryOperator::Not => "!",
            UnaryOperator::Neg => "-",
        }
    }

    fn reserved_function(
        &self,
        name: &str,
        args: &[String],
        _ty: ProcessConstraint,
    ) -> Result<String, BackendError> {
        match name {
            "num" => Ok(format!("{}->get_arity()", args[0])),
            _ => Err(BackendError::UnsupportedFunction {
                function: name.to_string(),
            }),
        }
    }
}

impl CppBackend {
    fn render_ir(&mut self, ir: &LMNtalIR, indent: usize) -> Result<String, BackendError> {
        let mut code = " ".repeat(indent);
        let fmt = |source| match source {
            crate::ir::VarSource::Variable(id) => format!("atom_{}, 0", id),
            crate::ir::VarSource::Head(id, port) | crate::ir::VarSource::Body(id, port) => {
                format!("atom_{}, {}", id, port)
            }
        };
        match ir {
            LMNtalIR::CreateAtom {
                id,
                name,
                arity,
                data,
            } => {
                let name = self.state.get_name(name);
                code.push_str(&format!(
                    "auto atom_{} = create_atom({}, {});",
                    id, name, arity
                ));
                if !data.is_empty() {
                    code.push_str(&format!("\n{}", " ".repeat(indent)));
                    match data {
                        Data::Int(i) => {
                            code.push_str(&format!("atom_{}->set_int({});", id, i));
                        }
                        Data::Float(f) => {
                            code.push_str(&format!("atom_{}->set_float({});", id, f));
                        }
                        Data::Variable(var_id) => {
                            let ty = self.state.var_type.get(&(*var_id).into()).copied().ok_or(
                                BackendError::MissingVariableType {
                                    variable_id: (*var_id).into(),
                                },
                            )?;
                            code.push_str(&format!(
                                "atom_{}->set_{}(var_{});",
                                id,
                                self.type_name(ty)?,
                                var_id
                            ));
                        }
                        Data::Char(_) => {
                            return Err(BackendError::UnsupportedData { kind: "character" });
                        }
                        Data::String(_) => {
                            return Err(BackendError::UnsupportedData { kind: "string" });
                        }
                        Data::Empty => {}
                    }
                }
            }
            LMNtalIR::RemoveAtom { id } => {
                code.push_str(&format!("remove_atom(atom_{});", id));
            }
            LMNtalIR::Link { src, dst } => {
                code.push_str(&format!("link({}, {});", fmt(*src), fmt(*dst)));
            }
            LMNtalIR::Relink { src, src_port, dst } => {
                code.push_str(&format!(
                    "relink(atom_{}, {}, {});",
                    src,
                    src_port,
                    fmt(*dst)
                ));
            }
            LMNtalIR::CheckType { id, port, ty } => match ty {
                ProcessConstraint::Hyperlink => {
                    return Err(BackendError::UnsupportedConstraint { constraint: *ty });
                }
                ProcessConstraint::Unique | ProcessConstraint::Ground => {
                    return Err(BackendError::UnsupportedConstraint { constraint: *ty });
                }
                ProcessConstraint::Unary => {
                    code.push_str(&format!("atom_{}->at({})->get_arity() == 1", id, port));
                }
                _ => {
                    code.push_str(&format!(
                        "atom_{}->at({}).is_{}()",
                        id,
                        port,
                        self.type_name(*ty)?
                    ));
                }
            },
            LMNtalIR::CheckValue(op) => code.push_str(&print_operation(self, op)?),
            LMNtalIR::DefineTempVar { id, op, ty, .. } => {
                code.push_str(&format!(
                    "auto var_{} = {};",
                    id,
                    print_operation(self, op)?
                ));
                self.state.var_type.insert(*id, *ty);
            }
            LMNtalIR::CloneAtom {
                id,
                from_id,
                from_port,
            } => {
                code.push_str(&format!(
                    "auto atom_{} = clone_atom(atom_{}, {});",
                    id, from_id, from_port
                ));
            }
            LMNtalIR::GetAtomAtPort {
                id,
                from,
                port,
                name,
                arity,
            } => {
                let name = self.state.get_name(name);
                code.push_str(&format!(
                    "auto atom_{} = get_atom_at_port(atom_{}, {}, {}, {});",
                    id, from, port, name, arity
                ));
            }
            LMNtalIR::GetHyperlinkAtPort { id, from, port } => {
                code.push_str(&format!(
                    "auto hl_{} = get_hlink_at_port(atom_{}, {});",
                    id, from, port
                ));
            }
            LMNtalIR::RemoveAtomAt { id, port } => {
                code.push_str(&format!("atom_{}->remove_at({});", id, port));
            }
            LMNtalIR::CreateHyperlink { id, name } => {
                let name = self.state.get_name(name);
                code.push_str(&format!("auto hl_{} = create_hyperlink({});", id, name));
            }
            LMNtalIR::LinkToHyperlink { atom, hyperlink } => {
                code.push_str(&format!("hl_{}->add({});", hyperlink.id(), fmt(*atom)));
            }
            LMNtalIR::RemoveFromHyperlink { atom, hyperlink } => {
                code.push_str(&format!("hl_{}->remove({});", hyperlink.id(), fmt(*atom)));
            }
            LMNtalIR::FuseHyperlink { into, from } => {
                code.push_str(&format!("hl_{}->fuse(hl_{});\n", into.id(), from.id()));
                code.push_str(&format!(
                    "{}remove_hyperlink(hl_{});",
                    " ".repeat(indent),
                    from.id()
                ));
            }
            LMNtalIR::Unify { into, from } => {
                code.push_str(&format!("unify({}, {});", fmt(*into), fmt(*from)));
            }
            _ => {
                return Err(BackendError::UnsupportedInstruction {
                    instruction: "pattern-only IR in statement context",
                });
            }
        }
        Ok(code)
    }

    fn print_main(&mut self, ir_set: &IRSet) -> Result<String, BackendError> {
        let mut code = String::new();
        code.push_str("int main() {\n");
        for node in &ir_set.init.body {
            code.push_str(&self.render_ir(node, 2)?);
            code.push('\n');
        }

        let rules = ir_set.rules.len();
        if rules == 0 {
            code.push_str("  dump_atoms();\n}\n");
            return Ok(code);
        }

        code.push_str(&format!("  std::bitset<{}> rule_fail;\n", rules));
        code.push_str(&format!("  constexpr rule rules[{}] = {{\n", rules));

        for rule in &ir_set.rules {
            code.push_str(&format!("    {},\n", rule.name));
        }

        code.push_str("  };\n");
        code.push_str("  std::mt19937 rng(std::random_device{}());\n");
        code.push_str(&format!(
            r#"  while (!rule_fail.all()) {{
    auto const rand = rng() % {rules};
    if (rules[rand]()) {{
      rule_fail.reset();
    }} else {{
      rule_fail.set(rand);
    }}
  }}
  dump_atoms();
"#,
        ));

        code.push_str("}\n");
        Ok(code)
    }

    fn print_rules(&mut self, rules: &[RuleIR]) -> Result<String, BackendError> {
        let mut code = String::new();
        for rule in rules {
            code.push_str(&self.print_rule(rule)?);
        }
        Ok(code)
    }

    fn print_rule(&mut self, rule: &RuleIR) -> Result<String, BackendError> {
        let mut code = String::new();
        let mut indent = 2;
        code.push_str(&format!("bool {}() {{\n", rule.name));

        for ir in &rule.pattern {
            code.push_str(&self.print_pattern(ir, &mut indent)?);
        }

        let one_case = rule.cases.len() == 1;
        for node in &rule.cases {
            code.push_str(&self.print_case(node, one_case, &mut indent)?);
            code.push('\n');
        }

        while indent > 2 {
            indent -= 2;
            code.push_str(&format!("{}}}\n", " ".repeat(indent)));
        }

        code.push_str("  return false;\n}\n\n");
        Ok(code)
    }

    fn print_pattern(&mut self, ir: &LMNtalIR, indent: &mut usize) -> Result<String, BackendError> {
        let mut code = String::new();
        match ir {
            LMNtalIR::FindAtom { id, name, arity } => {
                let name = self.state.get_name(name);
                code.push_str(&format!(
                    "{}for (auto atom_{} : find_atom({}, {})) {{\n",
                    " ".repeat(*indent),
                    id,
                    name,
                    arity
                ));
                *indent += 2;
            }
            LMNtalIR::GetAtomAtPort { id, .. } => {
                code.push_str(&self.render_ir(ir, *indent)?);
                code.push_str(&format!(" if (!atom_{}) continue;\n", id));
            }
            LMNtalIR::GetHyperlinkAtPort { id, .. } => {
                code.push_str(&self.render_ir(ir, *indent)?);
                code.push_str(&format!(" if (!hl_{}) continue;\n", id));
            }
            LMNtalIR::AtomEqualityIdPort { id_port_list, eq } => {
                if *eq {
                    code.push_str(&format!("{}if (!equals(", " ".repeat(*indent)));
                } else {
                    code.push_str(&format!("{}if (equals(", " ".repeat(*indent)));
                }
                for (i, (id, port)) in id_port_list.iter().enumerate() {
                    if i != 0 {
                        code.push_str(", ");
                    }
                    code.push_str(&format!("atom_{}->at({})", id, port));
                }
                code.push_str(")) continue;\n");
            }
            LMNtalIR::AtomEquality {
                id_list,
                eq,
                hyperlinks,
            } => {
                if *eq {
                    code.push_str(&format!("{}if (!equals(", " ".repeat(*indent)));
                } else {
                    code.push_str(&format!("{}if (equals(", " ".repeat(*indent)));
                }
                if *hyperlinks {
                    for (i, id) in id_list.iter().enumerate() {
                        if i != 0 {
                            code.push_str(", ");
                        }
                        code.push_str(&format!("hl_{}", id));
                    }
                } else {
                    for (i, id) in id_list.iter().enumerate() {
                        if i != 0 {
                            code.push_str(", ");
                        }
                        code.push_str(&format!("atom_{}", id));
                    }
                }

                code.push_str(")) continue;\n");
            }
            _ => {
                code.push_str(&self.render_ir(ir, *indent)?);
                code.push('\n');
            }
        }
        Ok(code)
    }

    fn print_case(
        &mut self,
        case: &Case,
        one_case: bool,
        indent: &mut usize,
    ) -> Result<String, BackendError> {
        let mut code = String::new();
        if case.condition.is_empty() {
            for ir in &case.definition {
                code.push_str(&self.render_ir(ir, *indent)?);
                code.push('\n');
            }

            for ir in &case.body {
                code.push_str(&self.render_ir(ir, *indent)?);
                code.push('\n');
            }

            code.push_str(&format!("{}return true;", " ".repeat(*indent)));
            return Ok(code);
        }
        code.push_str(&format!("{}if (", " ".repeat(*indent)));

        if one_case {
            code.push_str("!(")
        }

        let conditions = case
            .condition
            .iter()
            .map(|ir| self.render_ir(ir, 0))
            .collect::<Result<Vec<_>, _>>()?
            .join(" && ");
        code.push_str(&conditions);

        if one_case {
            code.push_str(&format!("))\n{}continue;\n", " ".repeat(*indent + 2)));
        } else {
            code.push_str(") {\n");
            *indent += 2;
        }

        for ir in &case.definition {
            code.push_str(&self.render_ir(ir, *indent)?);
            code.push('\n');
        }

        for ir in &case.body {
            code.push_str(&self.render_ir(ir, *indent)?);
            code.push('\n');
        }

        if !one_case {
            code.push_str(&format!(
                "{}return true;\n{}}}",
                " ".repeat(*indent),
                " ".repeat(*indent - 2)
            ));
            *indent -= 2;
        } else {
            code.push_str(&format!("{}return true;", " ".repeat(*indent)));
        }

        Ok(code)
    }

    fn print_name_map(&self) -> String {
        let mut code =
            String::from("constexpr std::string_view int2str(name_t const n) {\n  switch (n) {\n");

        for (name, id) in &self.state.name_map {
            code.push_str(&format!("    case {}: return \"{}\";\n", id, name));
        }
        code.push_str("    default: return \"unknown\";\n  }\n}\n\n");

        code
    }
}
