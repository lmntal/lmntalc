use lmntalc_core::{
    codegen::{
        IRSet,
        rule::{Case, RuleIR},
    },
    model::{Data, guard::ProcessConstraint},
};

use crate::ir::{BinaryOperator, LMNtalIR, UnaryOperator};

use super::{
    Backend, BackendError,
    common::{BackendState, Dialect, print_operation},
};

static HEADER: &[u8] = include_bytes!("../../assets/lib/python/lmntal.py");

pub struct PythonBackend {
    state: BackendState,
}

impl Backend for PythonBackend {
    fn new() -> Self {
        Self {
            state: BackendState::default(),
        }
    }

    fn emit(&mut self, ir_set: &IRSet) -> Result<String, BackendError> {
        let mut code = String::new();
        code.push_str(std::str::from_utf8(HEADER).expect("embedded runtime must be utf-8"));
        code.push_str(&self.print_rules(&ir_set.rules)?);
        let main = self.print_main(ir_set)?;
        code.push_str(&self.print_name_map());
        code.push_str(&main);
        Ok(code)
    }
}

impl Dialect for PythonBackend {
    fn string_literal(&self, value: &str) -> String {
        format!("str(\"{}\")", value)
    }

    fn char_literal(&self, value: char) -> String {
        format!("'{}'", value)
    }

    fn type_name(&self, ty: ProcessConstraint) -> Result<&'static str, BackendError> {
        match ty {
            ProcessConstraint::Int => Ok("int"),
            ProcessConstraint::Float => Ok("float"),
            ProcessConstraint::String => Ok("str"),
            ProcessConstraint::Hyperlink
            | ProcessConstraint::Unique
            | ProcessConstraint::Ground
            | ProcessConstraint::Unary => {
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
            Ok(format!("get_hyperlink_at_port(atom_{}, {})", atom_id, port))
        } else {
            Ok(format!(
                "atom_{}.at({}).get_{}()",
                atom_id,
                port,
                self.type_name(ty)?
            ))
        }
    }

    fn binary_operator(&self, op: BinaryOperator) -> Result<&'static str, BackendError> {
        match op {
            BinaryOperator::Pow => Ok("**"),
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
            "num" => Ok(format!("{}.arity()", args[0])),
            _ => Err(BackendError::UnsupportedFunction {
                function: name.to_string(),
            }),
        }
    }
}

impl PythonBackend {
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
                code.push_str(&format!("atom_{} = create_atom({}, {})", id, name, arity));
                if !data.is_empty() {
                    code.push_str(&format!("\n{}", " ".repeat(indent)));
                    match data {
                        Data::Int(i) => code.push_str(&format!("atom_{}.set_int({})", id, i)),
                        Data::Float(f) => code.push_str(&format!("atom_{}.set_float({})", id, f)),
                        Data::Char(c) => code.push_str(&format!("atom_{}.set_char({})", id, c)),
                        Data::String(s) => {
                            code.push_str(&format!("atom_{}.set_string(\"{}\")", id, s))
                        }
                        Data::Variable(var_id) => {
                            let ty = self.state.var_type.get(&(*var_id).into()).copied().ok_or(
                                BackendError::MissingVariableType {
                                    variable_id: (*var_id).into(),
                                },
                            )?;
                            code.push_str(&format!(
                                "atom_{}.set_{}(var_{})",
                                id,
                                self.type_name(ty)?,
                                var_id
                            ));
                        }
                        Data::Empty => {}
                    }
                }
            }
            LMNtalIR::RemoveAtom { id } => {
                code.push_str(&format!("remove_atom(atom_{})", id));
            }
            LMNtalIR::Link { src, dst } => {
                code.push_str(&format!("link({}, {})", fmt(*src), fmt(*dst)));
            }
            LMNtalIR::Relink { src, src_port, dst } => {
                code.push_str(&format!(
                    "relink(atom_{}, {}, {})",
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
                    code.push_str(&format!("atom_{}.at({}).arity() == 1", id, port));
                }
                _ => {
                    code.push_str(&format!(
                        "atom_{}.at({}).is_{}()",
                        id,
                        port,
                        self.type_name(*ty)?
                    ));
                }
            },
            LMNtalIR::CheckValue(op) => code.push_str(&print_operation(self, op)?),
            LMNtalIR::DefineTempVar { id, ty, op, .. } => {
                code.push_str(&format!("var_{} = {}", id, print_operation(self, op)?));
                self.state.var_type.insert(*id, *ty);
            }
            LMNtalIR::CloneAtom {
                id,
                from_id,
                from_port,
            } => {
                code.push_str(&format!(
                    "atom_{} = clone_atom(atom_{}, {})",
                    id, from_id, from_port
                ));
            }
            LMNtalIR::FindAtom { id, name, arity } => {
                let name = self.state.get_name(name);
                code.push_str(&format!("atom_{} = find_atom({}, {})", id, name, arity));
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
                    "atom_{} = get_atom_at_port(atom_{}, {}, {}, {})",
                    id, from, port, name, arity
                ));
            }
            LMNtalIR::GetHyperlinkAtPort { id, from, port } => {
                code.push_str(&format!(
                    "hl_{} = get_hyperlink_at_port(atom_{}, {})",
                    id, from, port
                ));
            }
            LMNtalIR::RemoveAtomAt { id, port } => {
                code.push_str(&format!("atom_{}.remove_at({})", id, port));
            }
            LMNtalIR::CreateHyperlink { id, name } => {
                let name = self.state.get_name(name);
                code.push_str(&format!("hl_{} = create_hyperlink({})", id, name));
            }
            LMNtalIR::LinkToHyperlink { atom, hyperlink } => {
                code.push_str(&format!("hl_{}.add({})", hyperlink.id(), fmt(*atom)));
            }
            LMNtalIR::RemoveFromHyperlink { atom, hyperlink } => {
                code.push_str(&format!("hl_{}.remove({})", hyperlink.id(), fmt(*atom)));
            }
            LMNtalIR::FuseHyperlink { into, from } => {
                code.push_str(&format!("hl_{}.fuse(hl_{})", into.id(), from.id()));
            }
            LMNtalIR::Unify { into, from } => {
                code.push_str(&format!("unify({}, {})", fmt(*into), fmt(*from)));
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
        code.push_str("def main():\n");

        for node in &ir_set.init.body {
            code.push_str(&self.render_ir(node, 4)?);
            code.push('\n');
        }

        let rules = ir_set.rules.len();

        code.push_str(&format!("    rule_fail = {} * [False]\n", rules));
        code.push_str("    rules = [\n");

        for rule in &ir_set.rules {
            code.push_str(&format!("        {},\n", rule.name));
        }

        code.push_str("    ]\n");

        if rules == 0 {
            code.push_str(
                r#"    dump_atoms()

if __name__ == "__main__":
    main()"#,
            );
            return Ok(code);
        }

        let rules_minus_1 = rules - 1;
        code.push_str(&format!(
            r#"    while not all(rule_fail):
        rand = random.randint(0, {rules_minus_1})
        if rules[rand]():
            rule_fail = {rules} * [False]
        else:
            rule_fail[rand] = True
    dump_atoms()

if __name__ == "__main__":
    main()
"#,
        ));

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
        let mut indent = 4;
        code.push_str(&format!("def {}() -> bool:\n", rule.name));

        for ir in &rule.pattern {
            code.push_str(&self.print_pattern(ir, &mut indent)?);
        }

        let one_case = rule.cases.len() == 1;
        for node in &rule.cases {
            code.push_str(&self.print_case(node, one_case, &mut indent)?);
            code.push('\n');
        }

        code.push_str("    return False\n\n");
        Ok(code)
    }

    fn print_pattern(&mut self, ir: &LMNtalIR, indent: &mut usize) -> Result<String, BackendError> {
        let mut code = String::new();
        match ir {
            LMNtalIR::FindAtom { id, name, arity } => {
                let name = self.state.get_name(name);
                code.push_str(&format!(
                    "{}for atom_{} in find_atom({}, {}):\n",
                    " ".repeat(*indent),
                    id,
                    name,
                    arity
                ));
                *indent += 4;
            }
            LMNtalIR::GetAtomAtPort { id, .. } => {
                code.push_str(&self.render_ir(ir, *indent)?);
                code.push_str(&format!(
                    "\n{}if not atom_{}:\n{}continue\n",
                    " ".repeat(*indent),
                    id,
                    " ".repeat(*indent + 4)
                ));
            }
            LMNtalIR::GetHyperlinkAtPort { id, .. } => {
                code.push_str(&self.render_ir(ir, *indent)?);
                code.push_str(&format!(
                    "\n{}if not hl_{}:\n{}continue\n",
                    " ".repeat(*indent),
                    id,
                    " ".repeat(*indent + 4)
                ));
            }
            LMNtalIR::AtomEqualityIdPort { id_port_list, eq } => {
                if *eq {
                    code.push_str(&format!("{}if not equals(", " ".repeat(*indent)));
                } else {
                    code.push_str(&format!("{}if equals(", " ".repeat(*indent)));
                }
                for (i, (id, port)) in id_port_list.iter().enumerate() {
                    if i != 0 {
                        code.push_str(", ");
                    }
                    code.push_str(&format!("atom_{}.at({})", id, port));
                }
                code.push_str(&format!("):\n{}continue\n", " ".repeat(*indent + 4)));
            }
            LMNtalIR::AtomEquality {
                id_list,
                eq,
                hyperlinks,
            } => {
                if *eq {
                    code.push_str(&format!("{}if not equals(", " ".repeat(*indent)));
                } else {
                    code.push_str(&format!("{}if equals(", " ".repeat(*indent)));
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

                code.push_str(&format!("):\n{}continue\n", " ".repeat(*indent + 4)));
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

            code.push_str(&format!("{}return True\n", " ".repeat(*indent)));
            return Ok(code);
        }
        code.push_str(&format!("{}if ", " ".repeat(*indent)));

        if one_case {
            code.push_str("not (");
        }

        let conditions = case
            .condition
            .iter()
            .map(|ir| self.render_ir(ir, 0))
            .collect::<Result<Vec<_>, _>>()?
            .join(" and ");
        code.push_str(&conditions);

        if one_case {
            code.push_str(&format!("):\n{}continue\n", " ".repeat(*indent + 4)));
        } else {
            code.push_str(":\n");
            *indent += 4;
        }

        for ir in &case.definition {
            code.push_str(&self.render_ir(ir, *indent)?);
            code.push('\n');
        }

        for ir in &case.body {
            code.push_str(&self.render_ir(ir, *indent)?);
            code.push('\n');
        }

        code.push_str(&format!("{}return True\n", " ".repeat(*indent)));

        if !one_case {
            *indent -= 4;
        }

        Ok(code)
    }

    fn print_name_map(&self) -> String {
        let mut code = "name_map = {\n".to_string();
        for (name, id) in &self.state.name_map {
            code.push_str(&format!("    {}: \"{}\",\n", id, name));
        }
        code.push_str("}\n\n");

        code
    }
}
