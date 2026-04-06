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

static LMNTAL: &[u8] = include_bytes!("../../assets/lib/java/lmntal.java");
static MAIN: &[u8] = include_bytes!("../../assets/lib/java/main.java");

pub struct JavaBackend {
    state: BackendState,
}

impl Backend for JavaBackend {
    fn new() -> Self {
        Self {
            state: BackendState::default(),
        }
    }

    fn emit(&mut self, ir_set: &IRSet) -> Result<String, BackendError> {
        let mut code = String::new();
        code.push_str(std::str::from_utf8(LMNTAL).expect("embedded runtime must be utf-8"));
        code.push_str("class Main {\n");
        code.push_str(&self.print_rules(&ir_set.rules)?);
        code.push_str(&self.print_main(ir_set)?);
        code.push_str(std::str::from_utf8(MAIN).expect("embedded runtime must be utf-8"));
        code.push_str("\n}\n");
        Ok(code)
    }
}

impl Dialect for JavaBackend {
    fn string_literal(&self, value: &str) -> String {
        format!("\"{}\"", value)
    }

    fn char_literal(&self, value: char) -> String {
        format!("'{}'", value)
    }

    fn type_name(&self, ty: ProcessConstraint) -> Result<&'static str, BackendError> {
        match ty {
            ProcessConstraint::Int => Ok("Int"),
            ProcessConstraint::Float => Ok("Float"),
            ProcessConstraint::String => Ok("String"),
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
            Ok(format!("getHyperlinkAtPort(atom_{}, {})", atom_id, port))
        } else {
            Ok(format!(
                "atom_{}.at({}).get{}()",
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
            "num" => Ok(format!("{}.getArity()", args[0])),
            _ => Err(BackendError::UnsupportedFunction {
                function: name.to_string(),
            }),
        }
    }
}

impl JavaBackend {
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
                    "var atom_{} = AtomStore.INSTANCE.createAtom({}, {});",
                    id, name, arity
                ));
                if !data.is_empty() {
                    code.push_str(&format!("\n{}", " ".repeat(indent)));
                    match data {
                        Data::Int(i) => code.push_str(&format!("atom_{}.setInt({});", id, i)),
                        Data::Float(f) => code.push_str(&format!("atom_{}.setFloat({});", id, f)),
                        Data::Char(c) => code.push_str(&format!("atom_{}.setChar({});", id, c)),
                        Data::String(s) => {
                            code.push_str(&format!("atom_{}.setString(\"{}\");", id, s))
                        }
                        Data::Variable(var_id) => {
                            let ty = self.state.var_type.get(&(*var_id).into()).copied().ok_or(
                                BackendError::MissingVariableType {
                                    variable_id: (*var_id).into(),
                                },
                            )?;
                            code.push_str(&format!(
                                "atom_{}.set{}(var_{});",
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
                code.push_str(&format!("AtomStore.INSTANCE.removeAtom(atom_{});", id));
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
                    code.push_str(&format!("atom_{}.at({}).getArity() == 1", id, port));
                }
                _ => {
                    code.push_str(&format!(
                        "atom_{}.at({}).is{}()",
                        id,
                        port,
                        self.type_name(*ty)?
                    ));
                }
            },
            LMNtalIR::CheckValue(op) => code.push_str(&print_operation(self, op)?),
            LMNtalIR::DefineTempVar { id, ty, op, .. } => {
                code.push_str(&format!("var var_{} = {};", id, print_operation(self, op)?));
                self.state.var_type.insert(*id, *ty);
            }
            LMNtalIR::CloneAtom {
                id,
                from_id,
                from_port,
            } => {
                code.push_str(&format!(
                    "var atom_{} = cloneAtom(atom_{}, {});",
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
                    "var atom_{} = atom_{}.getAtomAtPort({}, {}, {});",
                    id, from, port, name, arity
                ));
            }
            LMNtalIR::GetHyperlinkAtPort { id, from, port } => {
                code.push_str(&format!(
                    "var hl_{} = getHyperlinkAtPort(atom_{}, {});",
                    id, from, port
                ));
            }
            LMNtalIR::RemoveAtomAt { id, port } => {
                code.push_str(&format!("atom_{}.removeAt({});", id, port));
            }
            LMNtalIR::CreateHyperlink { id, name } => {
                let name = self.state.get_name(name);
                code.push_str(&format!(
                    "var hl_{} = AtomStore.INSTANCE.createHyperlink({});",
                    id, name
                ));
            }
            LMNtalIR::LinkToHyperlink { atom, hyperlink } => {
                code.push_str(&format!("hl_{}.add({});", hyperlink.id(), fmt(*atom)));
            }
            LMNtalIR::RemoveFromHyperlink { atom, hyperlink } => {
                code.push_str(&format!("hl_{}.remove({});", hyperlink.id(), fmt(*atom)));
            }
            LMNtalIR::FuseHyperlink { into, from } => {
                code.push_str(&format!("hl_{}.fuse(hl_{});", into.id(), from.id()));
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
        code.push_str(
            "    public static String getName(int name) {\n        return switch (name) {\n",
        );
        for (name, id) in &self.state.name_map {
            code.push_str(&format!("            case {} -> \"{}\";\n", id, name));
        }
        code.push_str("            default -> \"\";\n        };\n    }\n\n");

        code.push_str("    public static void main(String[] args) {\n");
        for node in &ir_set.init.body {
            code.push_str(&self.render_ir(node, 8)?);
            code.push('\n');
        }

        let rules = ir_set.rules.len();
        if rules == 0 {
            code.push_str("        dumpAtoms();\n    }\n\n");
            return Ok(code);
        }

        code.push_str(&format!("        var rule_fail = new BitSet({});\n", rules));
        code.push_str("        var rules = new Rule[]{\n");

        for rule in &ir_set.rules {
            code.push_str(&format!("{}new {}(),\n", " ".repeat(12), rule.name));
        }

        code.push_str("        };\n");
        code.push_str("        Random rng = new Random();\n");
        code.push_str(&format!(
            r#"        while (rule_fail.cardinality() != {rules}) {{
            var rand = rng.nextInt({rules});
            if (rules[rand].apply()) {{
                rule_fail.clear();
            }} else {{
                rule_fail.set(rand);
            }}
        }}
        dumpAtoms();
"#,
        ));

        code.push_str("    }\n\n");
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
        let mut indent = 12;
        code.push_str(&format!(
            "    static class {} implements Rule {{\n        @Override\n        public boolean apply() {{\n",
            rule.name
        ));

        for ir in &rule.pattern {
            code.push_str(&self.print_pattern(ir, &mut indent)?);
        }

        let one_case = rule.cases.len() == 1;
        for node in &rule.cases {
            code.push_str(&self.print_case(node, one_case, &mut indent)?);
            code.push('\n');
        }

        while indent > 12 {
            indent -= 4;
            code.push_str(&format!("{}}}\n", " ".repeat(indent)));
        }

        code.push_str("            return false;\n        }\n    }\n\n");
        Ok(code)
    }

    fn print_pattern(&mut self, ir: &LMNtalIR, indent: &mut usize) -> Result<String, BackendError> {
        let mut code = String::new();
        match ir {
            LMNtalIR::FindAtom { id, name, arity } => {
                let name = self.state.get_name(name);
                let it = format!("it_{}", id);
                code.push_str(&format!(
                    "{}for (var {} = findAtom({}, {}).iterator(); {}.hasNext(); ) {{\n{}var atom_{} = {}.next();\n",
                    " ".repeat(*indent),
                    it,
                    name,
                    arity,
                    it,
                    " ".repeat(*indent + 4),
                    id,
                    it,
                ));
                *indent += 4;
            }
            LMNtalIR::GetAtomAtPort { id, .. } => {
                code.push_str(&self.render_ir(ir, *indent)?);
                code.push_str(&format!(" if (atom_{} == null) continue;\n", id));
            }
            LMNtalIR::GetHyperlinkAtPort { id, .. } => {
                code.push_str(&self.render_ir(ir, *indent)?);
                code.push_str(&format!(" if (hl_{} == null) continue;\n", id));
            }
            LMNtalIR::AtomEqualityIdPort { id_port_list, eq } => {
                if *eq {
                    code.push_str(&format!("{}if (!Main.equals(", " ".repeat(*indent)));
                } else {
                    code.push_str(&format!("{}if (Main.equals(", " ".repeat(*indent)));
                }
                for (i, (id, port)) in id_port_list.iter().enumerate() {
                    if i != 0 {
                        code.push_str(", ");
                    }
                    code.push_str(&format!("atom_{}.at({})", id, port));
                }
                code.push_str(")) continue;\n");
            }
            LMNtalIR::AtomEquality {
                id_list,
                eq,
                hyperlinks,
            } => {
                if *eq {
                    code.push_str(&format!("{}if (!Main.equals(", " ".repeat(*indent)));
                } else {
                    code.push_str(&format!("{}if (Main.equals(", " ".repeat(*indent)));
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

            code.push_str(&format!("{}return true;\n", " ".repeat(*indent)));
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

        if !one_case {
            code.push_str(&format!(
                "{}return true;\n{}}}",
                " ".repeat(*indent),
                " ".repeat(*indent - 4)
            ));
            *indent -= 4;
        } else {
            code.push_str(&format!("{}return true;\n", " ".repeat(*indent)));
        }

        Ok(code)
    }
}
