use crate::{
    data::{guard::ProcessConstraint, Data},
    generator::{
        rule::{Case, RuleIR},
        Generator,
    },
    ir::{LMNtalIR, Literal, Operation},
};

use super::Backend;

static LMNTAL: &[u8] = include_bytes!("../../assets/lib/java/lmntal.java");
static MAIN: &[u8] = include_bytes!("../../assets/lib/java/main.java");

pub struct JavaBackend;

impl Backend for JavaBackend {
    fn new() -> Self {
        Self
    }

    fn pretty_print(&mut self, generator: &Generator) -> String {
        let mut code = String::new();
        code.push_str(std::str::from_utf8(LMNTAL).unwrap());
        code.push_str("class Main {\n");
        code.push_str(&self.print_rules(&generator.rules));
        code.push_str(&self.print_main(generator));
        code.push_str(std::str::from_utf8(MAIN).unwrap());
        code.push_str("\n}\n");
        code
    }
}

impl JavaBackend {
    fn pretty_print(&self, ir: &LMNtalIR, indent: usize) -> String {
        let mut code = " ".repeat(indent);
        let fmt = |&source| match source {
            crate::ir::VarSource::Definition(id) => {
                format!("temp_{}, 0", id)
            }
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
                code.push_str(&format!(
                    "var atom_{} = AtomStore.INSTANCE.createAtom(\"{}\", {});",
                    id, name, arity
                ));
                match data {
                    Data::Empty => {}
                    Data::Int(i) => {
                        code.push_str(&format!("\n{}", " ".repeat(indent)));
                        code.push_str(&format!("atom_{}.setInt({});", id, i));
                    }
                    Data::Float(f) => {
                        code.push_str(&format!("\n{}", " ".repeat(indent)));
                        code.push_str(&format!("atom_{}.setFloat({});", id, f));
                    }
                    Data::Char(c) => {
                        code.push_str(&format!("\n{}", " ".repeat(indent)));
                        code.push_str(&format!("atom_{}.setChar({});", id, c));
                    }
                    Data::String(s) => {
                        code.push_str(&format!("\n{}", " ".repeat(indent)));
                        code.push_str(&format!("atom_{}.setString(\"{}\");", id, s));
                    }
                }
            }
            LMNtalIR::RemoveAtom { id } => {
                code.push_str(&format!("AtomStore.INSTANCE.removeAtom(atom_{});", id));
            }
            LMNtalIR::Link { src, dst } => {
                code.push_str(&format!("link({}, {});", fmt(src), fmt(dst),));
            }
            LMNtalIR::Relink { src, src_port, dst } => {
                code.push_str(&format!(
                    "relink(atom_{}, {}, {});",
                    src,
                    src_port,
                    fmt(dst)
                ));
            }
            LMNtalIR::CheckType { id, port, ty } => {
                code.push_str(&format!(
                    "atom_{}.at({}).is{}()",
                    id,
                    port,
                    atom_type_to_string(ty),
                ));
            }
            LMNtalIR::CheckValue(op) => code.push_str(&print_operation(op)),
            LMNtalIR::DefineTempVar { id, name, ty, op } => {
                code.push_str(&format!("var temp_{} = AtomStore.INSTANCE.createAtom(\"{}\", 1);\n", id, name));
                code.push_str(&format!(
                    "{}temp_{}.set{}({});",
                    " ".repeat(indent),
                    id,
                    atom_type_to_string(ty),
                    print_operation(op)
                ));
            }
            LMNtalIR::CloneAtom { id, from } => {
                code.push_str(&format!("var atom_{} = cloneAtom(atom_{});", id, from));
            }
            LMNtalIR::FindAtom { id, name, arity } => {
                code.push_str(&format!(
                    "var atom_{} = findAtom(\"{}\", {});",
                    id, name, arity
                ));
            }
            LMNtalIR::GetAtomAtPort {
                id,
                from,
                port,
                name,
                arity,
            } => {
                code.push_str(&format!(
                    "var atom_{} = getAtomAtPort(atom_{}, {}, \"{}\", {});",
                    id, from, port, name, arity
                ));
            }
            LMNtalIR::AtomEquality {
                id_port_list: ids,
                eq,
            } => {
                if *eq {
                    code.push_str("if (equals(");
                } else {
                    code.push_str("if (!equals(");
                }
                for (i, (id, port)) in ids.iter().enumerate() {
                    if i != 0 {
                        code.push_str(", ");
                    }
                    code.push_str(&format!("atom_{}.at({})", id, port,));
                }

                code.push_str(")) return false;\n");
            }
            LMNtalIR::RemoveAtomAt { id, port } => {
                code.push_str(&format!("atom_{}.removeAt({});", id, port));
            }
        }
        code
    }

    fn print_main(&self, generator: &Generator) -> String {
        let mut code = String::new();
        code.push_str("    public static void main(String[] args) {\n");
        for node in &generator.init {
            code.push_str(&self.pretty_print(node, 8));
            code.push('\n');
        }

        let rules = generator.rules.len();

        code.push_str(&format!(
            "        var rule_fail = new BitSet({});\n",
            rules
        ));
        code.push_str("        var rules = new Rule[]{\n");

        for rule in &generator.rules {
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
        code
    }

    fn print_rules(&self, rules: &[RuleIR]) -> String {
        let mut code = String::new();
        for rule in rules {
            code.push_str(&self.print_rule(rule));
        }
        code
    }

    fn print_rule(&self, rule: &RuleIR) -> String {
        let mut code = String::new();
        let mut indent = 12;
        code.push_str(&format!(
            "    static class {} implements Rule {{\n        @Override\n        public boolean apply() {{\n",
            rule.name
        ));

        for ir in &rule.pattern {
            code.push_str(&self.print_pattern(ir, &mut indent));
        }

        let one_case = rule.cases.len() == 1;
        for node in &rule.cases {
            code.push_str(&self.print_case(node, one_case, &mut indent));
            code.push('\n');
        }

        while indent > 12 {
            indent -= 4;
            code.push_str(&format!("{}}}\n", " ".repeat(indent)));
        }

        code.push_str("            return false;\n        }\n    }\n\n");
        code
    }

    fn print_pattern(&self, ir: &LMNtalIR, indent: &mut usize) -> String {
        let mut code = String::new();
        match ir {
            LMNtalIR::FindAtom { id, name, arity } => {
                code.push_str(&format!(
                    "{}for (var atom_{} : findAtom(\"{}\", {})) {{\n",
                    " ".repeat(*indent),
                    id,
                    name,
                    arity
                ));
                *indent += 4;
            }
            LMNtalIR::GetAtomAtPort { id, .. } => {
                code.push_str(&self.pretty_print(ir, *indent));
                code.push_str(&format!(" if (atom_{} == null) continue;\n", id));
            }
            LMNtalIR::AtomEquality { id_port_list, eq } => {
                if *eq {
                    code.push_str(&format!("{}if (equals(", " ".repeat(*indent)));
                } else {
                    code.push_str(&format!("{}if (!equals(", " ".repeat(*indent)));
                }
                for (i, (id, port)) in id_port_list.iter().enumerate() {
                    if i != 0 {
                        code.push_str(", ");
                    }
                    code.push_str(&format!("atom_{}.at({})", id, port,));
                }

                code.push_str(")) continue;\n");
            }
            _ => {
                code.push_str(&self.pretty_print(ir, *indent));
                code.push('\n');
            }
        }
        code
    }

    fn print_case(&self, case: &Case, one_case: bool, indent: &mut usize) -> String {
        let mut code = String::new();
        if case.condition.is_empty() {
            for ir in &case.definition {
                code.push_str(&self.pretty_print(ir, *indent));
                code.push('\n');
            }

            for ir in &case.body {
                code.push_str(&self.pretty_print(ir, *indent));
                code.push('\n');
            }

            code.push_str(&format!("{}return true;\n", " ".repeat(*indent)));

            return code;
        }
        code.push_str(&format!("{}if (", " ".repeat(*indent)));

        if one_case {
            code.push_str("!(")
        }

        code.push_str(
            &case
                .condition
                .iter()
                .map(|ir| self.pretty_print(ir, 0))
                .collect::<Vec<_>>()
                .join(" && "),
        );

        if one_case {
            code.push_str(&format!("))\n{}continue;\n", " ".repeat(*indent + 2)));
        } else {
            code.push_str(") {\n");
            *indent += 4;
        }

        for ir in &case.definition {
            code.push_str(&self.pretty_print(ir, *indent));
            code.push('\n');
        }

        for ir in &case.body {
            code.push_str(&self.pretty_print(ir, *indent));
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

        code
    }
}

fn print_operation(op: &Operation) -> String {
    match op {
        Operation::Literal(l) => match l {
            Literal::Int(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::Char(c) => c.to_string(),
            Literal::String(s) => format!("std::string(\"{}\")", s),
        },
        Operation::Variable { source, ty_ } => match source {
            crate::ir::VarSource::Head(id, port) | crate::ir::VarSource::Body(id, port) => {
                format!("atom_{}.at({}).get{}()", id, port, atom_type_to_string(ty_))
            }
            crate::ir::VarSource::Definition(id) => {
                format!("atom_{}.get{}()", id, atom_type_to_string(ty_),)
            }
        },
        Operation::BinaryOP { op, lhs, rhs } => {
            format!(
                "({} {} {})",
                print_operation(lhs),
                match op {
                    crate::ir::BinaryOperator::Add => "+",
                    crate::ir::BinaryOperator::Sub => "-",
                    crate::ir::BinaryOperator::Mul => "*",
                    crate::ir::BinaryOperator::Div => "/",
                    crate::ir::BinaryOperator::Mod => "%",
                    crate::ir::BinaryOperator::Eq => "==",
                    crate::ir::BinaryOperator::Lt => "<",
                    crate::ir::BinaryOperator::Le => "<=",
                    crate::ir::BinaryOperator::Gt => ">",
                    crate::ir::BinaryOperator::Ge => ">=",
                    crate::ir::BinaryOperator::Ne => "!=",
                },
                print_operation(rhs)
            )
        }
        Operation::UnaryOP { op, operand } => {
            format!(
                "{}{}",
                match op {
                    crate::ir::UnaryOperator::Neg => "-",
                    crate::ir::UnaryOperator::Not => "!",
                },
                print_operation(operand)
            )
        }
    }
}

fn atom_type_to_string(ty: &ProcessConstraint) -> String {
    match ty {
        ProcessConstraint::Int => "Int".into(),
        ProcessConstraint::Float => "Float".into(),
        ProcessConstraint::Unique => unimplemented!(),
        ProcessConstraint::Ground => unimplemented!(),
        ProcessConstraint::Unary => unimplemented!(),
        ProcessConstraint::String => "String".into(),
        ProcessConstraint::Hyperlink => unimplemented!(),
    }
}
