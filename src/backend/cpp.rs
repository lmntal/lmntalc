use crate::{
    data::{guard::ProcessConstraint, Data},
    generator::{
        rule::{Case, RuleIR},
        Generator,
    },
    ir::{LMNtalIR, Literal, Operation},
};

use super::Backend;

static LMNTAL: &[u8] = include_bytes!("../../assets/lib/cpp/lmntal.hpp");

pub struct CppBackend;

impl Backend for CppBackend {
    fn new() -> Self {
        Self
    }
    fn pretty_print(&mut self, generator: &Generator) -> String {
        let mut code = String::new();
        code.push_str(std::str::from_utf8(LMNTAL).unwrap());
        code.push_str(&self.print_rules(&generator.rules));
        code.push_str(&self.print_main(generator));
        code
    }
}

impl CppBackend {
    fn pretty_print(&mut self, ir: &LMNtalIR, indent: usize) -> String {
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
                    "auto atom_{} = create_atom(\"{}\", {});",
                    id, name, arity
                ));
                match data {
                    Data::Empty => {}
                    Data::Int(i) => {
                        code.push_str(&format!("\n{}", " ".repeat(indent)));
                        code.push_str(&format!("atom_{}->set_int({});", id, i));
                    }
                    Data::Float(f) => {
                        code.push_str(&format!("\n{}", " ".repeat(indent)));
                        code.push_str(&format!("atom_{}->set_float({});", id, f));
                    }
                    Data::Char(c) => {
                        code.push_str(&format!("\n{}", " ".repeat(indent)));
                        code.push_str(&format!("atom_{}->set_char({});", id, c));
                    }
                    Data::String(s) => {
                        code.push_str(&format!("\n{}", " ".repeat(indent)));
                        code.push_str(&format!("atom_{}->set_string(\"{}\");", id, s));
                    }
                }
            }
            LMNtalIR::RemoveAtom { id } => {
                code.push_str(&format!("remove_atom(atom_{});", id));
            }
            LMNtalIR::Link { src, dst } => {
                code.push_str(&format!("link({}, {});", fmt(src), fmt(dst)));
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
                    "atom_{}->at({})->is_{}()",
                    id,
                    port,
                    atom_type_to_string(ty),
                ));
            }
            LMNtalIR::CheckValue(op) => code.push_str(&print_operation(op)),
            LMNtalIR::DefineTempVar { id, name, ty, op } => {
                let name = match ty {
                    ProcessConstraint::Int => "_int".to_string(),
                    ProcessConstraint::Float => "_float".to_string(),
                    ProcessConstraint::String => "_string".to_string(),
                    _ => name.clone(),
                };
                code.push_str(&format!(
                    "auto temp_{} = create_atom(\"{}\", 1);\n",
                    id, name
                ));
                code.push_str(&format!(
                    "{}temp_{}->set_{}({});",
                    " ".repeat(indent),
                    id,
                    atom_type_to_string(ty),
                    print_operation(op)
                ));
            }
            LMNtalIR::CloneAtom { id, from } => {
                code.push_str(&format!("auto atom_{} = clone_atom(atom_{});", id, from));
            }
            LMNtalIR::FindAtom { id, name, arity } => {
                code.push_str(&format!(
                    "auto atom_{} = find_atom(\"{}\", {});",
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
                    "auto atom_{} = get_atom_at_port(atom_{}, {}, \"{}\", {});",
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
                code.push_str(&format!("auto hl_{} = create_hyperlink(\"{}\");", id, name));
            }
            LMNtalIR::LinkToHyperlink { atom, hyperlink } => {
                code.push_str(&format!("hl_{}->add({});", hyperlink.id(), fmt(atom)));
            }
            LMNtalIR::RemoveFromHyperlink { atom, hyperlink } => {
                code.push_str(&format!("hl_{}->remove({});", hyperlink.id(), fmt(atom)));
            }
            LMNtalIR::FuseHyperlink { into, from } => {
                code.push_str(&format!("hl_{}->fuse(hl_{});", into.id(), from.id()));
                code.push_str(&format!("remove_hyperlink(hl_{});", from));
            }
            _ => unreachable!(),
        }
        code
    }

    fn print_main(&mut self, generator: &Generator) -> String {
        let mut code = String::new();
        code.push_str("int main() {\n");
        for node in &generator.init {
            code.push_str(&self.pretty_print(node, 2));
            code.push('\n');
        }

        let rules = generator.rules.len();

        code.push_str(&format!("  std::bitset<{}> rule_fail;\n", rules));
        code.push_str(&format!(
            "  constexpr rule rules[{}] = {{\n",
            generator.rules.len()
        ));

        for rule in &generator.rules {
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
        code
    }

    fn print_rules(&mut self, rules: &[RuleIR]) -> String {
        let mut code = String::new();
        for rule in rules {
            code.push_str(&self.print_rule(rule));
        }
        code
    }

    fn print_rule(&mut self, rule: &RuleIR) -> String {
        let mut code = String::new();
        let mut indent = 2;
        code.push_str(&format!("bool {}() {{\n", rule.name));

        for ir in &rule.pattern {
            code.push_str(&self.print_pattern(ir, &mut indent));
        }

        let one_case = rule.cases.len() == 1;
        for node in &rule.cases {
            code.push_str(&self.print_case(node, one_case, &mut indent));
            code.push('\n');
        }

        while indent > 2 {
            indent -= 2;
            code.push_str(&format!("{}}}\n", " ".repeat(indent)));
        }

        code.push_str("  return false;\n}\n\n");
        code
    }

    fn print_pattern(&mut self, ir: &LMNtalIR, indent: &mut usize) -> String {
        let mut code = String::new();
        match ir {
            LMNtalIR::FindAtom { id, name, arity } => {
                code.push_str(&format!(
                    "{}for (auto atom_{} : find_atom(\"{}\", {})) {{\n",
                    " ".repeat(*indent),
                    id,
                    name,
                    arity
                ));
                *indent += 2;
            }
            LMNtalIR::GetAtomAtPort { id, .. } => {
                code.push_str(&self.pretty_print(ir, *indent));
                code.push_str(&format!(" if (!atom_{}) continue;\n", id));
            }
            LMNtalIR::GetHyperlinkAtPort { id, .. } => {
                code.push_str(&self.pretty_print(ir, *indent));
                code.push_str(&format!(" if (!hl_{}) continue;\n", id));
            }
            LMNtalIR::AtomEquality { id_port_list, eq } => {
                if *eq {
                    code.push_str(&format!("{}if (!equals(", " ".repeat(*indent)));
                } else {
                    code.push_str(&format!("{}if (equals(", " ".repeat(*indent)));
                }
                for (i, (id, port)) in id_port_list.iter().enumerate() {
                    if i != 0 {
                        code.push_str(", ");
                    }
                    code.push_str(&format!("atom_{}->at({})", id, port,));
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

    fn print_case(&mut self, case: &Case, one_case: bool, indent: &mut usize) -> String {
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

            code.push_str(&format!("{}return true;", " ".repeat(*indent)));

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
            *indent += 2;
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
                " ".repeat(*indent - 2)
            ));
            *indent -= 2;
        } else {
            code.push_str(&format!("{}return true;", " ".repeat(*indent)));
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
                format!(
                    "atom_{}->at({})->get_{}()",
                    id,
                    port,
                    atom_type_to_string(ty_)
                )
            }
            crate::ir::VarSource::Definition(id) => {
                format!("temp_{}->get_{}()", id, atom_type_to_string(ty_),)
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
        ProcessConstraint::Int => "int".into(),
        ProcessConstraint::Float => "float".into(),
        ProcessConstraint::Unique => unimplemented!(),
        ProcessConstraint::Ground => unimplemented!(),
        ProcessConstraint::Unary => unimplemented!(),
        ProcessConstraint::String => "string".into(),
        ProcessConstraint::Hyperlink => "hlink".into(),
    }
}
