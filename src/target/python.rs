use std::collections::HashMap;

use crate::{
    codegen::{
        rule::{Case, RuleIR},
        IRSet,
    },
    ir::{LMNtalIR, Literal, Operation},
    model::{guard::ProcessConstraint, Data},
};

use super::Backend;

static HEADER: &[u8] = include_bytes!("../../assets/lib/python/lmntal.py");

pub struct PythonBackend {
    var_type: HashMap<usize, ProcessConstraint>,
    name_map: HashMap<String, usize>,
}

impl Backend for PythonBackend {
    fn new() -> Self {
        Self {
            var_type: HashMap::new(),
            name_map: HashMap::new(),
        }
    }

    fn pretty_print(&mut self, ir_set: &IRSet) -> String {
        let mut code = String::new();
        code.push_str(std::str::from_utf8(HEADER).unwrap());
        code.push_str(&self.print_rules(&ir_set.rules));
        let main = self.print_main(ir_set);
        code.push_str(&self.print_name_map());
        code.push_str(&main);
        code
    }
}

impl PythonBackend {
    fn print_ir(&mut self, ir: &LMNtalIR, indent: usize) -> String {
        let mut code = " ".repeat(indent);
        let fmt = |&source| match source {
            crate::ir::VarSource::Variable(id) => {
                format!("atom_{}, 0", id)
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
                let name = self.get_name(name);
                code.push_str(&format!("atom_{} = create_atom({}, {})", id, name, arity));
                if !data.is_empty() {
                    code.push_str(&format!("\n{}", " ".repeat(indent)));
                    match data {
                        Data::Int(i) => {
                            code.push_str(&format!("atom_{}.set_int({})", id, i));
                        }
                        Data::Float(f) => {
                            code.push_str(&format!("atom_{}.set_float({})", id, f));
                        }
                        Data::Char(c) => {
                            code.push_str(&format!("atom_{}.set_char({})", id, c));
                        }
                        Data::String(s) => {
                            code.push_str(&format!("atom_{}.set_string(\"{}\")", id, s));
                        }
                        Data::Variable(var_id) => {
                            let ty = self.var_type.get(&(*var_id).into()).unwrap();
                            code.push_str(&format!(
                                "atom_{}.set_{}(var_{})",
                                id,
                                atom_type_to_string(ty),
                                var_id
                            ));
                        }
                        _ => {}
                    }
                }
            }
            LMNtalIR::RemoveAtom { id } => {
                code.push_str(&format!("remove_atom(atom_{})", id));
            }
            LMNtalIR::Link { src, dst } => {
                code.push_str(&format!("link({}, {})", fmt(src), fmt(dst),));
            }
            LMNtalIR::Relink { src, src_port, dst } => {
                code.push_str(&format!("relink(atom_{}, {}, {})", src, src_port, fmt(dst)));
            }
            LMNtalIR::CheckType { id, port, ty } => match ty {
                ProcessConstraint::Hyperlink => unreachable!(),
                ProcessConstraint::Unique => unimplemented!(),
                ProcessConstraint::Ground => unimplemented!(),
                ProcessConstraint::Unary => {
                    code.push_str(&format!("atom_{}.at({}).arity() == 1", id, port,));
                }
                _ => {
                    code.push_str(&format!(
                        "atom_{}.at({}).is_{}()",
                        id,
                        port,
                        atom_type_to_string(ty),
                    ));
                }
            },

            LMNtalIR::CheckValue(op) => code.push_str(&print_operation(op)),
            LMNtalIR::DefineTempVar { id, ty, op, .. } => {
                code.push_str(&format!("var_{} = {}", id, print_operation(op)));
                self.var_type.insert(*id, *ty);
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
                let name = self.get_name(name);
                code.push_str(&format!("atom_{} = find_atom({}, {})", id, name, arity));
            }
            LMNtalIR::GetAtomAtPort {
                id,
                from,
                port,
                name,
                arity,
            } => {
                let name = self.get_name(name);
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
                let name = self.get_name(name);
                code.push_str(&format!("hl_{} = create_hyperlink({})", id, name));
            }
            LMNtalIR::LinkToHyperlink { atom, hyperlink } => {
                code.push_str(&format!("hl_{}.add({})", hyperlink.id(), fmt(atom)));
            }
            LMNtalIR::RemoveFromHyperlink { atom, hyperlink } => {
                code.push_str(&format!("hl_{}.remove({})", hyperlink.id(), fmt(atom)));
            }
            LMNtalIR::FuseHyperlink { into, from } => {
                code.push_str(&format!("hl_{}.fuse(hl_{})", into.id(), from.id()));
            }
            _ => unreachable!(),
        }
        code
    }

    fn print_main(&mut self, ir_set: &IRSet) -> String {
        let mut code = String::new();
        code.push_str("def main():\n");

        for node in &ir_set.init {
            code.push_str(&self.print_ir(node, 4));
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
            return code;
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
        let mut indent = 4;
        code.push_str(&format!("def {}() -> bool:\n", rule.name));

        for ir in &rule.pattern {
            code.push_str(&self.print_pattern(ir, &mut indent));
        }

        let one_case = rule.cases.len() == 1;
        for node in &rule.cases {
            code.push_str(&self.print_case(node, one_case, &mut indent));
            code.push('\n');
        }

        code.push_str("    return False\n\n");
        code
    }

    fn print_pattern(&mut self, ir: &LMNtalIR, indent: &mut usize) -> String {
        let mut code = String::new();
        match ir {
            LMNtalIR::FindAtom { id, name, arity } => {
                let name = self.get_name(name);
                code.push_str(&format!(
                    "{}for atom_{} in find_atom({}, {}): \n",
                    " ".repeat(*indent),
                    id,
                    name,
                    arity
                ));
                *indent += 4;
            }
            LMNtalIR::GetAtomAtPort { id, .. } => {
                code.push_str(&self.print_ir(ir, *indent));
                code.push_str(&format!(
                    "\n{}if not atom_{}:\n{}continue\n",
                    " ".repeat(*indent),
                    id,
                    " ".repeat(*indent + 4)
                ));
            }
            LMNtalIR::GetHyperlinkAtPort { id, .. } => {
                code.push_str(&self.print_ir(ir, *indent));
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
                    code.push_str(&format!("atom_{}.at({})", id, port,));
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
                code.push_str(&self.print_ir(ir, *indent));
                code.push('\n');
            }
        }
        code
    }

    fn print_case(&mut self, case: &Case, one_case: bool, indent: &mut usize) -> String {
        let mut code = String::new();
        if case.condition.is_empty() {
            for ir in &case.definition {
                code.push_str(&self.print_ir(ir, *indent));
                code.push('\n');
            }

            for ir in &case.body {
                code.push_str(&self.print_ir(ir, *indent));
                code.push('\n');
            }

            code.push_str(&format!("{}return True\n", " ".repeat(*indent)));

            return code;
        }
        code.push_str(&format!("{}if (", " ".repeat(*indent)));

        if one_case {
            code.push_str("not(")
        }

        code.push_str(
            &case
                .condition
                .iter()
                .map(|ir| self.print_ir(ir, 0))
                .collect::<Vec<_>>()
                .join(" and "),
        );

        if one_case {
            code.push_str(&format!(")):\n{}continue\n", " ".repeat(*indent + 4)));
        } else {
            code.push_str(") :\n");
            *indent += 4;
        }

        for ir in &case.definition {
            code.push_str(&self.print_ir(ir, *indent));
            code.push('\n');
        }

        for ir in &case.body {
            code.push_str(&self.print_ir(ir, *indent));
            code.push('\n');
        }

        if !one_case {
            code.push_str(&format!(
                "{}return True\n{}}}",
                " ".repeat(*indent),
                " ".repeat(*indent - 4)
            ));
            *indent -= 4;
        } else {
            code.push_str(&format!("{}return True\n", " ".repeat(*indent)));
        }

        code
    }

    fn get_name(&mut self, name: &str) -> usize {
        if let Some(id) = self.name_map.get(name) {
            *id
        } else {
            let id = self.name_map.len();
            self.name_map.insert(name.to_string(), id);
            id
        }
    }

    fn print_name_map(&self) -> String {
        let mut code = "name_map = {\n".to_string();
        for (name, id) in &self.name_map {
            code.push_str(&format!("    {}: \"{}\",\n", id, name));
        }
        code.push_str("}\n\n");

        code
    }
}

fn print_operation(op: &Operation) -> String {
    match op {
        Operation::Literal(l) => match l {
            Literal::Int(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::Char(c) => c.to_string(),
            Literal::String(s) => format!("str(\"{}\")", s),
        },
        Operation::Variable { source, ty_ } => match source {
            crate::ir::VarSource::Head(id, port) | crate::ir::VarSource::Body(id, port) => {
                if *ty_ == ProcessConstraint::Hyperlink {
                    format!("get_hyperlink_at_port(atom_{}, {})", id, port)
                } else {
                    format!(
                        "atom_{}.at({}).get_{}()",
                        id,
                        port,
                        atom_type_to_string(ty_)
                    )
                }
            }
            crate::ir::VarSource::Variable(id) => {
                format!("var_{}", id)
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
        Operation::FunctionCall { name, args, ty_ } => {
            if crate::model::guard::RESERVED_FUNC
                .iter()
                .any(|func| func.name == name)
            {
                print_reserved_func(name, args, ty_)
            } else {
                format!("{}({})", name, args.len())
            }
        }
    }
}

fn print_reserved_func(name: &str, args: &[Operation], _type: &ProcessConstraint) -> String {
    match name {
        "num" => format!("{}.arity()", print_operation(&args[0])),
        _ => unimplemented!(),
    }
}

fn atom_type_to_string(ty: &ProcessConstraint) -> String {
    match ty {
        ProcessConstraint::Int => "int".into(),
        ProcessConstraint::Float => "float".into(),
        ProcessConstraint::Unique => unimplemented!(),
        ProcessConstraint::Ground => unimplemented!(),
        ProcessConstraint::Unary => unimplemented!(),
        ProcessConstraint::String => "str".into(),
        ProcessConstraint::Hyperlink => unimplemented!(),
    }
}
