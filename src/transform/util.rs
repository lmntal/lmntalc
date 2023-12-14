use crate::data::{guard::Operator, Data};

pub(super) enum AtomType {
    Plain,
    Char(char),
    Int(i64),
    Float(f64),
    Operator(Operator),
}

pub(super) fn solve_name(name: &str) -> AtomType {
    if name.starts_with('\'') && name.ends_with('\'') {
        let core = &name[1..name.len() - 1];
        // check if it's a escape character
        if core.starts_with('\\') && core.len() == 2 {
            let core = &core[1..];
            return AtomType::Char(core.chars().next().unwrap());
        }
        // check if it's a char
        if core.len() == 1 {
            return AtomType::Char(core.chars().next().unwrap());
        }
    }
    if let Ok(int) = name.parse::<i64>() {
        return AtomType::Int(int);
    }
    if let Ok(float) = name.parse::<f64>() {
        return AtomType::Float(float);
    }
    if let Ok(op) = name.parse::<Operator>() {
        return AtomType::Operator(op);
    }
    AtomType::Plain
}

pub(super) fn name_dispatch(name: &str) -> (String, Data) {
    if name.starts_with('\'') && name.ends_with('\'') {
        let core = &name[1..name.len() - 1];
        // check if it's a escape character
        if core.starts_with('\\') && core.len() == 2 {
            let core = &core[1..];
            return (
                String::from("atom"),
                Data::Char(core.chars().next().unwrap()),
            );
        }
        // check if it's a char
        if core.len() == 1 {
            return (
                String::from("char"),
                Data::Char(core.chars().next().unwrap()),
            );
        }
    }
    if let Ok(int) = name.parse::<i64>() {
        return (String::from("int"), Data::Int(int));
    }
    if let Ok(float) = name.parse::<f64>() {
        return (String::from("float"), Data::Float(float));
    }
    (name.to_string(), Data::Empty)
}
