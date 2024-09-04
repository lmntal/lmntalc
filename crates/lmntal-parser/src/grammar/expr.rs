use crate::{
    parser::{CompletedMarker, Marker, Parser},
    syntax_kind::SyntaxKind,
    token_set::TokenSet,
    T,
};

use super::process::{hyperlink, link, membrane};

enum Associativity {
    Left,
    Right,
}

/// Binding powers of operators for a Pratt parser.
///
/// See <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
///
/// Note that Rust doesn't define associativity for some infix operators (e.g. `==` and `..`) and
/// requires parentheses to disambiguate. We just treat them as left associative.
#[rustfmt::skip]
fn current_op(p: &Parser<'_>) -> (u8, SyntaxKind, Associativity) {
    use Associativity::*;
    const NOT_AN_OP: (u8, SyntaxKind, Associativity) = (0, T![@], Left);
    match p.current() {
        T![or]                 => (10, T![or],  Left),
        T![and]                => (11, T![and], Left),
        T![xor]                => (10, T![xor], Left),
        T![ash]                => (11, T![ash], Left),
        T![lsh]                => (11, T![lsh], Left),

        T![>] if p.at(T![>+<]) => (9,  T![>+<], Left),
        T![>] if p.at(T![>*<]) => (9,  T![>*<], Left),
        T![>] if p.at(T![><])  => (9,  T![><],  Left),
        T![>] if p.at(T![>=.]) => (1,  T![>=.], Right),
        T![>] if p.at(T![>=])  => (5,  T![>=],  Right),
        T![>] if p.at(T![>>])  => (9,  T![>>],  Left),
        T![>] if p.at(T![>.])  => (5,  T![>.],  Right),
        T![>]                  => (5,  T![>],   Right),

        T![<] if p.at(T![<=.]) => (1,  T![<=.], Right),
        T![<] if p.at(T![<=])  => (5,  T![<=],  Right),
        T![<] if p.at(T![<.])  => (5,  T![<.],  Right),
        T![<] if p.at(T![<<])  => (9,  T![<<],  Left),
        T![<]                  => (5,  T![<],   Right),

        T![=] if p.at(T!["=\\=."])  => (1,  T!["=\\=."],Right),
        T![=] if p.at(T!["=\\="])   => (5,  T!["=\\="], Right),
        T![=] if p.at(T![=:=.])     => (1,  T![=:=.],   Right),
        T![=] if p.at(T![=:=])      => (5,  T![=:=],    Right),
        T![=] if p.at(T![===])      => (5,  T![===],    Right),
        T![=] if p.at(T![==])       => (5,  T![==],     Right),
        T![=]                       => (5,  T![=],      Right),

        T![+] if p.at(T![+.])   => (10, T![+.],  Left),
        T![+]                   => (10, T![+],   Left),
        T![-] if p.at(T![-.])   => (10, T![-.],  Left),
        T![-]                   => (10, T![-],   Left),
        T![*] if p.at(T![*.])   => (11, T![*.],  Left),
        T![*] if p.at(T![**])   => (12, T![**],  Left),
        T![*]                   => (11, T![*],   Left),
        T![/] if p.at(T![/.])   => (11, T![/.],  Left),
        T![/]                   => (11, T![/],   Left),
        T![^]                   => (13,  T![^],  Left),
        T![mod]                 => (12, T![mod], Left),
        
        _                       => NOT_AN_OP
    }
}

pub(crate) fn expr_bp(
    p: &mut Parser<'_>,
    m: Option<Marker>,
    min_bp: u8,
) -> Option<CompletedMarker> {
    let m = m.unwrap_or_else(|| p.start());
    let mut lhs = match lhs(p) {
        Some(cm) => cm.extend_to(p, m),
        None => {
            m.abandon(p);
            return None;
        }
    };

    loop {
        let (op_bp, op, associativity) = current_op(p);
        if op_bp < min_bp {
            break;
        }
        let m = lhs.precede(p);
        p.bump(op);

        let op_bp = match associativity {
            Associativity::Left => op_bp + 1,
            Associativity::Right => op_bp,
        };

        expr_bp(p, None, op_bp);
        lhs = m.complete(p, SyntaxKind::Atom);
    }

    Some(lhs)
}

fn lhs(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = p.start();
    match p.current() {
        T![+] if p.at(T![+.]) => {
            p.bump(T![+.]);
        }
        T![-] if p.at(T![-.]) => {
            p.bump(T![-.]);
        }
        T![+] | T![-] => {
            p.bump_any();
        }
        _ => {
            let lhs = atom_expr(p)?;
            return Some(lhs);
        }
    }

    expr_bp(p, None, u8::MAX);
    let cm = m.complete(p, SyntaxKind::Atom);
    Some(cm)
}

fn atom_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    todo!()
}

#[test]
fn test_parse() {
    let source = r#"a + b * c"#;
}