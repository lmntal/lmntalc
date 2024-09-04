use expect_test::{expect, Expect};
use std::fmt::Write;

use super::*;

fn check_lexing(src: &str, expect: Expect) {
    let actual: String = tokenize(src).fold(String::new(), |mut acc, token| {
        _ = writeln!(acc, "{:?}", token);
        acc
    });
    expect.assert_eq(&actual)
}

#[test]
fn basic_test() {
    check_lexing(
        "a(b), 'a'(b).",
        expect![[r#"
        Token { kind: Ident, len: 1 }
        Token { kind: OpenParen, len: 1 }
        Token { kind: Ident, len: 1 }
        Token { kind: CloseParen, len: 1 }
        Token { kind: Comma, len: 1 }
        Token { kind: Whitespace, len: 1 }
        Token { kind: Literal { kind: Char { terminated: true } }, len: 3 }
        Token { kind: OpenParen, len: 1 }
        Token { kind: Ident, len: 1 }
        Token { kind: CloseParen, len: 1 }
        Token { kind: Dot, len: 1 }
     "#]],
    )
}

#[test]
fn guard_test() {
    check_lexing(
        "a =:= 2, b =\\= 3 |",
        expect![[r#"
        Token { kind: Ident, len: 1 }
        Token { kind: Whitespace, len: 1 }
        Token { kind: Eq, len: 1 }
        Token { kind: Colon, len: 1 }
        Token { kind: Eq, len: 1 }
        Token { kind: Whitespace, len: 1 }
        Token { kind: Literal { kind: Int { base: Decimal, empty_int: false } }, len: 1 }
        Token { kind: Comma, len: 1 }
        Token { kind: Whitespace, len: 1 }
        Token { kind: Ident, len: 1 }
        Token { kind: Whitespace, len: 1 }
        Token { kind: Eq, len: 1 }
        Token { kind: Backslash, len: 1 }
        Token { kind: Eq, len: 1 }
        Token { kind: Whitespace, len: 1 }
        Token { kind: Literal { kind: Int { base: Decimal, empty_int: false } }, len: 1 }
        Token { kind: Whitespace, len: 1 }
        Token { kind: Or, len: 1 }
     "#]],
    )
}

#[test]
fn smoke_test() {
    check_lexing(
        "/* my source file */ a{X=b, c+2}, e(X).\na:-b.\n",
        expect![[r#"
            Token { kind: BlockComment { terminated: true }, len: 20 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 1 }
            Token { kind: OpenBrace, len: 1 }
            Token { kind: Ident, len: 1 }
            Token { kind: Eq, len: 1 }
            Token { kind: Ident, len: 1 }
            Token { kind: Comma, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 1 }
            Token { kind: Plus, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false } }, len: 1 }
            Token { kind: CloseBrace, len: 1 }
            Token { kind: Comma, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 1 }
            Token { kind: OpenParen, len: 1 }
            Token { kind: Ident, len: 1 }
            Token { kind: CloseParen, len: 1 }
            Token { kind: Dot, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 1 }
            Token { kind: Colon, len: 1 }
            Token { kind: Minus, len: 1 }
            Token { kind: Ident, len: 1 }
            Token { kind: Dot, len: 1 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    )
}

#[test]
fn comment_flavors() {
    check_lexing(
        r"
// line
//// line as well
/// outer doc line
//! inner doc line
/* block */
/**/
/*** also block */
/** outer doc block */
/*! inner doc block */
",
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: LineComment, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: LineComment, len: 17 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: LineComment, len: 18 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: LineComment, len: 18 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 11 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 4 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 18 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 22 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 22 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    )
}

#[test]
fn nested_block_comments() {
    check_lexing(
        "/* /* */ */'a'",
        expect![[r#"
            Token { kind: BlockComment { terminated: true }, len: 11 }
            Token { kind: Literal { kind: Char { terminated: true } }, len: 3 }
        "#]],
    )
}

#[test]
fn characters() {
    check_lexing(
        "'a' ' ' '\\n'",
        expect![[r#"
            Token { kind: Literal { kind: Char { terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Char { terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Char { terminated: true } }, len: 4 }
        "#]],
    );
}
