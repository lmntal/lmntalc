use crate::{
    parser::{CompletedMarker, Marker, Parser},
    syntax_kind::SyntaxKind,
};

use super::{expr::expr_bp, world_item};

pub(crate) fn process(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    match p.current() {
        kind if kind.is_operator() => {
            p.bump_any();
            atom(p, m)
        }
        _ => {
            todo!()
        }
    }
}

fn atom(p: &mut Parser<'_>, m: Marker) -> CompletedMarker {
    let next = p.nth(1);
    if next == SyntaxKind::Colon {
        todo!()
    } else {
        expr_bp(p, None, 0).unwrap()
    }
}

fn unit_atom(p: &mut Parser<'_>, m: Marker) -> CompletedMarker {
    match p.current() {
        SyntaxKind::LinkName => link(p, m),
        SyntaxKind::Bang => hyperlink(p, m),
        SyntaxKind::LCurly => membrane(p, m),
        _ => {
            p.error("Expected an atom");
            m.abandon(p);
            p.start().complete(p, SyntaxKind::Error)
        }
    }
}

pub(crate) fn hyperlink(p: &mut Parser<'_>, m: Marker) -> CompletedMarker {
    p.bump(SyntaxKind::Bang);
    p.bump(SyntaxKind::LinkName);
    m.complete(p, SyntaxKind::Hyperlink)
}

pub(crate) fn link(p: &mut Parser<'_>, m: Marker) -> CompletedMarker {
    p.bump(SyntaxKind::LinkName);
    m.complete(p, SyntaxKind::Link)
}

pub(crate) fn membrane(p: &mut Parser<'_>, m: Marker) -> CompletedMarker {
    p.bump(SyntaxKind::LCurly);
    world_item(p, true);
    p.bump(SyntaxKind::RCurly);
    m.complete(p, SyntaxKind::Membrane)
}
