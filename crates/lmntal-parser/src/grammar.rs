mod process;
mod expr;

use crate::{
    parser::{CompletedMarker, Marker, Parser},
    syntax_kind::SyntaxKind::{self, World, EOF},
};

pub fn world(p: &mut Parser<'_>) {
    let root = p.start();
    while !p.at(EOF) {
        world_item(p, false);
    }
    root.complete(p, World);
}

fn world_item(p: &mut Parser<'_>, end_at_rcurly: bool) {
    loop {
        let possible_start_of_rule = p.start();
        let raw_pl = process_list_r(p);
        match p.current() {
            SyntaxKind::ColonDash => {
                // It is a rule head
                raw_pl.complete(p, SyntaxKind::RuleHead);
                rule(p, possible_start_of_rule);
            }
            SyntaxKind::Dot => {
                // Meets the end of the process list
                p.bump(SyntaxKind::Dot);
                possible_start_of_rule.abandon(p);
                raw_pl.complete(p, SyntaxKind::ProcessList);
            }
            EOF => {
                possible_start_of_rule.abandon(p);
                break;
            }
            SyntaxKind::RCurly if end_at_rcurly => {
                possible_start_of_rule.abandon(p);
                break;
            }
            _ => unreachable!(),
        }
    }
}

/// Parse a process list, without consuming the dot
fn process_list(p: &mut Parser<'_>) -> CompletedMarker {
    process_list_r(p).complete(p, SyntaxKind::ProcessList)
}

/// Parse a process list, leaving the kind of the list to the caller.
///
/// It is used when the kind of the list is unknown at the time of parsing.
fn process_list_r(p: &mut Parser<'_>) -> Marker {
    let m = p.start();
    loop {
        process::process(p);
        if p.at(SyntaxKind::Comma) {
            p.bump(SyntaxKind::Comma);
            // Allow trailing comma
        }
        let current = p.current();
        if current == SyntaxKind::Dot
            || current == SyntaxKind::ColonDash
            || current == SyntaxKind::VerticalBar
            || current == EOF
        {
            break;
        }
    }
    m
}

fn rule(p: &mut Parser<'_>, m: Marker) {
    // We have already consumed the head of the rule
    p.bump(SyntaxKind::ColonDash);

    let guard_or_body = process_list_r(p);

    match p.current() {
        SyntaxKind::Dot => {
            // It is a body
            guard_or_body.complete(p, SyntaxKind::RuleBody);
        }
        SyntaxKind::VerticalBar => {
            // It is a body
            guard_or_body.complete(p, SyntaxKind::Guard);
            p.bump(SyntaxKind::VerticalBar);
            process_list_r(p).complete(p, SyntaxKind::RuleBody);
        }
        _ => unreachable!(),
    }
    p.bump(SyntaxKind::Dot);
    m.complete(p, SyntaxKind::Rule);
}
