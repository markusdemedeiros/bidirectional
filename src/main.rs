#![allow(unused)]
#![feature(box_patterns)]

mod stlcb;
use stlcb::*;

use core::panic;
use std::{collections::BTreeMap, default, fmt, ops::Deref, rc::Rc};

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub(crate) parse_stlcb);

/// Typechecks a judgment of the form
///     |- term => ty
fn typecheck(term: &STLCBTerm, ty: STLCBType) {
    let mut proof = STLCBProof::default();
    let check = term.infer(Rc::new(TyCtx::default()), &mut proof);
    assert_eq!(
        check.unwrap().0,
        ty,
        "type can be checked, but not to that!"
    );
    println!("[typecheck: \u{22A2} {:?} \u{21D2}  {:?}]", term, ty);
    proof.trace();
    println!();
}

fn main() {
    println!("Starting tests...");
    assert!(parse_stlcb::TermParser::new().parse("False").is_ok());
    assert!(parse_stlcb::TermParser::new().parse("True").is_ok());
    // assert!(parse_stlcb::TermParser::new().parse("(  (True ) )").is_ok());
    assert!(parse_stlcb::TermParser::new().parse("True False").is_ok());
    assert!(parse_stlcb::TermParser::new()
        .parse("(if v then False else True)")
        .is_ok());
    assert!(parse_stlcb::TermParser::new()
        .parse("((if v then False else True) : Bool)")
        .is_ok());
    assert!(parse_stlcb::TermParser::new()
        .parse("True False False")
        .is_ok());
    // assert!(parse_stlcb::TermParser::new().parse("lambda x. x").is_ok());
    // assert!(parse_stlcb::TermParser::new()
    //     .parse("lambda x. lol69")
    //     .is_ok());
    assert!(parse_stlcb::TermParser::new()
        .parse("(lambda x. x) False")
        .is_ok());

    assert!(parse_stlcb::TermParser::new()
        .parse("(lambda x. (lambda y. x)) False")
        .is_ok());

    assert!(parse_stlcb::TermParser::new()
        .parse("(lambda x. False) (lambda y. y) False")
        .is_ok());
    // println!("{:?}", stlcb_term!(False ~ True));
    // println!("{:?}", stlcb_term!((lambda x . x) ~ True));
    // println!("{:?}", stlcb_term!((lambda x . x) ~ (lambda z. True)));
    // println!(
    //     "{:?}",
    //     stlcb_term!((lambda b . if b then False else True) : (Bool -> Bool))
    // );
    // typecheck(
    //     &stlcb_term!((lambda b . if b then False else True) : (Bool -> Bool)),
    //     stlcb_type!(Bool -> Bool),
    // );
    // typecheck(&stlcb_term!((lambda f . lambda g . lambda b . (g ~ (f ~ b))) : ((Bool -> Bool) -> (Bool -> Bool) -> Bool -> Bool)),
    //             stlcb_type!(((Bool -> Bool) -> (Bool -> Bool) -> Bool -> Bool)));
    println!("All tests passed!");
}
