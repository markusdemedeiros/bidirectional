#![allow(unused)]
#![feature(box_patterns)]

mod stlcb;
use stlcb::*;

use core::panic;
use std::{collections::BTreeMap, default, fmt, ops::Deref, rc::Rc};

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
    // println!("{:?}", stlcb_term!(lambda x . x));
    println!("{:?}", stlcb_term!(False ~ True));
    println!("{:?}", stlcb_term!((lambda x . x) ~ True));
    println!("{:?}", stlcb_term!((lambda x . x) ~ (lambda z. True)));
    println!(
        "{:?}",
        stlcb_term!((lambda b . if b then False else True) : (Bool -> Bool))
    );
    typecheck(
        &stlcb_term!((lambda b . if b then False else True) : (Bool -> Bool)),
        stlcb_type!(Bool -> Bool),
    );
    typecheck(&stlcb_term!((lambda f . lambda g . lambda b . (g ~ (f ~ b))) : ((Bool -> Bool) -> (Bool -> Bool) -> Bool -> Bool)),
                stlcb_type!(((Bool -> Bool) -> (Bool -> Bool) -> Bool -> Bool)));
    println!("All tests passed!");
}
