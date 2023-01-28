use std::rc::Rc;

use crate::{stlcb_term, stlcb_type};

use super::{STLCBTerm, STLCBType, TyCtx};

#[macro_use(stlcb_term)]
#[macro_use(stlcb_type)]

pub type STLCBProofIdent = usize;

pub(crate) enum STLCBInference {
    /// Forward rules
    BTVar(Rc<TyCtx>, String, Rc<STLCBType>),
    BTTrue(Rc<TyCtx>),
    BTFalse(Rc<TyCtx>),
    BTAnn(Rc<TyCtx>, Rc<STLCBTerm>, Rc<STLCBType>, STLCBProofIdent),
    BTApp(
        Rc<TyCtx>,
        Rc<STLCBTerm>,
        Rc<STLCBTerm>,
        Rc<STLCBType>,
        Rc<STLCBType>,
        STLCBProofIdent,
        STLCBProofIdent,
    ),

    // Reverse rules
    BTCheckInfer(Rc<TyCtx>, Rc<STLCBTerm>, Rc<STLCBType>, STLCBProofIdent),
    BTIf(
        Rc<TyCtx>,
        Rc<STLCBTerm>,
        Rc<STLCBTerm>,
        Rc<STLCBTerm>,
        Rc<STLCBType>,
        STLCBProofIdent,
        STLCBProofIdent,
        STLCBProofIdent,
    ),
    BTAbs(
        Rc<TyCtx>,
        String,
        Rc<STLCBTerm>,
        Rc<STLCBType>,
        Rc<STLCBType>,
        STLCBProofIdent,
    ),
}

#[derive(Default)]
pub struct STLCBProof {
    steps: Vec<Box<STLCBInference>>,
}

impl STLCBInference {
    pub fn show_kind(&self) -> String {
        match self {
            STLCBInference::BTVar(_, _, _) => "BT-Var",
            STLCBInference::BTTrue(_) => "BT-True",
            STLCBInference::BTFalse(_) => "BT-False",
            STLCBInference::BTAnn(_, _, _, _) => "BT-Ann",
            STLCBInference::BTApp(_, _, _, _, _, _, _) => "BT-App",
            STLCBInference::BTCheckInfer(_, _, _, _) => "BT-CheckInfer",
            STLCBInference::BTIf(_, _, _, _, _, _, _, _) => "BT-If",
            STLCBInference::BTAbs(_, _, _, _, _, _) => "BT-Abs",
        }
        .to_string()
    }

    // fixme: refactor into list type
    pub fn ctx(&self) -> &TyCtx {
        match self {
            STLCBInference::BTVar(c, _, _) => c,
            STLCBInference::BTTrue(c) => c,
            STLCBInference::BTFalse(c) => c,
            STLCBInference::BTAnn(c, _, _, _) => c,
            STLCBInference::BTApp(c, _, _, _, _, _, _) => c,
            STLCBInference::BTCheckInfer(c, _, _, _) => c,
            STLCBInference::BTIf(c, _, _, _, _, _, _, _) => c,
            STLCBInference::BTAbs(c, _, _, _, _, _) => c,
        }
    }

    pub fn show_step(&self) {
        match self {
            STLCBInference::BTVar(_, v, ty) => {
                println!("\t  ({}:{:?}) \u{2208} \u{0393} ", v, ty);
                println!("\t{}", "\u{2015}".repeat(70));
                println!("\t  \u{0393} \u{22A2} {:?} \u{21D2}  {:?} ", v, ty);
            }
            STLCBInference::BTTrue(_) => {
                println!();
                println!("\t{}", "\u{2015}".repeat(70));
                println!(
                    "\t  \u{0393} \u{22A2} {:?} \u{21D0}  {:?} ",
                    stlcb_term!(True),
                    stlcb_type!(Bool)
                );
            }
            STLCBInference::BTFalse(_) => {
                println!();
                println!("\t{}", "\u{2015}".repeat(70));
                println!(
                    "\t  \u{0393} \u{22A2} {:?} \u{21D0}  {:?} ",
                    stlcb_term!(False),
                    stlcb_type!(Bool)
                );
            }
            STLCBInference::BTAnn(_, term, typ, pf) => {
                println!(
                    "  (#{:?}):\t  \u{0393} \u{22A2} {:?} \u{21D0}  {:?} ",
                    pf, term, typ,
                );
                println!("\t{}", "\u{2015}".repeat(70));
                println!(
                    "\t  \u{0393} \u{22A2} {:?} \u{21D2}  {:?} ",
                    STLCBTerm::Ann((*term).clone(), (*typ).clone()),
                    typ,
                );
            }
            STLCBInference::BTApp(_, rator, rand, ty_rand, ty_res, pf_rator, pf_rand) => {
                println!(
                    "  (#{:?}):\t  \u{0393} \u{22A2} {:?} \u{21D2}  {:?} ",
                    pf_rator,
                    rator,
                    STLCBType::Function((*ty_rand).clone(), (*ty_res).clone())
                );
                println!("\t{}", "\u{2015}".repeat(70));
                println!(
                    "\t  \u{0393} \u{22A2} {:?} \u{21D0}  {:?} ",
                    STLCBTerm::App((*rator).clone(), (*rand).clone()),
                    ty_rand
                );
            }
            STLCBInference::BTCheckInfer(_, term, typ, witness) => {
                println!(
                    "  (#{:?}):\t  \u{0393} \u{22A2} {:?} \u{21D2}  {:?} ",
                    witness, term, typ,
                );
                println!("\t{}", "\u{2015}".repeat(70));
                println!("\t  \u{0393} \u{22A2} {:?} \u{21D0}  {:?} ", term, typ);
            }
            STLCBInference::BTIf(_, cond, ct, cf, typ, pfc, pft, pff) => {
                println!(
                    "  (#{:?}):\t  \u{0393} \u{22A2} {:?} \u{21D0}  {:?} ",
                    pfc,
                    cond,
                    stlcb_type!(Bool),
                );
                println!(
                    "  (#{:?}):\t  \u{0393} \u{22A2} {:?} \u{21D0}  {:?} ",
                    pft, ct, typ,
                );
                println!(
                    "  (#{:?}):\t  \u{0393} \u{22A2} {:?} \u{21D0}  {:?} ",
                    pff, cf, typ,
                );
                println!("\t{}", "\u{2015}".repeat(70));
                println!(
                    "\t  \u{0393} \u{22A2} {:?} \u{21D0}  {:?} ",
                    stlcb_term!(if {(**cond).to_owned()} then {(**ct).to_owned()} else {(**cf).to_owned()}),
                    typ
                );
            }
            STLCBInference::BTAbs(_, var, body, var_ty, body_ty, body_pf) => {
                println!(
                    "  (#{:?}):\t  \u{0393}, ({} : {:?}) \u{22A2} {:?} \u{21D0}  {:?} ",
                    body_pf, var, var_ty, body, body_ty,
                );
                println!("\t{}", "\u{2015}".repeat(70));
                println!(
                    "\t  \u{0393} \u{22A2} {:?} \u{21D0}  {:?} ",
                    STLCBTerm::Lam((*var).clone(), (*body).clone()),
                    STLCBType::Function((*var_ty).clone(), (*body_ty).clone())
                );
            }
        }
    }
}

impl STLCBProof {
    pub(crate) fn step(&mut self, inf: STLCBInference) -> usize {
        self.steps.push(Box::new(inf));
        self.steps.len() - 1
    }

    pub(crate) fn trace(&self) {
        for (ix, box s) in self.steps.iter().enumerate() {
            println!(
                "[#{:?}]: {:<8}\t \u{0393} = {:?}",
                ix,
                s.show_kind(),
                s.ctx()
            );
            s.show_step();
            println!();
        }
    }
}
