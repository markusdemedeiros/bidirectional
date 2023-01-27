#![allow(unused)]
#![feature(box_patterns)]

use core::panic;
use std::{collections::BTreeMap, default, fmt, ops::Deref, rc::Rc};

#[derive(PartialEq, Eq, Clone)]
enum STLCBTerm {
    Var(String),
    App(Rc<STLCBTerm>, Rc<STLCBTerm>),
    Lam(String, Rc<STLCBTerm>),
    Ann(Rc<STLCBTerm>, Rc<STLCBType>),
    True,
    False,
    If(Rc<STLCBTerm>, Rc<STLCBTerm>, Rc<STLCBTerm>),
}

#[derive(PartialEq, Eq, Clone)]
enum STLCBType {
    Bool,
    Function(Rc<STLCBType>, Rc<STLCBType>),
}

#[derive(Eq, PartialEq, Default, Clone)]
struct TyCtx {
    variables: BTreeMap<String, Rc<STLCBType>>,
}

type STLCBProofIdent = usize;

enum STLCBInference {
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

macro_rules! stlcb_type {
    (($($rest:tt)+)) => {
        // Brackets bind tightest
        stlcb_type!($($rest)*)
    };
    ($first:tt -> $($rest:tt)+) => {
        {
            // Right-associative arrows
            let t0 = stlcb_type!($first);
            let t1 = stlcb_type!($($rest)*);
            STLCBType::Function(Rc::new(t0), Rc::new(t1))
        }
    };
    (Bool) => {
        // Bool is the only base type
        STLCBType::Bool
    };
    ({$ex:expr}) => {
        // Curly parens escape expressions
        $ex
    };
}

macro_rules! stlcb_term {
    ($first:tt ~ $($rest:tt)+) => {
        {
            let rator = stlcb_term!($first);
            let rand = stlcb_term!($($rest)+);
            STLCBTerm::App(Rc::new(rator), Rc::new(rand))
        }
    };
    (lambda $var:ident . $($body:tt)+) => {
        {
            // Lambda terms must be enclosed in brackets
            let body_parsed = stlcb_term!($($body)+);
            STLCBTerm::Lam(stringify!($var).to_string(), Rc::new(body_parsed))
        }
    };
    (($($rest:tt)+)) => {
        // Brackets on their own eval to their terms
        stlcb_term!($($rest)*)
    };
    ($ex:tt : $ty:tt) => {
        {
            // Type ascriptions
            let expr_parsed = stlcb_term!($ex);
            let ty_parsed = stlcb_type!($ty);
            STLCBTerm::Ann(Rc::new(expr_parsed), Rc::new(ty_parsed))
        }
    };
    (True) => {
        STLCBTerm::True
    };
    (False) => {
        STLCBTerm::False
    };
    ($var:ident) => {
        STLCBTerm::Var(stringify!($var).to_string())
    };
    (if $cond:tt then $ctrue:tt else $cfalse:tt) => {
        {
            let cond_parsed = stlcb_term!($cond);
            let ctrue_parsed = stlcb_term!($ctrue);
            let cfalse_parsed = stlcb_term!($cfalse);
            STLCBTerm::If(Rc::new(cond_parsed), Rc::new(ctrue_parsed), Rc::new(cfalse_parsed))
        }
    };
    ({$ex:expr}) => {
        // Curly parens escape expressions
        $ex
    };
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

#[derive(Default)]
struct STLCBProof {
    steps: Vec<Box<STLCBInference>>,
}

impl STLCBProof {
    pub fn step(&mut self, inf: STLCBInference) -> usize {
        self.steps.push(Box::new(inf));
        self.steps.len() - 1
    }

    pub fn trace(&self) {
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

impl TyCtx {
    pub fn bind(&mut self, x: String, t: Rc<STLCBType>) {
        self.variables.insert(x, t);
    }

    pub fn lookup(&self, x: &String) -> Option<Rc<STLCBType>> {
        self.variables.get(x).map(|v| v.clone())
    }
}

impl fmt::Debug for TyCtx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (v, t) in self.variables.iter() {
            write!(f, "{}:{:?}, ", v, t)?;
        }
        write!(f, "]")
    }
}

impl fmt::Debug for STLCBType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "Bool"),
            Self::Function(from, to) => match from.as_ref() {
                STLCBType::Bool => write!(f, "{:?} -> {:?}", from, to),
                STLCBType::Function(_, _) => write!(f, "({:?}) -> {:?}", from, to),
            },
        }
    }
}

impl fmt::Debug for STLCBTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(v) => write!(f, "{}", v),
            Self::App(rator, rand) => write!(f, "({:?}) $ {:?}", rator, rand),
            Self::Lam(v, body) => write!(f, "lambda {} . {:?}", v, body),
            Self::Ann(ex, ty) => write!(f, "{:?} : {:?}", ex, ty),
            Self::True => write!(f, "True"),
            Self::False => write!(f, "False"),
            Self::If(cond, ctrue, cfalse) => {
                write!(f, "if {:?} then {:?} else {:?}", cond, ctrue, cfalse)
            }
        }
    }
}

impl STLCBTerm {
    pub fn typecheck_fwd(
        &self,
        ctx: Rc<TyCtx>,
        proof: &mut STLCBProof,
    ) -> (STLCBType, STLCBProofIdent) {
        match self {
            // BT-VAR
            STLCBTerm::Var(v) => match ctx.lookup(&v) {
                Some(v_ty) => (
                    (*v_ty).clone(),
                    proof.step(STLCBInference::BTVar(ctx.clone(), v.clone(), v_ty)),
                ),
                None => {
                    panic!("typecheck failed: free variable {} without annotation", v);
                }
            },

            // BT-APP
            STLCBTerm::App(t1, t2) => match t1.typecheck_fwd(ctx.clone(), proof) {
                (STLCBType::Bool, _) => panic!("typecheck failed: applied value as function"),
                (STLCBType::Function(tau1, tau2), step_rator) => {
                    let step_rand = (**t2).typecheck_rev(ctx.clone(), proof, (*tau1).clone());
                    (
                        (*tau2).clone(),
                        proof.step(STLCBInference::BTApp(
                            ctx,
                            (*t1).clone(),
                            (*t2).clone(),
                            tau1,
                            tau2,
                            step_rator,
                            step_rand,
                        )),
                    )
                }
            },

            // BT-ANN
            STLCBTerm::Ann(term, typ) => {
                let step_body = term.typecheck_rev(ctx.clone(), proof, (**typ).clone());
                (
                    (**typ).clone(),
                    proof.step(STLCBInference::BTAnn(
                        ctx,
                        term.clone(),
                        typ.clone(),
                        step_body,
                    )),
                )
            }

            // BT-TRUE
            STLCBTerm::True => ((stlcb_type!(Bool), proof.step(STLCBInference::BTTrue(ctx)))),

            // BT-FALSE
            STLCBTerm::False => ((stlcb_type!(Bool), proof.step(STLCBInference::BTFalse(ctx)))),

            _ => panic!("typecheck failed: cannot infer type of {:?}", self),
        }
    }

    pub fn typecheck_rev(
        &self,
        ctx: Rc<TyCtx>,
        proof: &mut STLCBProof,
        target: STLCBType,
    ) -> STLCBProofIdent {
        match self {
            // BT-LAM
            STLCBTerm::Lam(x, body) => match target {
                STLCBType::Bool => panic!("typecheck failed: lambda is not a bool"),
                STLCBType::Function(tau1, tau2) => {
                    let mut bcx = (*ctx).clone();
                    bcx.bind(x.clone(), tau1.clone());
                    let body_step = body.typecheck_rev(Rc::new(bcx), proof, (*tau2).clone());
                    proof.step(STLCBInference::BTAbs(
                        ctx,
                        (*x).clone(),
                        (*body).clone(),
                        tau1,
                        tau2,
                        body_step,
                    ))
                }
            },

            // T-IF
            STLCBTerm::If(cond, ctrue, cfalse) => {
                let cond_step = cond.typecheck_rev(ctx.clone(), proof, stlcb_type!(Bool));
                let ctrue_step = ctrue.typecheck_rev(ctx.clone(), proof, target.clone());
                let cfalse_step = cfalse.typecheck_rev(ctx.clone(), proof, target.clone());
                proof.step(STLCBInference::BTIf(
                    ctx,
                    (*cond).clone(),
                    (*ctrue).clone(),
                    (*cfalse).clone(),
                    Rc::new(target),
                    cond_step,
                    ctrue_step,
                    cfalse_step,
                ))
            }

            // BT-CHECKINFER
            term => {
                let (tau, step) = term.typecheck_fwd(ctx.clone(), proof);
                assert_eq!(tau, target, "type mismatch in BT-CHECKINFER");
                proof.step(STLCBInference::BTCheckInfer(
                    ctx,
                    Rc::new(term.clone()),
                    Rc::new(target),
                    step,
                ))
            }
        }
    }
}

/// Typechecks a judgment of the form
///     |- term => ty
fn typecheck(term: &STLCBTerm, ty: STLCBType) {
    let mut proof = STLCBProof::default();
    let check = term.typecheck_fwd(Rc::new(TyCtx::default()), &mut proof);
    assert_eq!(check.0, ty, "type can be checked, but not to that!");
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
