#![allow(unused)]
#![feature(box_patterns)]

use core::panic;
use std::{collections::BTreeMap, default, fmt};

#[derive(PartialEq, Eq)]
enum STLCBTerm {
    Var(String),
    App(Box<STLCBTerm>, Box<STLCBTerm>),
    Lam(String, Box<STLCBTerm>),
    Ann(Box<STLCBTerm>, STLCBType),
    True,
    False,
    If(Box<STLCBTerm>, Box<STLCBTerm>, Box<STLCBTerm>),
}

#[derive(PartialEq, Eq, Clone)]
enum STLCBType {
    Bool,
    Function(Box<STLCBType>, Box<STLCBType>),
}

#[derive(Eq, PartialEq, Default, Clone)]
struct TyCtx {
    variables: BTreeMap<String, STLCBType>,
}

impl TyCtx {
    pub fn bind(&mut self, x: String, t: STLCBType) {
        self.variables.insert(x, t);
    }

    /// panics on unbound variables with no ascription
    pub fn lookup(&self, x: &String) -> Option<&STLCBType> {
        self.variables.get(x)
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
            Self::Function(box from, box to) => match from {
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
            Self::App(rator, rand) => write!(f, "({:?}) ~ {:?}", rator, rand),
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
            STLCBType::Function(Box::new(t0), Box::new(t1))
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

//    ($first:tt applyto ($next: tt) $($rest:tt)*) => {
//        {
//            let rator = stlcb_term!($first);
//            let rand = stlcb_term!(($next) $($rest)* );
//            STLCBTerm::App(Box::new(rator), Box::new(rand))
//        }
//    };

macro_rules! stlcb_term {
    ($first:tt ~ $($rest:tt)+) => {
        {
            let rator = stlcb_term!($first);
            let rand = stlcb_term!($($rest)+);
            STLCBTerm::App(Box::new(rator), Box::new(rand))
        }
    };
    (lambda $var:ident . $($body:tt)+) => {
        {
            // Lambda terms must be enclosed in brackets
            let body_parsed = stlcb_term!($($body)+);
            STLCBTerm::Lam(stringify!($var).to_string(), Box::new(body_parsed))
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
            STLCBTerm::Ann(Box::new(expr_parsed), ty_parsed)
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
            STLCBTerm::If(Box::new(cond_parsed), Box::new(ctrue_parsed), Box::new(cfalse_parsed))
        }
    };
    ({$ex:expr}) => {
        // Curly parens escape expressions
        $ex
    };
}

/// Typechecks a judgment of the form
///     |- term => ty
fn typecheck(term: &STLCBTerm, ty: STLCBType) {
    assert_eq!(
        typecheck_fwd(&mut TyCtx::default(), term),
        ty,
        "type can be checked, but not to that!"
    );
}

/// Typechecks a judgment of the form
///     ctx |- term => (return value)
fn typecheck_fwd<'cx, 't: 'cx>(ctx: &'cx mut TyCtx, term: &'t STLCBTerm) -> STLCBType {
    match term {
        // BT-VAR
        STLCBTerm::Var(v) => match ctx.lookup(&v) {
            Some(v_ty) => (*v_ty).clone(),
            None => {
                panic!("typecheck failed: free variable {} without annotation", v);
            }
        },

        // BT-APP
        STLCBTerm::App(box t1, box t2) => match typecheck_fwd(&mut (*ctx).clone(), t1) {
            STLCBType::Bool => panic!("typecheck failed: applied value as function"),
            STLCBType::Function(box tau1, box tau2) => {
                typecheck_rev(ctx, &t2, tau1);
                tau2
            }
        },

        // BT-ANN
        STLCBTerm::Ann(box term, typ) => {
            typecheck_rev(ctx, term, (typ).clone());
            (*typ).clone()
        }

        // BT-TRUE
        STLCBTerm::True => stlcb_type!(Bool),

        // BT-FALSE
        STLCBTerm::False => stlcb_type!(Bool),

        _ => panic!("typecheck failed: cannot infer type of {:?}", term),
    }
}

/// Typechecks a judgment of the form
///     ctx |- term <= ty
fn typecheck_rev<'cx, 't: 'cx>(ctx: &'cx mut TyCtx, term: &'t STLCBTerm, ty: STLCBType) {
    match term {
        // BT-LAM
        STLCBTerm::Lam(x, box body) => match ty {
            STLCBType::Bool => panic!("typecheck failed: lambda is not a bool"),
            STLCBType::Function(box tau1, box tau2) => {
                let bcx = &mut ctx.clone();
                bcx.bind((*x).clone(), tau1);
                typecheck_rev(bcx, body, tau2);
            }
        },

        // T-IF
        STLCBTerm::If(box cond, box ctrue, box cfalse) => {
            typecheck_rev(&mut ctx.clone(), cond, stlcb_type!(Bool));
            typecheck_rev(&mut ctx.clone(), ctrue, ty.clone());
            typecheck_rev(ctx, cfalse, ty);
        }

        // BT-CHECKINFER
        _ => assert_eq!(typecheck_fwd(ctx, term), ty),
    }
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
