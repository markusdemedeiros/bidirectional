use std::rc::Rc;

use crate::stlcb_type;

use super::{
    proof::{STLCBInference, STLCBProof},
    STLCBTerm, STLCBType, TyCtx,
};

/// Bidirectional type checks to a type T or error E with context C
pub trait Bidirectional<C, P, E, T, W> {
    /// Judgements of the form
    ///     C |- self => T
    fn infer(&self, ctx: C, proof: &mut P) -> Result<(T, W), E>;

    /// Judgements of the form
    ///     C |- self <= T
    fn check(&self, ctx: C, proof: &mut P, target: T) -> Result<W, E>;
}

impl Bidirectional<Rc<TyCtx>, STLCBProof, String, STLCBType, usize> for STLCBTerm {
    fn infer(&self, ctx: Rc<TyCtx>, proof: &mut STLCBProof) -> Result<(STLCBType, usize), String> {
        match self {
            // BT-VAR
            STLCBTerm::Var(v) => match ctx.lookup(&v) {
                Some(v_ty) => Ok((
                    (*v_ty).clone(),
                    proof.step(STLCBInference::BTVar(ctx.clone(), v.clone(), v_ty)),
                )),
                None => {
                    panic!("typecheck failed: free variable {} without annotation", v);
                }
            },

            // BT-APP
            STLCBTerm::App(t1, t2) => match t1.infer(ctx.clone(), proof)? {
                (STLCBType::Bool, _) => panic!("typecheck failed: applied value as function"),
                (STLCBType::Function(tau1, tau2), step_rator) => {
                    let step_rand = (**t2).check(ctx.clone(), proof, (*tau1).clone())?;
                    Ok((
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
                    ))
                }
            },

            // BT-ANN
            STLCBTerm::Ann(term, typ) => {
                let step_body = term.check(ctx.clone(), proof, (**typ).clone())?;
                Ok((
                    (**typ).clone(),
                    proof.step(STLCBInference::BTAnn(
                        ctx,
                        term.clone(),
                        typ.clone(),
                        step_body,
                    )),
                ))
            }

            // BT-TRUE
            STLCBTerm::True => Ok((stlcb_type!(Bool), proof.step(STLCBInference::BTTrue(ctx)))),

            // BT-FALSE
            STLCBTerm::False => (Ok((stlcb_type!(Bool), proof.step(STLCBInference::BTFalse(ctx))))),

            _ => panic!("typecheck failed: cannot infer type of {:?}", self),
        }
    }

    fn check(
        &self,
        ctx: Rc<TyCtx>,
        proof: &mut STLCBProof,
        target: STLCBType,
    ) -> Result<usize, String> {
        match self {
            // BT-LAM
            STLCBTerm::Lam(x, body) => match target {
                STLCBType::Bool => panic!("typecheck failed: lambda is not a bool"),
                STLCBType::Function(tau1, tau2) => {
                    let mut bcx = (*ctx).clone();
                    bcx.bind(x.clone(), tau1.clone());
                    let body_step = body.check(Rc::new(bcx), proof, (*tau2).clone())?;
                    Ok(proof.step(STLCBInference::BTAbs(
                        ctx,
                        (*x).clone(),
                        (*body).clone(),
                        tau1,
                        tau2,
                        body_step,
                    )))
                }
            },

            // T-IF
            STLCBTerm::If(cond, ctrue, cfalse) => {
                let cond_step = cond.check(ctx.clone(), proof, stlcb_type!(Bool))?;
                let ctrue_step = ctrue.check(ctx.clone(), proof, target.clone())?;
                let cfalse_step = cfalse.check(ctx.clone(), proof, target.clone())?;
                Ok(proof.step(STLCBInference::BTIf(
                    ctx,
                    (*cond).clone(),
                    (*ctrue).clone(),
                    (*cfalse).clone(),
                    Rc::new(target),
                    cond_step,
                    ctrue_step,
                    cfalse_step,
                )))
            }

            // BT-CHECKINFER
            term => {
                let (tau, step) = term.infer(ctx.clone(), proof)?;
                assert_eq!(tau, target, "type mismatch in BT-CHECKINFER");
                Ok(proof.step(STLCBInference::BTCheckInfer(
                    ctx,
                    Rc::new(term.clone()),
                    Rc::new(target),
                    step,
                )))
            }
        }
    }
}
