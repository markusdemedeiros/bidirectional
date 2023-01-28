use std::{collections::BTreeMap, fmt, rc::Rc};

#[derive(PartialEq, Eq, Clone)]
pub(crate) enum STLCBTerm {
    Var(String),
    App(Rc<STLCBTerm>, Rc<STLCBTerm>),
    Lam(String, Rc<STLCBTerm>),
    Ann(Rc<STLCBTerm>, Rc<STLCBType>),
    True,
    False,
    If(Rc<STLCBTerm>, Rc<STLCBTerm>, Rc<STLCBTerm>),
}

#[derive(PartialEq, Eq, Clone)]
pub(crate) enum STLCBType {
    Bool,
    Function(Rc<STLCBType>, Rc<STLCBType>),
}

#[derive(Eq, PartialEq, Default, Clone)]
pub(crate) struct TyCtx {
    variables: BTreeMap<String, Rc<STLCBType>>,
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
