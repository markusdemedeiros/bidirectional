#[macro_export(stlcb_type)]
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

#[macro_export(stlcb_term)]
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
