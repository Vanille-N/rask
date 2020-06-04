macro_rules! as_expr {
    ( $e:expr ) => { $e }
}

macro_rules! rask {
    ( $x:ident ) => { as_expr!($x) };
    ( ^($( $x:tt )*) ) => { as_expr!(($( $x, )*)) };
    ( (lambda ($($arg:ident)*) ($($body:tt)*)) ) => {
        {
            let func = |$($arg),*| {
                as_expr!(rask!(($($body)*)))
            };
            as_expr!(func)
        }
    };
    ( ($f:ident! $( $x:tt )*) ) => {
        $f!($( rask!($x) ),*)
    };
    ( ($f:ident $( $x:tt )*) ) => {
        {
            as_expr!($f($( rask!($x) ),*))
        }
    };
    ( ($f:tt $( $x:tt )*) ) => {
        {
            let func = as_expr!(rask!($f));
            as_expr!(func($( rask!($x) ),*))
        }
    };
    ( $x:expr ) => { $x };
}


#[cfg(test)]
mod test {
    pub fn add(a: i32, b: i32) -> i32 {
        a + b
    }

    #[test]
    pub fn apply_function() {
        assert_eq!(rask!((add 1 2)), 3);
        assert_eq!(rask!((add (add 1 1) 3)), 5);
    }

    #[test]
    pub fn apply_macro() {
        assert_eq!(rask!((format! "{}{}" 1 2)), "12");
        assert_eq!(rask!((format! "<{}>" (format! "{}{}" 'a' 100))), "<a100>");
    }

    #[test]
    pub fn apply_lambda() {
        let f = |x| x*x;
        // assert_eq!(rask!((f -1)), 1);
        assert_eq!(rask!((f (f 2))), 16);
    }
}
