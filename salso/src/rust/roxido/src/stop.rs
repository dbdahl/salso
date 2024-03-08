use crate::rbindings::*;
use crate::RObject;
use std::fmt::Display;

#[doc(hidden)]
pub struct RStopHelper(pub String);

/// Throw an R error.
#[macro_export]
#[allow(clippy::crate_in_macro_def)]
macro_rules! stop {
    () => {
        match std::env::var("RUST_BACKTRACE") {
            Ok(_) => {
                panic!("Panic in stop!() due to RUST_BACKTRACE environment variable")
            },
            Err(_) => {
                std::panic::panic_any(crate::RStopHelper(String::new()))
            }
        }
    };
    ($fmt_string:expr) => {
        match std::env::var("RUST_BACKTRACE") {
            Ok(_) => {
                let mut msg = String::new();
                msg.push_str("Panic in stop!(...) due to RUST_BACKTRACE environment variable... ");
                msg.push_str(&format!($fmt_string));
                panic!("{}", msg);
            },
            Err(_) => {
                std::panic::panic_any(crate::RStopHelper(format!($fmt_string)))
            }
        }
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        match std::env::var("RUST_BACKTRACE") {
            Ok(_) => {
                let mut msg = String::new();
                msg.push_str("Panic in stop!(...) due to RUST_BACKTRACE environment variable... ");
                msg.push_str(&format!($fmt_string, $($arg),*));
                panic!("{}", msg);
            },
            Err(_) => {
                std::panic::panic_any(crate::RStopHelper(format!($fmt_string, $($arg),*)))
            }
        }
    }
}

pub trait UnwrapOrStop<T> {
    fn stop(self) -> T;
    fn stop_str(self, msg: &str) -> T;
    fn stop_closure(self, msg: impl FnMut() -> String) -> T;
}

impl<T, S: Display> UnwrapOrStop<T> for Result<T, S> {
    fn stop(self) -> T {
        match self {
            Ok(t) => t,
            Err(e) => stop!("{}", e),
        }
    }
    fn stop_str(self, msg: &str) -> T {
        match self {
            Ok(t) => t,
            Err(_) => stop!("{}", msg),
        }
    }
    fn stop_closure<'a>(self, mut msg: impl FnMut() -> String) -> T {
        match self {
            Ok(t) => t,
            Err(_) => stop!("{}", msg()),
        }
    }
}

impl<T, RType, RMode> UnwrapOrStop<T> for Result<T, RObject<RType, RMode>> {
    fn stop(self) -> T {
        match self {
            Ok(t) => t,
            Err(_) => stop!("Not of asserted type or mode"),
        }
    }
    fn stop_str(self, msg: &str) -> T {
        match self {
            Ok(t) => t,
            Err(_) => stop!("{}", msg),
        }
    }
    fn stop_closure<'a>(self, mut msg: impl FnMut() -> String) -> T {
        match self {
            Ok(t) => t,
            Err(_) => stop!("{}", msg()),
        }
    }
}

impl<T> UnwrapOrStop<T> for Option<T> {
    fn stop(self) -> T {
        match self {
            Some(t) => t,
            None => stop!(),
        }
    }
    fn stop_str(self, msg: &str) -> T {
        match self {
            Some(t) => t,
            None => stop!("{}", msg),
        }
    }
    fn stop_closure<'a>(self, mut msg: impl FnMut() -> String) -> T {
        match self {
            Some(t) => t,
            None => stop!("{}", msg()),
        }
    }
}

#[doc(hidden)]
#[no_mangle]
pub extern "C" fn set_custom_panic_hook() -> SEXP {
    let default_panic = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        if panic_info
            .payload()
            .downcast_ref::<crate::RStopHelper>()
            .is_none()
        {
            default_panic(panic_info);
        }
    }));
    unsafe { R_NilValue }
}
