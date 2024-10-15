use std::future::Future;
use std::fmt;
use std::fmt::Write;
use std::pin::Pin;
use std::sync;
use std::task;
use std::vec;

use seq_macro::seq;

pub fn nybble_to_hex(nyb: u8) -> char {
    (if nyb < 10 {
        b'0' + nyb
    } else {
        b'a' + (nyb - 10)
    }) as char
}

pub fn fmt_hex_slice(vec: &[u8]) -> Result<std::string::String, fmt::Error> {
    let mut acc = std::string::String::new();
    for (i, b) in vec.iter().enumerate() {
        if i % 8 == 0 {
            write!(&mut acc, "{:02x}  ", b)?;
        } else {
            write!(&mut acc, "{:02x} ", b)?;
        }
    }
    Ok(acc)
}

pub struct Notifier {
    wakers: sync::Mutex<vec::Vec<task::Waker>>,
}

impl Notifier {
    pub fn new() -> Notifier {
        Notifier {
            wakers: sync::Mutex::new(vec::Vec::new()),
        }
    }

    pub fn notify(&self) {
        for w in self.wakers.lock().unwrap().drain(..) {
            w.wake();
        }
    }

    pub fn enroll(&self, cx: &task::Context) {
        let waker = cx.waker();
        let mut wakers = self.wakers.lock().unwrap();
        if !wakers.iter().any(|w| w.will_wake(waker)) {
            wakers.push(waker.clone());
        }
    }
}

impl std::ops::Drop for Notifier {
    fn drop(&mut self) {
        self.notify();
    }
}

impl Default for Notifier {
    fn default() -> Notifier {
        Self::new()
    }
}

pub enum Never {
}

// TODO: get rid of me when never type is stabilized.
pub struct NeverIterator<T>(Never, std::marker::PhantomData<T>);

impl<T> Iterator for NeverIterator<T> {
    type Item = T;
    
    fn next(&mut self) -> Option<T> {
        match self.0 {
        }
    }
}

impl<T> DoubleEndedIterator for NeverIterator<T> {
    fn next_back(&mut self) -> Option<T> {
        match self.0 {
        }
    }
}

// TODO: fix me up when we get variadic generics
// TODO: remove me when we get anonymous sum types
seq!(N in 1..=6 {
    pub enum PhiIterator<Item, #(I~N: Iterator<Item = Item> = NeverIterator<Item>,)*> {
        #(I~N(I~N),)*
    }

    impl<Item, #(I~N: Iterator<Item = Item>,)*> Iterator for PhiIterator<Item, #(I~N,)*> {
        type Item = Item;

        fn next(&mut self) -> Option<Item> {
            match self {
                #(Self::I~N(i) => i.next(),)*
            }
        }
    }

    impl<Item, #(I~N: DoubleEndedIterator<Item = Item>,)*> DoubleEndedIterator for PhiIterator<Item, #(I~N,)*> {
        fn next_back(&mut self) -> Option<Item> {
            match self {
                #(Self::I~N(i) => i.next_back(),)*
            }
        }
    }

    // TODO: get rid of me once never type and never type fallback is stabilized.
    #(
        seq!(M in 1..=N {
            pub type PhiIteratorOf~N<Item, #(I~M,)*> = PhiIterator<Item, #(I~M,)*>;
        });
    )*
});

pub struct PreemptableFuture<'lt, A: Future, B: Future>(pub Pin<&'lt mut A>, pub Pin<&'lt mut B>);

impl<'lt,A: Future, B: Future> Future for PreemptableFuture<'lt, A, B> {
    //type Output = Result<(A::Output, Pin<&'lt mut B>), (Pin<&'lt mut A>, B::Output)>;
    type Output = Result<A::Output, B::Output>;

    fn poll(mut self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
        match self.0.as_mut().poll(cx) {
            std::task::Poll::Ready(x) => std::task::Poll::Ready(Ok(x)),
            std::task::Poll::Pending => match self.1.as_mut().poll(cx) {
                std::task::Poll::Ready(x) => std::task::Poll::Ready(Err(x)),
                std::task::Poll::Pending => std::task::Poll::Pending
            }
        }
    }
}

/* We use a different version of this macro for tests. */
#[cfg(not(test))]
#[macro_export]
macro_rules! catch_panic {
    { @default($fallback:expr); $($body:tt)* } => {
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            $($body)*
        })) {
            Ok(x) => x,
            Err(_) => $fallback,
        }
    };
    
    { $($body:tt)* } => {
        catch_panic! { @default(()); $($body)* }
    };
}

#[cfg(test)]
pub mod test {
    use std::rc;
    use std::cell;
    use std::any::Any;
    
    thread_local! {
        pub static PANIC_GUARD: cell::RefCell<Option<rc::Rc<cell::RefCell<Option<Box<dyn Any + Send>>>>>> = Default::default();
    }

    #[macro_export]
    macro_rules! catch_panic {
        { @default($fallback:expr); $($body:tt)* } => {
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                $($body)*
            })) {
                Ok(x) => x,
                Err(e) => {
                    $crate::util::test::PANIC_GUARD.with_borrow(|pg| {
                        if let Some(pg) = &*pg {
                            let mut guard = pg.borrow_mut();
                            if guard.is_none() {
                                *guard = Some(e);
                            }
                        }
                    });
                    $fallback
                },
            }
        };
        
        { $($body:tt)* } => {
            catch_panic! { @default(()); $($body)* }
        };
    }
    
    pub struct PanicGuard {
        pub payload: rc::Rc<cell::RefCell<Option<Box<dyn std::any::Any + Send>>>>,
    }

    impl PanicGuard {
        pub fn new() -> Self {
            let payload: rc::Rc<cell::RefCell<Option<Box<dyn std::any::Any + Send>>>> = Default::default();

            PANIC_GUARD.with_borrow_mut(|pg| *pg = Some(payload.clone()));
            
            Self {
                payload,
            }
        }

        pub fn drain(&self) {
            if let Some(payload) = self.payload.take() {
                std::panic::resume_unwind(payload);
            }
        }
    }

    impl Drop for PanicGuard {
        fn drop(&mut self) {
            self.drain();
        }
    }
}
