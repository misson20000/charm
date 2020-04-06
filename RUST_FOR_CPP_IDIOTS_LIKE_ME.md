# Rust for C++ Idiots like Me

## What's up with `dyn Trait`? Why does the `dyn` keyword exist?

A trait is not a type. It's not like in C++ where you inherit from a base class
of entirely pure virtual methods and your derived class grows a vtable and some
field to accomodate the memory layout of the base class so you can take a
pointer. A trait has no memory layout.

The `dyn` keyword modifies a trait to make it into a "trait object", which is a
type you can take a pointer to. Pointers to trait objects are fat pointers,
containing a pointer to the underlying object and a vtable for adapting trait
object calls to underlying struct calls.

This whole "fat pointer" thing is pretty normal in Rust and happens in a few
other places, too.

 - https://stackoverflow.com/questions/57754901/what-is-a-fat-pointer-in-rust
