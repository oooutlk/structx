# Summary

[summary]: #summary

**Provides _structural records_** of the form `structx!{ foo: 1u8, bar: true }`
of type `Structx!{ foo: u8, bar: bool }`. Another way to understand these sorts
of objects is to think of them as *"tuples with named fields"*,
*"unnamed structs"*, or *"anonymous structs"*.

# Motivation

[motivation]: #motivation

There are four motivations to introduce structural records, two major and two
minor.

## Major: Improving ergonomics, readability, and maintainability

Sometimes, you just need a one-off struct that you will use a few times for some
public API or in particular for some internal API. For example, you'd like to
send over a few values to a function, but you don't want to have too many
function arguments. In such a scenario, defining a new nominal `struct` which
contains all the fields you want to pass is not a particularly ergonomic
solution.

Instead, it is rather heavy weight:

```rust
struct Color {
    red   : u8,
    green : u8,
    blue  : u8,
}

fn caller() {
    ...
    let color = Color{
        red: 255, green: 0, blue: 0
    };
    ...
    do_stuff_with( color );
    ...
}

// Only time we use `Color`!
fn do_stuff_with( color: Color ) {
    some_stuff( color.red );
    ...
    other_stuff( color.green );
    ...
    yet_more_stuff( color.blue );
}
```

To remedy the ergonomics problem, you may instead opt for using a positional
tuple to contain the values you want to pass. However, now you have likely
created a regression for readers of your code since the fields of tuples are
accessed with their positional index, which does not carry clear semantic
intent:

```rust
fn caller() {
    ...
    let color = (255, 0, 0);
    ...
    // Unclear what each position means...
    do_stuff_with( color );
    ...
    // More ergonomic!
    fn do_stuff_with(color: (u8, u8, u8)) {
        // But less readable... :(
        some_stuff( color.0 );
    ...
    other_stuff( color.1 );
    ...
    yet_more_stuff( color.2 );
}
```

Using structural records, we can have our cake and eat it too:

```rust
use structx::*;

fn caller() {
    ...
    let color = structx!{ red: 255, green: 0, blue: 0 };
    ...
    // ...but here it is clear.
    do_stuff_with( color );
    ...
}

// More ergonomic!
fn do_stuff_with( color: structx!{ red: u8, green: u8, blue: u8 }) {
    // *And* readable! :)
    some_stuff( color.red );
    ...
    other_stuff( color.green );
    ...
    yet_more_stuff( color.blue );
}
```

In the above snippet, the semantic intent of the fields is clear both when
reading the body of the function, as well as when reading the documentation
when `do_stuff_with` is exposed as a public API.

[@eternaleye]: https://internals.rust-lang.org/t/pre-rfc-unnamed-struct-types/3872/58
[@kardeiz]: https://internals.rust-lang.org/t/pre-rfc-unnamed-struct-types/3872/65

Another example of reducing boilerplate was given by [@eternaleye]:

```rust
use structx::*;

struct RectangleClassic {
    width  : u64,
    height : u64,
    red    : u8,
    green  : u8,
    blue   : u8,
}

struct RectangleTidy {
    dimensions : Structx!{
        width  : u64,
        height : u64,
    },
    color     : Structx!{
        red   : u8,
        green : u8,
        blue  : u8,
    },
}
```

In the second type `RectangleTidy`, we keep boilerplate to a minimum and we can
also treat `rect.color` and `rect.dimensions` as separate objects and move them
out of `rect : RectangleTidy` as units, which we cannot do with
`RectangleClassic`.

If we wanted to do that, we would have to invent two new types `Dimensions` and
`Color` and then `#[derive(..)]` the bits and pieces that we need.

As noted by [@kardeiz], this ability may also be useful for serializing one-off
structures with `serde`.

## Major: Better rapid prototyping and refactoring

Let's assume we opted for using the type
`Structx!{ red: u8, green: u8, blue: u8 }` from above. This gave us the ability
to prototype our application rapidly.

However, as time passes, we might have more uses for RGB colours and so we
decide to make a nominal type out of it and give it some operations.

Because the structural record we've used above has named fields, we can easily
refactor it into the nominal type `Color`.

Indeed, an IDE should be able to use the information that exists and provide the
refactoring for you automatically. If we had instead used a positional tuple,
the information would simply be unavailable. Thus, the refactoring could not be
made automatically and if you had to do it manually, you would need to spend
time understanding the code to deduce what proper field names would be.

## Minor: Emulating named function arguments

Structural records could be considered to lessen the need for named function
arguments by writing in the following style:

```rust
use structx::*;

fn foo( bar: Structx!{ baz: u8, quux: u8 }) -> u8 {
    bar.baz + bar.quux
}

fn main() {
    assert_eq!( 3, foo(structx!{ baz: 1, quux: 2 }));
}
```

Using some macros to make it more clear:

```rust
use structx::*;
use structx::named_args::*;

#[named_args]
fn foo( baz: u8, quux: u8 ) -> u8 {
    bar.baz + bar.quux
}

fn main() {
    assert_eq!( 3, foo( args!{ baz: 1, quux: 2 }));
}
```

While this is a possible use case, in this crate, we do not see named function
arguments as a *major* motivation for structural records as they do not cover
aspects of named arguments that people sometimes want. In particular:

1. With structural records, you cannot construct them positionally.

In other words, you may not call `foo` from above with `foo(1, 2)` because these
records do not have a defined order that users can make use of.

[`<*const T>::copy_to_nonoverlapping`]: https://doc.rust-lang.org/std/primitive.pointer.html#method.copy_to_nonoverlapping

2. You cannot adapt existing standard library functions to use named arguments.

Consider for example the function [`<*const T>::copy_to_nonoverlapping`]. It has
the following signature:

```rust
pub unsafe fn copy_to_nonoverlapping( self, dest: *mut T, count: usize );
```

Because this is an `unsafe` function, we might want to call this as:

```rust
ptr.copy_to_nonoverlapping( args!{ dest: <the_destination>, count: <the_count> })
```

However, because we can write:

```rust
const X: unsafe fn( *const u8, *mut u8, usize ) = <*const u8>::copy_to_nonoverlapping;
```

it would be a breaking change to redefine the function to take a structural
record instead of two arguments.

Having noted these two possible deficiencies of structural records as a way to
emulate named function arguments, this emulation can still work well in many
cases. Thus, while the motivation is not major here, we still consider it to be
a minor motivation.

## Minor: Smoothing out the language

The current situation in the language with respect to product types can be
described with the following table:

|                  | Nominal                        | Structural         |
|------------------|--------------------------------|--------------------|
| **Unit**         | Yes, `struct T;`               | Yes, `()`          |
| **Positional**   | Yes, `struct T(A, B);`         | Yes, `(A, B)`      |
| **Named fields** | Yes, `struct T { a: A, b: B }` | **No**, this crate |

As we can see, the current situation is inconsistent.

While the language provides for unit types and positional product types of both
the nominal and structural flavour, the structural variant of structs with named
fields is missing while the nominal type exists.

A consistent programming language is a beautiful language, but it's not an end
in itself. Instead, the main benefit is to reduce surprises for learners.

[@withoutboats]: https://internals.rust-lang.org/t/pre-rfc-unnamed-struct-types/3872/23
[@regexident]: https://internals.rust-lang.org/t/pre-rfc-catching-functions/6505/188

Indeed, [@withoutboats] noted:

> To me this seems consistent and fine - the kind of feature a user could infer
to exist from knowing the other features of Rust - but I’m not thrilled by the
idea of trying to use this to implement named arguments.

# Guide-level explanation

[guide-level-explanation]: #guide-level-explanation

## Vocabulary

[structural typing]: https://en.wikipedia.org/wiki/Structural_type_system
[nominal typing]: https://en.wikipedia.org/wiki/Nominal_type_system
[product type]: https://en.wikipedia.org/wiki/Product_type
[record type]: https://en.wikipedia.org/wiki/Record_(computer_science)

- With *[structural typing]*, the equivalence of two types is determined by
looking at their structure. For example, given the type `(A, B)` and `(C, D)`
where `A`, `B`, `C`, and `D` are types, if `A == C` and `B == D`, then
`(A, B) == (C, D)`. In Rust, the only case of structural typing is the
positional tuples we've already seen.

The main benefit of structural typing is that it becomes ergonomic and quite
expressive to produce new types *"out of thin air"* without having to predefine
each type.

- With *[nominal typing]*, types are instead equivalent if their declared names
are the same. For example, assuming that the types `Foo` and `Bar` are defined
as:

```rust
struct Foo( u8, bool );
struct Bar( u8, bool );
```

even though they have the same structure, `Foo` is not the same type as `Bar`
and so the type system will not let you use a `Foo` where a `Bar` is expected.

The main benefit to nominal typing is maintainability and robustness. In Rust,
we take advantage of nominal typing coupled with privacy to enforce API
invariants. This is particularly important for code that involves use of
`unsafe`.

- A *[product type]* is a type which is made up of a sequence of other types.

For example, the type `(A, B, C)` consists of the types `A`, `B`, *and* `C`.
They are called product types because the number of possible values that the
type can take is the *product* of the number of values that each component /
factor / operand type can take. For example, if we consider `(A, B, C)`, the
number of values is `values(A) * values(B) * values(C)`.

- A *[record type]* is a special case of a product type in which each component

type is *labeled*. In Rust, record types are `struct`s with named fields.

For example, you can write:

```rust
struct Foo {
    bar : u8,
    baz : bool,
}
```

The only case of a record type in Rust uses nominal typing. There is currently
no structural variant of record types. This brings us to this crate...

## Feature

As we've seen, Rust currently lacks a structurally typed variant of record types.

In this crate, we propose to change this by providing predefined record types that seems also structural, or in other words: *"structural records"*.

### Construction

To create a structural record with the field `alpha` with value `42u8`, `beta`
with value `true`, and `gamma` with value `"Dancing Ferris"`, you can simply
write:

```rust
use structx::*;

let my_record = structx!{
    alpha : 42u8,
    beta  : true,
    gamma : "Dancing Ferris",
};
```

Note how this is the same syntax as used for creating normal structs but without
the name of the struct. So we have taken:

```rust
use structx::*;

struct MyRecordType {
    alpha : u8,
    beta  : bool,
    gamma : &'static str,
}

let my_record_nominal = MyRecordType {
    alpha : 42u8,
    beta  : true,
    gamma : "Dancing Ferris",
};
```

and removed `MyRecordType`. Note that because we are using structural typing,
we did not have to define a type `MyRecordType` ahead of time. If you already
had variables named `alpha` and `beta` in scope, just as you could have with
`MyRecordType`, you could have also written:

```rust
use structx::*;

let alpha = 42u8;
let beta = true;
let my_record = structx!{
    alpha,
    beta ,
    gamma: "Dancing Ferris",
};
```

### Pattern matching

Once you have produced a structural record, you can also pattern match on the
expression. To do so, you can write:

```rust
match my_record {
    structx!{ alpha, beta, gamma } =>
        println!( "{}, {}, {}", alpha, beta, gamma ),
}
```

This pattern is *irrefutable* so you can also just write:

```rust
let structx!{ alpha, beta, gamma } = my_record;
println!( "{}, {}, {}", alpha, beta, gamma );
```

This is not particular to `match` and `let`. This also works for `if let`,
`while let`, `for` loops, and function arguments.

If we had used `MyRecordType`, you would have instead written:

```rust
let MyRecordType{ alpha, beta, gamma } = my_record_nominal;
println!( "{}, {}, {}", alpha, beta, gamma );
```

When pattern matching on a structural record, it is also possible to give the
binding you've created a different name. To do so, write:

```rust
let structx!{ alpha: new_alpha, beta, gamma } = my_record;
println!( "{}, {}, {}", new_alpha, beta, gamma );
```

In this snippet, we've bound the field `alpha` to the binding `new_alpha`.
This is not limited to one field, you can do this will all of them.
Unlike nominal structs, it's impossible to ignore some or all fields when
pattern matching on structural records:

```rust,compile_fail
let structx!{ alpha, .. } = my_record;
println!( "{}", alpha );
let structx!{ .. } = my_record;
```

### Field access

Given the binding `my_record_nominal` of type `MyRecordType`, you can access its
fields with the usual `my_record_nomina.alpha` syntax. This also applies to
structural records. It is perfectly valid to move or copy:

```rust
println!( "The answer to life... is: {}", my_record.alpha );
```

or to borrow a field:

```rust
fn borrow( x: &bool ) { .. }
borrow( &my_record.beta );
```

including mutably:

```rust
use structx::*;

fn mutably_borrow( x: &mut bool ) { .. }
let mut my_record = structx!{ alpha: 42u8, beta: true, gamma: "Dancing Ferris" };
mutably_borrow( &mut my_record.beta );
```

# Struct update syntax

Nominal structs support what is referred to as the "struct update syntax",
otherwise known as functional record update (FRU). For example, you can write:

```rust
struct Color {
    red   : u8,
    green : u8,
    blue  : u8,
}

let yellow = Color{ red: 0, green: 255, blue: 255 };
let white = Color{ red: 255, ..yellow };
```

This also works for structural records, so you can write:

```rust
let yellow = structx!{ red: 0, green: 255, blue: 255 };
let white = structx!{ red: 255, ..yellow };
```

To match the behaviour of FRU for nominal structs, we impose a restriction that
the fields mentioned before ..yellow must all exist in yellow and have the same
types. This means that we cannot write:

```rust,compile_fail
let white_50_opacity = structx!{ alpha: 0.5, red: 255, ..yellow };
```

### The type of a structural record

You might be wondering what the type of `my_record` that we've been using thus
far is. Because this is structural typing, the fields are significant, so the
type of the record is simply:

```rust
use structx::*;

type TheRecord = Structx!{
    alpha: u8, beta: bool, gamma: &'static str
};
let my_record: TheRecord = structx!{
    alpha: 42u8, beta: true, gamma: "Dancing Ferris"
};
```

Notice how this matches the way we defined `MyRecordType` is we remove the
prefix `struct MyRecordType`. The order in which we've put `alpha`, `beta`, and
`gamma` here does not matter.

We could have also written:

```rust
use structx::*;

type TheRecord = structx!{
    beta: bool, alpha: u8, gamma: &'static str
};
```

As long as the type is a *permutation* of each pair of field name and field
type, the type is the same. This also means that we can write:

```rust
use structx::*;

let my_record: TheRecord = structx!{
    alpha: 42u8, gamma: "Dancing Ferris", beta: true
};
```

### Implemented traits

With respect to trait implementations, because the type is structural, and
because there may be an unbound number of fields that can all be arbitrary
identifiers, there's no way to define implementations for the usual traits
in the language itself.

[tuples]: https://doc.rust-lang.org/std/primitive.tuple.html

Instead, the compiler will automatically provide trait implementations for the
standard traits that are implemented for [tuples]. These traits are: `Clone`,
`Copy`, `PartialEq`, `Eq`, `PartialOrd`, `Ord`, `Debug`, `Default`, and `Hash`.
Each of these traits will only be implemented if all the field types of a struct
implements the trait.

For all of the aforementioned standard traits, the semantics of the
implementations are similar to that of `#[derive(Trait)]` for named-field
structs.

+ For cloning `Structx!{ alpha: A, beta: B, gamma: C }` the logic is simply:

```rust
structx!{
    alpha : self.alpha.clone(),
    beta  : self.beta .clone(),
    gamma : self.gamma.clone(),
}
```

+ For `Default`, you would get:

```rust
structx!{
    alpha : Default::default(),
    beta  : Default::default(),
    gamma : Default::default(),
}
```

+ For `PartialEq`, each field is compared with same field in `other: Self`.

+ For `ParialOrd` and `Ord`, lexicographic ordering is used based on the name of
the fields and not the order given because structural records don't respect the
order in which the fields are put when constructing or giving the type of the
record.

+ For `Debug` the same lexicographic ordering for `Ord` is used. As an example,
when printing out `my_record` as with:

```rust
use structx::*;

let my_record = structx!{
    beta: true, alpha: 42u8, gamma: "Dancing Ferris"
};
println!( "{:#?}", my_record );
```

the following would appear:

```rust
structx!{
    alpha : 42,
    beta  : true,
    gamma : "Dancing Ferris",
}
```

+ For `Hash`, the same ordering of the fields as before is used and then
`self.the_field.hash(state)` is called on each field in that order.

For example:

```rust
self.alpha.hash( state );
self.beta .hash( state );
self.gamma.hash( state );
```

For auto traits (e.g. `Send`, `Sync`, `Unpin`), if all field types implement the
auto trait, then the structural record does as well. For example, if `A` and `B`
are `Send`, then so is `Structx!{ x: A, y: B }`.

A structural record is `Sized` if all field types are. If the lexicographically
last field of a structural record is `!Sized`, then so is the structural record.
If any other field but the last is `!Sized`, then the type of the structural
record is not well-formed.

### Implementations and orphans

[RFC 2451]: https://github.com/rust-lang/rfcs/pull/2451

It is possible to define your own implementations for a structural record. The
orphan rules that apply here are those of [RFC 2451] by viewing a structural
record as a positional tuple after sorting the elements lexicographically.

For example, if a `trait` is crate-local, we may implement it for a record:

```rust
use structx::*;

trait Foo {}
impl Foo for Structx!{ alpha: bool, beta: u8 } {}
```

Under 2451, we can also write:

```rust
use structx::*;

struct Local<T>( T );

impl From<()> for Local<Structx!{ alpha: bool, beta: u8 }> { ... }
```

This is valid because `Local` is considered a local type.

However, a structural record itself isn't a local type, so you cannot write:

```rust
use structx::*;

struct A;

impl From<()> for Structx!{ alpha: A, beta: u8 } { ... }
```

This is the case even though `A` is a type local to the crate.

The behaviour for inherent implementations is also akin to tuples.

# Drawbacks

[drawbacks]: #drawbacks

## Overuse?

Nominal typing is a great thing. It offers robustness and encapsulation with
which quantities that are semantically different but which have the same type
can be distinguished statically. With privacy, you can also build APIs that
make use of `unsafe` that you couldn't do with tuples or structural records.

If structural records are overused, this may reduce the overall robustness
of code in the ecosystem. However, we argue that structural records are more
robust than positional tuples are and allow you to more naturally transition
towards nominally typed records so the loss of robustness may be made up for
by reduced usage of positional tuples.

## Hard to implement crate-external traits

As with positional tuples, because a structural record is never crate local,
this presents users with a problem when they need to implement a trait they
don't own for a structural record comprising of crate-local types.

For example, say that you have the crate-local types `Foo` and `Bar`. Both of
these types implement `serde::Serialize`. Now you'd like to serialize
`type T = { foo: Foo, bar: Bar };`. However, because neither `serde::Serialize`
nor `T` is crate-local, you cannot `impl serde::Serialize for T { ... }`.

This inability is both a good and a bad thing. The good part of it is that it
might prevent overuse of structural records and provide some pressure towards
nominal typing that might be good for robustness. The bad part is that these
sort of one-off structures are a good reason to have structural records in the
first place. With some combined quantification of field labels (possibly via
const generics), and with tuple-variadic generics, it should be possible (for
`serde`, if there is a will, to offer implementations of `Serialize` for all
structural records.

Note that while `impl serde::Serialize for T { ... }` may not be possible
without extensions, the following would be:

```rust
#[derive( Serialize )]
struct RectangleTidy {
    dimensions: Structx!{
        width: u64,
        height: u64,
    },
    color: Structx!{
        red: u8,
        green: u8,
        blue: u8,
    },
}
```

## "Auto-implementing traits is a magical hack"

Indeed, we would much prefer to use a less magical approach, but providing these
traits without compiler magic would require significantly more complexity such
as polymorphism over field names as well as variadic generics. Therefore, to
make structural records usable in practice, providing a small set of traits with
compiler magic is a considerably less complex approach. In the future, perhaps
the magic could be removed.
