
# Notes

## Design

The ultimate goal is to infer types for *functions*, since that's what's 
passed to RLLVMCompile.

If we consider something like
```
a <- 5
```
then we can infer that `a` has type `double`. Is that useful, though?
Every line has to be translated to LLVM IR, so yes. We need type 
information for:

* Allocating new variables
* Calling functions

What's the best way to store the type information? There will potentially be 
other metadata we want to store (e.g., dimensions).

## Type System

Can we infer R types and then have the compiler figure out how to translate 
them to C?

* It's not clear whether this would make inference any easier (probably not) 
  but it does decouple the inference system from the compiler.
* This approach is also more general, and allows us to support semantic types 
  such as an "index" type.

Should the S4 classes be grouped by atomic type or by scalar/vector? 

* Scalar/vector types may have similar metadata, such as length. Inheritance 
  makes handling slots for this easy.
* Binary operations and any kind of upcasting based on atomic behavior is 
  easier if scalar/vector types use the same class.
    + A length slot could be inspected to determine scalar/vector, but then we 
      can't use dispatch.
    + If all types have a length slot, aggregate operations are easier. But 
      testing a slot could be an issue for other metadata as well.
    + Could a comparison operator (<) be used for upcasting instead?

## `if` Statements

Consider
```
if (x > 4)
    "hello"
else
    c("hello", "goodbye")
```
Should this collapse to `CharacterVectorType`?
