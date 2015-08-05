
# Notes

## 2015.08.05

Inferring the return type of a function is a small challenge. Functions can 
return at any point, and it's not enough to just build a list of possible 
return types--we also need the conditions that lead to those return types. A 
function with multiple branches should return a ConditionalType. This seems at 
odds with our AST-walking algorithm, unless we add more state into the 
`TypeCollector` object to inform calls higher up the tree that a return was 
detected.

In other words, we need to handle returns in `inferTypes.if()`, even though 
they'll be detected farther down in `returnTypes.call()`. We could set up a 
queue of returns in the `TypeCollector`. However, how do we handle nested if 
statements?

## 2015.08.04

Upcasting is used for inference in several different places, including 
operators and known functions. Perhaps the `upcast()` function in `utilities.R` 
can be generalized for reuse.

The primary motivation for using mixins or multiple inheritance in the type 
system is to easily attach information to a type. For instance, to mark a 
numeric type as a vector, we'd mix in a vector type. 

New idea: make all of the semantic types (where it makes sense) a composition 
of "base" types: character, numeric, integer, etc. Then we can use a 
`getBaseType()` method to reduce any semantic types to a base type, which makes 
upcasting a lot easier. Similarly, a `setBaseType()` method could be used to 
change the base type of a semantic type. The constructors for semantic types
could take a base type as an argument, so it's equally easy to go from base 
type to semantic type (for instance, to promote a scalar to a vector). With 
this approach, we can still use `is()` to check whether a type is a vector or 
not. Further thought is needed to address the case where a variable has 
multiple semantic types--for instance, a vector-valued iterator. These cases do 
seem to be uncommon, though.

For a function, `inferTypes()` should return the type of the returned value. 
This way, the caller can do whatever they need to with it. It's possible that 
this will make the slot for return type on `TypeCollector` unnecessary, but it 
might be good to keep anyways, for convenience.

We also need to support type annotations, possibly using a `.Type()` or similar 
at the beginning of a function. This should match the existing infrastructure 
in the compiler as closely as possible. The annotation function should be a 
no-op, or at least minimize interference with standard interpretation. Letting 
the user annotate types through an argument to `inferTypes()` could also be 
useful, but seems like a lower priority since it violates locality.

Once the features above are implemented, we should test them on case studies. 
These can be integrated into the test suite, as long as they don't take too 
long to test.

## 2015.07.31

ConditionalType is just a `phi()` in disguise. Maybe it would make more sense 
(especially with SSA) to treat it as such.

What's the correct behavior for assignment to a call? For instance:
```
names(x) = c("a", "b", "c")
```
For now we can just ignore inference in this situation, but this could be 
useful for picking up type information, e.g., with `dim<-`. Alternatively, 
these could be something that gets rewritten out, although that might be 
difficult if there are many assignment functions.

---

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

> Probably not; there's no clear reason for that to happen yet. And how would 
> we set the length parameter?

## `for` Loops

What's the correct inference for a simple for loop
```
for (i in 1:10) i
```
We should tag the `i` as an index variable. The body of the loop should be 
tagged normally.

What if the index is not even a number? We need to track the "real" type of the 
index along with its status as an index. This seems like more evidence in favor 
of using mixins to indicate type.

For now, we could do this using a slot on the IndexType.

## Calls (that are not type-stable)

How can we infer the type of a function like `rnorm`, where this depends on the 
**value** of input arguments?

We can use `formals` to inspect a function definition, and `standardise_call` 
from pryr to get the arguments in a named list.

We need a data structure for specifying known types. This requires 

Another fun function for this is `c()`.

One strategy is to define a ConditionalType. This type tracks conditions and 
their corresponding types. It could include a method for evaluating the type 
when more information is available. We could use a list or a simple S4 class 
for storing each condition and its corresponding type.

ConditionalType also makes sense for if statements.

What's the best way to check the conditions and compute the type? Chief 
concerns:

* Writing code to describe a conditional type should be easy, and easy to 
  automate (at least in terms of generating the code).
* Need to inspect some arguments, all arguments, or the number of arguments.
* May want to compute something in advance before checking conditions.
* Need a mechanism to specify arguments by position, not just name.

What if we stored conditional types as handler functions? Then "calling" the 
conditional type would evaluate the conditions. This cuts down on the 
non-standard evaluation needed, and makes specifying arguments by position 
trivial. However, generating this kind of conditional type is trickier: we'd 
need to generate the handler function. So handling the conditions dynamically 
becomes slightly harder, but this could be hidden by a set of helper methods 
for adding and querying the conditions.

Storing conditional types as handler functions could make it harder for the 
compiler to inspect the types on each branch. Types may be stored as a literal 
value or as a call to construct the type (and this is not any different from 
when not using handler functions). A call to construct a type really just 
represents partial information: we know the type class but not its parameters. 
Is there a better way to handle this information?



