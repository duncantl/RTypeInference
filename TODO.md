# TODO

* Generate a default top TypeEnvironment based on the global environment, or
  else add type signatures to this TypeEnvironment on demand.
    - [partially done] Handle global variables such as in `function() 1 + pi`

* Unify function parameters by name (right now we only unify by position)

* Allow the user to specify types for specific parameters or variables by
  basename rather than SSA name.

* Use literal values to get additional type information. E.g., `rnorm(1, ...)`
  is always a scalar.

* [partially done] Branches
    - When a variable `x` is defined on different branches, unify the types of
      the definitions. If they are different types then unification will fail.
      What should we do in this case? It depends whether RTypeInference is
      primarily a static type checker or not. We could take the Siek & Taha
      gradual typing approach of having a dynamic "ANY" type.

* Subtyping
    - When passing a real (literal or not) to `numeric()`, we already know that
      `numeric()` expects an IntegerType.

* Arrays
    - Symbolic length (for feature parity with old inference)
    - `x = numeric(2)` gives an ArrayType with the length unknown. Why isn't the
      length propagated? `RTypeInference::infer_types(function() numeric(2))`

* Type signatures for specific functions
    - `x[1L]` where `x` is `ArrayType(RealType())` should return `RealType()`.
      More generally, check whether the second argument to `[` is a scalar and
      return a scalar if it is.

* Deal with literals that are nominally numeric
    - E.g. `1:10`. Both 1 and 10 are integers here.
    - Type of `i` in `for(i in 1:n)` is currently a RealType. But the 1 is
      clearly an integer here. So do the right thing.  And identify as a
      counter.
    * If we see `numeric(n)`, then we'd like to say `n` should be an integer,
      but it could be a real if we already know something about it.

* [x] Example in corsair/tests/dexp.R 
    * [x] Why aren't we getting a return_type()? No return() probably?
    * Can we infer type for `x`? Sort of -- we can tell `x` is a number but not
      whether it is complex/real/integer/logical.
    * How to distinguish scalars from vectors? We can use a parameter `scalar`
      to tell us more. This is essentially an annotation applied to all
      variables (or just all parameters?).

* ???
    - When the type of `i` in a for loop is real, we should get IntegerType + i
      gives RealType not a Union. See tests/for.R in the case i has RealType
      (until we fix that).
    - [half-done] For the init parameter of infer_types() match by position if
      no names given.  `match.call()`
    - [possibly done?] Propagate signature from one or more functions to other
      calls. 

* [done?] Add call handlers for constrain() so can handle injecting constraints
  for calls to functions such as numeric(n), runif(n) which would add
  constraints on n. Also vector('type', len)
 
* We need pointers for C routines. Introduce pointer type in typesys.

* Consider recursive calls, e.g., `fib()`. Can we infer a signature and then
  pass it down to recursive calls?

* [low] Control whether the results are for use in R and hence SEXPs or part of
  a larger computation. This can get done when computing the types and so in
  `infer_types()`. The `ArrayType[ element type ]` allows us to do this.

* [partially done] Allow caller to specify signatures of other functions and
  also routines. Do we have to separate these?

* [ ] Package-level documentation
  + [ ] Update vignettes for new interface

* [x] Add/update tests for new interface
  + [ ] Test math handlers on vectors

* Annotations from __types__ package (but this is mostly a task for
  __rstatic__)

* [ ] Provide parameter for list of call handler overrides
