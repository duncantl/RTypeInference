# TODO

<!-- [R2llvm](/Users/duncan/NickUThesis/RTypeInference/TODO.md) -->

* [half-done] For the init parameter of infer_types() match by position if no names given.  match.call()

* dexp.R example in corsair/tests/dexp.R  - why aren't we getting a return_type().

* Add call handlers for constrain() so can handle injecting constraints for calls to functions such as numeric(n), 
  runif(n) which would add constraints on n. 
  Also vector('type', len)
  
* Unify method for two typesys:: type objects. Understand what SolutionSet should be for this and
  where the name fits in?
  
* [??] If we see numeric(n), then we'd like to say n should be an integer,
  but it could be a Real, but let it be a Real if we already know something about it.
  Maybe emit Union()? Is the Union() the right thing here?
  
* See tests/dexp.R and can we infer type for x.  Can scalar = TRUE tell us more - yes!

* How to deal with scalars and vectors.  Can't tell from a function - e.g. dexp.R
  Use the scalar parameter we just added. 
  Should get passed to 

* Allow caller to specify signatures of other functions and also routines. 
  Do we have to separate these?  We need pointers for C routines.
  
* Introduce pointer type in typesys.

* Propogate signature from one or more functions to other calls. 

* Consider recursive calls fib().  Can we infer signature and then pass it down to recursive calls.

* [Partially done] Allow the user to specify types for specific variables. Need to deal with the SSA
  naming system. Currently inelegant.

* [Low] Control whether the results are for use in R and hence SEXPs or part of a larger computation. This
  can get done when computing the types and so in infer_types().
  The ArrayType[ element type ] allows us to do this.

* [Low] solve() fails if there are two identical constraints in the ConstraintSet. See
  tests/dupConstraints.R if we don't have the code in ConstrainSet$append() checking for identical.

Defer inference as long as possible. Use an UnresolvedCall class to represent
calls that need detection. 

* [ ] Package-level documentation
  + [ ] Update vignettes for new interface

* [ ] Add/update tests for new interface
  + [ ] Test math handlers on vectors

* [ ] Use annotations from Jan & team (2DRW & other examples - ambiguity)
  * [ ] Static checking example

* [ ] Provide parameter for list of call handler overrides

* [ ] Constraint generation for `Brace`

<!-- Old Stuff
## Old Stuff

* Tests for larger bodies of code (not just snippets).
* Support for `[`.
    + Non-scalar indexes.
* Support extraction functions `[[` and `$`.
    + Distinguish data frame / matrix based on these.
* Support for multidimensional `[`.
    + Equivalent to `[[` followed by `[` for data frames.
* Detect variables used as indexes in subset operations.
* Document exported functions.
* Support for `read.csv()`, `readRDS()`, etc via the `colClasses` parameter.
* Support for inference based on assertions.

* Generalize condition reduction to type system rather than case-by-case.

* Infer metadata from assignment functions such as `dim<-`
* Detect branching `return()` behavior and branch conditions; currently we just 
  assume user functions are type stable.
* Detect iterator patterns in while loops.
* Consolidate code for handling math operations with RLLVMCompile.
* Mark aggregate (sum, min, max, range, sd, mean) results
-->
