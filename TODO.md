
# TODO

* Allow the user to specify types for specific variables. Need to work around the SSA.
* Control whether the results are for use in R and hence SEXPs or part of a larger computation. This
  can get done when computing the types and so in infer_types().

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
