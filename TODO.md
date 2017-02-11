
# TODO

Defer inference as long as possible. Use an UnresolvedCall class to represent
calls that need detection. 

* [ ] Package-level documentation.
* [ ] Examples for type detection.
* [ ] Write up strategy used with examples.
* [x] Detect types for literals and symbols.
* [ ] Detect types for control flow.
    + [ ] If statements generate IfType when type depends input type.
    + [ ] Loops might change type of variable many times.
* [ ] Detect types for calls.
    + [ ] Get called functions and apply inference.
    + [ ] RuntimeType when type depends on input values
    + [ ] Special handling for index operations.
* [x] Frontend for solving constraint systems.
* [ ] Show more constraint info when InferState is printed.
* [ ] Use annotations from Jan & team (2DRW & other examples - ambiguity).
  * [ ] Static checking example.

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
