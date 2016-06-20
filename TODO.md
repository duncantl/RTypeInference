
# TODO:

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
