
# Pending:

* Tests for larger bodies of code (not just snippets).
* Inference for while loops.
* Support extraction functions `[[` and `$`.
* Support for multidimensional `[`.

# Deferred:

* Infer metadata from assignment functions such as `dim<-`
* Detect branching `return()` behavior and branch conditions; currently we just 
  assume user functions are type stable.
