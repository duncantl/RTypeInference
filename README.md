# RTypeInference

__RTypeInference__ provides functions to infer the types of variables in R
code.

A "type" is a set of values that have common representation (in memory) and
behavior.

Types are useful for error detection. For instance, dividing one
string by another, `"hi" / "bye"` does not make sense because division is
not defined for strings. This is an example of a type error. Catching type
errors is so important that many languages require programmers to specify a
type whenever they define a variable. Type errors can be detected before the
code runs if all the types are known.

In contrast, R does not require programmers to specify a type when they define
a variable. Instead, R detects type errors at run-time as they occur. The
drawback is that some type errors may go unnoticed until long after code is put
into use.

Types are also useful for optimization and documentation of code.

This package can determine the types of many variables in R code even when the
types are not specified. The idea is to take advantage of knowledge we have
about functions that are built into the language. For example, we know that the
`length()` function always returns an integer, so if we see a definition `y =
length(x)` then we know `y` must be an integer (regardless of the value of
`x`).

__RTypeInference__ uses a modified [Damas-Milner][] type system for inference.
Code analysis tools are provided by __[rstatic][]__ and algebraic data types
are provided by __[typesys][]__. In the future this package may also accept
type annotations from the __[types][]__ package where types cannot be inferred.


[Damas-Milner]: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
[rstatic]: https://github.com/nick-ulle/rstatic
[typesys]: https://github.com/nick-ulle/typesys
[types]: https://github.com/jimhester/types


## Installation

__RTypeInference__ is unstable and under active development, so it's not yet
available on CRAN. To install, open an R prompt and run:

```r
install.packages("devtools")

devtools::install_github("duncantl/RTypeInference")
```
