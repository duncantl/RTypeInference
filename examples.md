
# Examples

## 2015.07.31

```
for (i in 1:10)
  x = i
```
We label `i` as an IteratorType, but should we label `x` as an IteratorType? In 
a less contrived example, `x` might be computed from `i`, so probably not. On 
the other hand, we want to tell the compiler that `x` changes on every 
iteration.

## 2015.07.31 Array Assignment

```
x[3] = 5
```
In this example, we can infer that `x` is a list or a numeric vector, but 
little else.

## 2015.07.31 Assignment Functions

```
dim(x) = c(5, 4)
```
This assignment gives us metadata about the type of `x`, but not the actual 
type. For now, we can ignore the information and simply return the type of the 
right-hand side (in case this will be used by the caller).

## 2015.07.26 Variable Changes Type in Branch

Consider the example:
```
x = 42L

if (...)
  x = "hello"

foo(x)
```

## 2015.07.26 Length

Consider the slightly pathological example:
```
len = rpois(1, 8)
x = rep(42L, len)

length(x)
```
Because the call to `length()` could appear at any later point in the code, we 
need to tell the compiler that it should associate the length of `x` with the 
variable `len`.

The situation becomes more complicated if `len` may be reassigned:
```
len = rpois(1, 8)
x = rep(42L, len)

# ...

len = 20
length(x)
```
Here we need to ensure that the compiler uses a new name for the second version 
of `len`. This case is closely related to the example proposed by Duncan where 
the type of a variable changes at some point in the program.

This example is an amalgamation of the `rnorm()` and the variable changes type 
examples.

This example suggests that we might want to use single static assignment from 
the beginning. That is, before doing any inference and compilation, rewrite the 
R code being compiled. There are no obvious, major drawbacks to this approach, 
except perhaps the time it would take to set up the rewrite rules.

Using SSA is a mixed bag for type inference and compilation. On one hand, we'd 
no longer need to worry about name clashes in the symbol table--not even 
scoping issues. On the other hand, SSA complicates tracking data flow in the 
code, which could have unexpected consequences for compilation, especially with 
regards to loop fusion and other parts of the compiler that modify code.

The numerous rewrites we're applying before compilation suggest that we may 
want to define a "simplified" subset of the R language as our input for 
compilation, and use a separate rewrite package to get us there. This subset 
would enforce things like SSA, use of `return()`, format of if-statements, and 
other optional but potentially nuisance syntax features.

## 2015.07.23 Template Functions and Branching Type

Suppose the type of a variable depends on a runtime condition:
```
z =
  if (...)
    "Hello"
  else
    42L

foo(z)
```
If `foo()` is a template function--meaning the type of its argument is 
parameterized, so behavior is different depending on type--then the compiler 
will need to compile each version used separately, and call the correct 
version.

In simple cases, we could rewrite this example:
```
if (...) {
  z = "Hello"
  foo_character(z)
} else {
  z = 42L
  foo_integer(z)
}
```
However, this requires us to keep a reference to the line where `z` was 
defined, which is challenging because any changes to the AST (rewrites) will 
necessitate an update of references across the entire type inference system.

Alternatively, we could treat the type of `z` as an actual parameter:
```
if (...) {
  z = "Hello"
  type_z = "character"
} else {
  z = 42L
  type_z = "integer"
}

foo(z, type_z)
```
In this case, there are two branches in the code--one is hidden in `foo`--so 
performance may not be as good. The branch hidden in `foo` could just as well 
be lifted out:
```
if (type_z = "character")
  foo_character(z)
else if (type_z = "integer")
  foo_integer(z)
```
This benefit of this approach is that the type inference system can rewrite the 
if-statement when it's encountered, and there's no need to keep references. The 
name of the variable `type_z` can then be stored in the type information for 
`z`, so that when the compiler encounters a line using `z`, it can generate the 
necessary branching instructions. In the actual implementation, we would 
probably want to use an enumeration rather than characters for the type 
information.

## 2015.07.09 Variable Changes Type

Duncan pointed out the it's possible a variable's type will change midway 
through a program:
```
x = 8L

# ...

x = "Hello there!"
```
The obvious way to handle this in the compiler is to use a different name for 
the second version of `x`, mimicking SSA.

## 2015.07.02 Type Depending on Argument Value: `rnorm()` and Friends

There are several functions whose type depends on the value of the arguments. 
Duncan has referred to these as *value-dependent* functions. An example is 
`rnorm()`, which returns a numeric scalar or a numeric vector of specified 
length.

This example is easy to deal with when the value of the argument is known at 
compile-time. However, often that's not the case:
```
n = read.table("data.txt")
y = rnorm(n)
```
Allocation for `y` is a problem--the compiled code will need to inspect `n` to 
determine this.

We might assign `n` to a length slot on the type information for `y`, and then 
have the compiler generate the appropriate code (shown in C here, for brevity):
```
if (n == 1)
  double y = rnorm(n);
else
  // Or allocate y separately.
  double[] y = rnorm(n);
```
