# RTypeInference

An R package for type inference on R code.

## Description

The data types used in a program can reveal whether the program is valid and
ways the program can be improved. Type information is also helpful for
translating between languages, since some languages require explicit type
annotations in the syntax.

R doesn't include type annotations in the language. This is justified because R
supports interactive workflows, where brevity is an important consideration.
The R interpreter checks that programs are correctly typed at run-time. In
other words, the interpreter makes _dynamic_ type checks because type
information isn't available before run-time. 

This package uses type inference to gather type information for R programs
before run-time. The inference system is loosely based on Hindley-Milner type
systems, although the primary goal of the package is to be useful rather than
provably correct.

The steps taken by the inference algorithm are:

1. A _type variable_ is created for each program variable.
2. The program's control-flow graph is traversed to generate a system of
   constraints for each type variable.
3. The constraint systems are solved to determine a type for each program
   variable.

The types of the program variables are returned in a dictionary. Since the
algorithm expects a control-flow graph in single static-assignment form, each
program variable corresponds to exactly one definition and thus one type at
run-time.

The greatest obstacle to inference is that R allows variables to take different
types on branches:

```r
if (some_condition)
  x = 10
else
  x = "Hello"

print(x)
```

Variables that may have any of several different types after a control flow
merge can blow up the number of types possible for other variables.


## Old Description

We want to determine the types of the inputs and output of a function.
For the return type, we can look at the return type of the last
expression.  The type of the result of that expression may depend on
the inputs.  As a result, we have to recursively process all of the
expressions in a function.

We also want to identify input parameters that are 'constant',
i.e. not modified by the function. Of course, in R the parameters are
copied and so we don't modify the original object. However, it is
useful to know whether the function actually uses the parameter as
read-only or if it actually modifies the object.  We can use this to
avoid copying the value in the call, e.g. when we compile.

We also want to identify parameters that are truly modified in a
non-functional manner, e.g. a reference object or a closure that may
mutate variables in its environment(s).

We want to store the type information so we can query it at different
times.  We can store this information on each function, e.g. using the
TypeInfo package.  We can also store it in a global variable, or in a
separate database.


See the Hindley-Milner algorithm.
