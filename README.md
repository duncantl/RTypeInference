RTypeInference
==============

Tools for inferring the types of inputs and outputs for functions and expressions


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