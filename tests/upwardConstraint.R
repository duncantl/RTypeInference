# Poor name for this. Rename the file later.

# The idea is that code of the form
f =
function(n)
{
  x = runif(n, 1, 2)
}

# or
g =
function(n)
   x = numeric(n)
# or
# 1:n
# implies that n is an integer and so we should add that constraint.
        
library(rstatic)
library(RTypeInference)

debug(RTypeInference:::constrain_ast)
types = infer_types(to_cfg(g))
