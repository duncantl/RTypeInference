library(rstatic)
library(RTypeInference)

f =
function(x = 1L)
{
   a = x + 2L
   c = g(a)
   return(c * 2L)
}

g =
function(y)
  return(as.integer(y) + 3L)

g.cfg = to_cfg(g)
g.types = infer_types(g.cfg, scalar = TRUE)

cfg = to_cfg(f)
types = infer_types(cfg, scalar = TRUE)

