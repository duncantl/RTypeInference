library(rstatic)
library(RTypeInference)

rdexp =
function(x, lambda = 1.0)
{
    #FIXME - get the one w/o the () after the -
#  return(lambda * exp( - x * lambda))
  y = lambda * exp( - (x * lambda))
  return(y)
}

# Not clear we can tell if this a scalar or a vector of RealType.
# Can we make RTypeInference do this based on its knowledge of exp()

cfg = to_cfg(rdexp)
types = infer_types(cfg, scalar = TRUE)


