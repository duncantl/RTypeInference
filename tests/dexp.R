library(rstatic)
library(RTypeInference)

rdexp =
function(x, lambda = 1.0)
{
  lambda * exp( - x * lambda)
}

# Not clear we can tell if this a scalar or a vector of RealType.
# Can we make RTypeInference do this based on its knowledge of exp()

cfg = to_cfg(rdexp)
types = infer_types(cfg)

