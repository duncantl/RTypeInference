library(rstatic)
library(RTypeInference)

f =
function(n)
{
    x = numeric(n)
    y = numeric(n)
    list(x, y)
}

t = infer_types(to_cfg(f))

