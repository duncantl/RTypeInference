## ---- echo=FALSE, results="hide"-----------------------------------------
library(RTypeInference)

## ---- eval = FALSE-------------------------------------------------------
#  ones = function(n) {
#    rep.int(1, n)
#  }

## ------------------------------------------------------------------------
circle_area = function(r_inner, r_outer) {
  outer = pi * r_outer^2
  inner = pi * r_inner^2

  return (outer - inner)
}

## ------------------------------------------------------------------------
cfg = rstatic::to_cfg(circle_area)

cons = constrain(cfg)

