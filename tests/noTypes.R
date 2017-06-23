
foo =
function(x, y)
 x + y

cfg = rstatic::to_cfg(foo)
types = RTypeInference::infer_types(cfg)
