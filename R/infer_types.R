# Description:
#   Functions to generate type constraint systems for a control-flow graph.


#' Infer Types for a Function
#'
#' @export
infer_types = function(code) {
  UseMethod("infer_types")
}

#' @export
infer_types.CFGraph = function(code) {
  constraints = constrain(code)
  types = solve(constraints)

  return (types)
}

#' @export
infer_types.default = function(code) {
  cfg = rstatic::to_cfg(rstatic::to_ast(code), in_place = TRUE)

  infer_types(cfg)
}
