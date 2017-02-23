# Description:
#   Functions to generate type constraint systems for a control-flow graph.


#' Infer Types for a Function
#'
#' @export
infer_types = function(code) {
  cfg = ast::to_cfg(ast::to_ast(code), in_place = TRUE)

  constraints = constrain(cfg)
  types = solve(constraints)

  return (types)
}
