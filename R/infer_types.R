# Description:
#   Functions to generate type constraint systems for a control-flow graph.


#' Infer Types for a Function
#'
#' @export
infer_types = function(code) {
  cfg = ast::to_cfg(ast::to_ast(code))
  cfg = ast::ssa(cfg)

  constraints = constrain(cfg)
  types = solve(constraints)

  return (types)
}
