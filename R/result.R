#' @include symbol_map.R
NULL


#' @exportClass RTypeInference::ConstrainResult
setClass("RTypeInference::ConstrainResult",
  slots = list(
    type = "typesys::Term"
    , constraints = "list"
    , map = "RTypeInference::SymbolMap"
  ))


#' @export
Result =
function(type, constraints, map)
{
  new("RTypeInference::Result", type = type, constraints = constraints
    , map = map)
}
