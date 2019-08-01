#' @include symbol_map.R
NULL


#' @exportClass RTypeInference::Result
setClass("RTypeInference::Result",
  slots = list(
    constraints = "list"
    , map = "RTypeInference::SymbolMap"
  ))


#' @export
Result =
function(constraints, map)
{
  new("RTypeInference::Result", constraints = constraints, map = map)
}
