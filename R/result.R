#' @exportClass RTypeInference::Result
setClass("RTypeInference::Result",
  slots = list(
    constraints = "list"
    , helper = "RTypeInference::InferHelper"
  ))


#' @export
Result =
function(constraints, helper)
{
  new("RTypeInference::Result", constraints = constraints, helper = helper)
}
