#' @include result.R
NULL


#' @export
setMethod("csolve", "RTypeInference::Result",
function(x, counter = x@map@counter, ...)
{
  callGeneric(x@constraints, counter, ...)
})
