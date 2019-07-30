#' @include result.R
NULL


#' @export
setMethod("csolve", "RTypeInference::Result",
function(x, counter = x@helper@counter, ...)
{
  callGeneric(x@constraints, counter, ...)
})
