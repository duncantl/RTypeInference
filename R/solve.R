
#' @export
setGeneric("solve_types",
function(x, ...)
  standardGeneric("solve_types")
)


#' @export
setMethod("solve_types", "RTypeInference::Result",
function(x, ...)
{
  typesys::solve(x@constraints, x@helper@counter)
})
