
setClass("RTypeInference::AssumptionSet", "list")

AssumptionSet = function(name, term) {
  if (missing(name) && missing(term)) {
    new("RTypeInference::AssumptionSet")

  } else {
    data = list(term)
    names(data) = name

    new("RTypeInference::AssumptionSet", data)
  }
}

setGeneric("setdiff")

setMethod("setdiff", signature("RTypeInference::AssumptionSet", "character"),
  function(x, y) {
    idx = match(y, names(x), 0L)
    idx = idx[idx != 0L]
    if (length(idx) > 0L) {
      x@.Data = x@.Data[-idx]
    }

    x
  })
