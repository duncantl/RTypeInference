#' @include symbol_map.R
#' @include result.R
NULL


#' @export
setMethod("do_substitution", signature("RTypeInference::SymbolMap"),
function(term, sub)
{
  term@.Data = lapply(term@.Data, function(x) {
    defn_as = x$defined_as
    if (!is.null(defn_as))
      defn_as = sub(defn_as)

    used_as = lapply(x$used_as, sub)

    SymbolMapEntry(defn_as, used_as, x$is_parameter)
  })

  term
})


#' @export
setMethod("do_substitution", signature("RTypeInference::Result"),
function(term, sub)
{
  callGeneric(term@map, sub)
})
