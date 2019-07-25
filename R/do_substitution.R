#' @include helper.R
NULL


#' @export
setMethod("do_substitution", signature("RTypeInference::InferHelper"),
function(term, sub)
{
  term@.Data = lapply(term@.Data, function(x) {
    def = x$def
    if (!is.null(def))
      def = sub(def)

    uses = lapply(x$uses, sub)

    helper_record(def, uses, x$is_parameter)
  })

  term
})
