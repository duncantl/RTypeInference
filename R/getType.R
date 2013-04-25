# ctx might be a reference class that allows us to collect
#   information as we traverse expressions, functions, etc.
#  May want a global context and within function context
setGeneric("inferTypes",
              function(x, ctx, ...)
                  standardGeneric("inferType"))

setMethod("inferTypes",
          c("function"),
          function(x, ctx, ...) {

          })

                             
