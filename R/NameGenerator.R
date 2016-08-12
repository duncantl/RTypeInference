
#' Generate Unique Variable Names
#'
NameGenerator = R6::R6Class("NameGenerator",
  public = list(

    get = function(x) {
      counter = private$counter[x]
      if (is.na(counter)) {
        counter = 1
      } else {
        counter = counter + 1
      }

      private$counter[x] = counter
      return (paste0(x, counter))
    },

    reset = function() {
      counter = numeric(0)
    }
  ),
  private = list(
    counter = numeric(0)
  )
)

