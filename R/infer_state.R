
InferState = R6::R6Class("InferState",
  public = list(
    namegen = NULL,
    live = NULL,
    constraints = NULL,

    initialize = function() {
      self$namegen = NameGenerator$new()
      self$live = list()
      self$constraints = list()
    },

    assign_type = function(name) {
      type = self$namegen$get(name)
      self$live[[name]] = type
      return (type)
    },

    get_type = function(name) {
      # Look up the variable name.
      type = self$live[[name]]
      if (is.NULL(type))
        return ("???")
      else
        return (type)
    },

    add_constraint = function(left, right) {
      n = length(self$constraints)
      self$constraints[[n + 1]] = list(left, right)
    }
  )
)
