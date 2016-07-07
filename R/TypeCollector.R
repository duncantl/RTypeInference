# Description:
#   Reference class for collecting type information.

#' TypeCollector
#'
#' This reference class stores type information gathered by the type inference
#' algorithm.
#'
#' @export TypeCollector
TypeCollector =
setRefClass("TypeCollector",
  fields = list(
    variable_types = "list",
    return_types = "list"
  ),

  methods = list(
    setVariableType = function(name, type, force = FALSE)
      # Set the type of a variable.
      #
      # TODO: 
    {
      name = as.character(name)
      index = match(name, names(variable_types))

      if (is.na(index))
        variable_types[[name]] <<- type
      else if ( force || is(variable_types[[index]], "UnknownType") )
        variable_types[[index]] <<- type
      #else
      #  warning(
      #    sprintf("'%s' has type %s, not updating as %s.",
      #      name, class(variable_types[[index]]), class(type))
      #  )
    },

    mergeVariableTypeList = function(type_list, force = FALSE)
      # Merge a list of variable types.
    {
      if (length(type_list) > 0)
        mapply(.self$setVariableType, names(type_list), type_list, force,
          SIMPLIFY = FALSE)
    },

    getVariableType = function(name) {
      name = as.character(name)
      index = match(name, names(variable_types))

      if(is.na(index))
        UnknownType()
      else
        variable_types[[index]]
    },

    addReturn = function(type) {
      return_types[[ length(return_types) + 1L ]] <<- type
    }
  )
)
