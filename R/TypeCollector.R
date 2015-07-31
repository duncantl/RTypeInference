# Description:
#   Reference class for collecting type information.

TypeCollector =
setRefClass("TypeCollector",
  fields = list(
    varTypes = "list",
    returnTypes = "list"
  ),

  methods = list(
    addType = function(name, types) {
      # TODO: Check if symbol already exists.
      #
      # Previously this method could add the same name multiple times. This
      # behavior is unnecessary if using SSA; it could be useful, albeit
      # unwieldy, otherwise.
      varTypes[[name]] <<- types
    },

    getType = function(name) {
      idx = match(as.character(name), names(varTypes))
      if(is.na(idx))
        NA
      else
        varTypes[[idx]]
    },

    addReturn = function(type) {
      returnTypes[[ length( returnTypes) + 1L ]] <<- type
    }
  )
)
