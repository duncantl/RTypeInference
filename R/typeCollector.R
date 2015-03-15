typeInferenceCollector =
    setRefClass("TypeInferenceCollector",
                fields = c(varTypes = "list",
                           returnTypes = "list"),
                methods = list(
                        addType = function(name, types) {
                            n = length(varTypes) + 1L
                            varTypes [[ n ]] <<- types
                            names(varTypes)[n] <<- name
                        },

                        getType = function(name) {
                             i = match(as.character(name), names(varTypes))
                             if(is.na(i))
                                 return(NA)
                              varTypes[[ max(i) ]]
                        },

                        addReturn = function(type) {
                            returnTypes[[ length( returnTypes) + 1L ]] <<- type
                        }
                    ))
