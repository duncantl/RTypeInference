catchAllConstrain =
function(node, set, scalar, handler)
{
                                   #XXX remove "g" - just testing.
  if((node$fn$name %in% c("numeric", "integer", "logical", "character", "runif", "rexp", "g")) &&
       is(node$args[[1]], "Symbol") ) {
         # Need to match the argument by name for other functions, eg. runif(n, 1, 2)
         # but  runif(1, 2, n = n1)
      set$append(node$args[[1]]$name, typesys::IntegerType())
  }
}

ConstraintHandlers =
list(
    catchAllConstrain
  , ":" = function(node, set, scalar, handler) {
             sapply(node$args[1:2], function(x)
                                      if(is(x,"Symbol"))
                                         set$append(x$name, typesys::IntegerType()))
          }

    )    

getConstraintHandlers =
function(..., .default = ConstraintHandlers)
{
   o = list(...)
   .default[names(o)] = o
   .default
}

