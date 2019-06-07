# FIXME: We might need this later.

FN_TYPES = list(
  # TODO: How do we handle unary operators?
##    "+" = c(a, b) ~ Join(a, b, Integer)
##  , "-" = c(a, b) ~ Join(a, b, Integer)
##  , "*" = c(a, b) ~ Join(a, b, Integer)
##  , "/" = c(a, b) ~ Join(a, b, Numeric)
##  , "^" = c(a, b) ~ Join(a, b, Numeric)

   ">"  = c(a, b) ~ RLogical # logical(0) in some cases
  , "<"  = c(a, b) ~ RLogical
  , ">=" = c(a, b) ~ RLogical
  , "<=" = c(a, b) ~ RLogical
  , "==" = c(a, b) ~ RLogical
  , "!=" = c(a, b) ~ RLogical
  , "!"  = a ~ RLogical # Allows integer/real/complex input

  , "&&" = c(a, b) ~ RLogical
  , "||" = c(a, b) ~ RLogical

  # TODO: Handle vectorized operations.
  # TODO: How can we describe optional types?
  # TODO: How can we use constants? `runif(1, ...)` always returns a scalar.
  #, "runif" = c(n, min, max) ~ Vec(Numeric)
)

mkNumberType =
function(args, rtype = "numeric")
{
   type = typesys::ArrayType(mapRTypeToTypesys(rtype))

   len = args[[1]]@value

   if(is(len, "typesys::SymbolValue") || is(len, "typesys::NumberType")) 
     type@dimension = list(len)

   type
}

#XXX Complete
## RTypeMap = list( logical = typesys::LogicalType,
##                  integer = typesys::IntegerType,
##                  numeric = typesys::NumericType) 
## mapRTypeToTypesys =
## function(rtype, map = RTypeMap)
## {
##   map[[rtype]]()
## }


CALL_HANDLERS = list(
  "+" = function(args, constraints = NULL) upcast_math(args, "+")
  , "-" = function(args, constraints = NULL) upcast_math(args, "-")
  , "*" = function(args, constraints = NULL) upcast_math(args, "*")
  , "/" = function(args, constraints = NULL) upcast_math(args, "/")
  , "^" = function(args, constraints = NULL) upcast_math(args, "^")
  , "%%" = function(args, constraints = NULL) upcast_math(args, "%%")

  , ">" = function(args, constraints = NULL) typesys::RLogical
  , "<" = function(args, constraints = NULL) typesys::RLogical
  , ">=" = function(args, constraints = NULL) typesys::RLogical
  , "<=" = function(args, constraints = NULL) typesys::RLogical
  , "==" = function(args, constraints = NULL) typesys::RLogical
  , "!=" = function(args, constraints = NULL) typesys::RLogical
  , "!" = function(args, constraints = NULL) typesys::RLogical
    
  , "&&" = function(args, constraints = NULL) typesys::RLogical
  , "||" = function(args, constraints = NULL) typesys::RLogical

  , "as.integer" = function(args, constraints = NULL) typesys::RInteger
    
  , "runif" = function(args, constraints = NULL) typesys::RNumeric
  , "Rf_runif" = function(args, constraints = NULL) typesys::RNumeric
##   , "Rf_allocVector" = function(args, constraints = NULL) typesys::SEXPType()
##   , "mkList" = function(args, constraints = NULL) typesys::LISTSEXPType()            
    
  , ":" = function(args, constraints = NULL) {
    from = args[[1]]
    to = args[[2]]

    type =
      if (is(from, "typesys::RNumeric")) # || is(to, "typesys::NumericType"))
        typesys::RNumeric
      else if (is(from, "typesys::RInteger"))
        typesys::RInteger
      else
        stop("Invalid types for `:`.")

    return (typesys::ArrayType(type, NA))
  }

  , "[[" = function(args, constraints = NULL) {
    x = args[[1]]
if(length(args) > 2) warning("need to handle nested list access for [[")
    return (typesys::element_type(x))
  }
  , "[" = function(args, constraints = NULL) {
    x = args[[1]]

if(length(args) > 2) warning("need to handle multi-dimensional access for [")    

    return (typesys::element_type(x))
  }    

  , "numeric" = function(args, constraints = NULL) mkNumberType(args, "numeric")
  , "integer" = function(args, constraints = NULL) mkNumberType(args, "integer")
  , "logical" = function(args, constraints = NULL) mkNumberType(args, "logical")    


  , "length" = function(args, constraints = NULL) {
    return (typesys::RInteger)
  }

  , "which" = function(args, constraints = NULL) {
    type = typesys::ArrayType(typesys::RInteger, NA)

    return (type)
  }

  , "list" = function(args, constraints = NULL) {
    type = typesys::ListType(args)

    return (type)
  }

  , "exp" = function(args, constraints = NULL) {
      browser()
    return (typesys::RNumeric)
  }
)


getSolveHandlers =
function(..., .default = CALL_HANDLERS)
{
   o = list(...)
   .default[names(o)] = o
   .default
}
    





# Old Definitions from Previous RTypeInference Version
# ----------------------------------------------------

# Type-dependent return type.
#"abs" = ConditionalType(
#  # complex|numeric -> numeric
#  # integer -> integer
#  function(args, constraints = NULL) {
#    # Here x should be an vector, not a list.
#    atom = element_type(args$x)

#    atom =
#      if (is(atom, "ComplexType") || is(atom, "NumericType"))
#        NumericType()
#      else if (is(atom, "IntegerType"))
#        IntegerType()

#    element_type(args$x) = atom
#    # FIXME:
#    value(args$x) = UnknownValue()
#    return(args$x)
#  }),

## Value-dependent return type.
#"rnorm" = ConditionalType(
#  function(args, constraints = NULL) {
#    makeVector(NumericType(), value(args$n, NA))
#  }),
#"numeric" = ConditionalType(
#  function(args, constraints = NULL) {
#    makeVector(NumericType(), value(args$length, NA))
#  }),

#"matrix" = ConditionalType(
#  function(args, constraints = NULL) {
#    # TODO:
#    #   * The default arguments should really be pulled using `formals()`.
#    #   * Propagate value if literal?
#    atom = element_type(args$data)

#    nrow =
#      if ("nrow" %in% names(args, constraints = NULL)) value(args$nrow)
#      else 1L
#    ncol =
#      if ("ncol" %in% names(args, constraints = NULL)) value(args$ncol)
#      else 1L
#    dimension = c(nrow, ncol)

#    value(atom) = UnknownValue()

#    makeVector(atom, dimension)
#  }),
#
#":" = ConditionalType(
#  # `:()` downcasts to integer whenever possible, and works on vector
#  # arguments by taking the first elements.
#  function(args, constraints = NULL) {
#    # FIXME: This function can also return NumericType.
#    x = args[[1]]
#    y = args[[2]]

#    length = abs(value(x, NA) - value(y, NA)) + 1
#    makeVector(IntegerType(), length)
#  }),
#"c" = ConditionalType(
#  function(args, constraints = NULL) {
#    # TODO: Propagate value if all args are literal?
#    length = sum(sapply(args, length))
#    makeVector(upcast(args, constraints = NULL), length)
#  })
