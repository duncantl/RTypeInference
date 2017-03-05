
CALL_HANDLERS = list(
  "+" = function(args) {
    x = args[[1]]
    y = args[[2]]

    type =
      if (is(x, "typesys::ComplexType") || is(y, "typesys::ComplexType"))
        typesys::ComplexType()
      else if (is(x, "typesys::RealType") || is(y, "typesys::RealType"))
        typesys::RealType()
      else if (is(x, "typesys::IntegerType") || is(y, "typesys::IntegerType"))
        typesys::IntegerType()
      else if (is(x, "typesys::BooleanType") || is(y, "typesys::BooleanType"))
        typesys::IntegerType()
      else
        stop("Invalid types for `+`.")

    # TODO: vectors
    return (type)
  }

  , ":" = function(args) {
    from = args[[1]]
    to = args[[2]]

    type =
      if (is(from, "typesys::RealType") || is(to, "typesys::RealType"))
        typesys::RealType()
      else if (is(from, "typesys::IntegerType"))
        typesys::IntegerType()
      else
        stop("Invalid types for `:`.")

    return (typesys::ArrayType(type, NA))
  }

  , "[[" = function(args) {
    x = args[[1]]

    return (typesys::element_type(x))
  }

  , "numeric" = function(args) {
    type = typesys::ArrayType(typesys::RealType(), NA)

    return (type)
  }

  , "length" = function(args) {
    return (typesys::IntegerType())
  }

  , "which" = function(args) {
    type = typesys::ArrayType(typesys::IntegerType(), NA)

    return (type)
  }
)


# Old Definitions from Previous RTypeInference Version
# ----------------------------------------------------

# Type-dependent return type.
#"abs" = ConditionalType(
#  # complex|numeric -> numeric
#  # integer -> integer
#  function(args) {
#    # Here x should be an vector, not a list.
#    atom = element_type(args$x)

#    atom =
#      if (is(atom, "ComplexType") || is(atom, "RealType"))
#        RealType()
#      else if (is(atom, "IntegerType"))
#        IntegerType()

#    element_type(args$x) = atom
#    # FIXME:
#    value(args$x) = UnknownValue()
#    return(args$x)
#  }),

## Value-dependent return type.
#"rnorm" = ConditionalType(
#  function(args) {
#    makeVector(RealType(), value(args$n, NA))
#  }),
#"numeric" = ConditionalType(
#  function(args) {
#    makeVector(RealType(), value(args$length, NA))
#  }),

#"matrix" = ConditionalType(
#  function(args) {
#    # TODO:
#    #   * The default arguments should really be pulled using `formals()`.
#    #   * Propagate value if literal?
#    atom = element_type(args$data)

#    nrow =
#      if ("nrow" %in% names(args)) value(args$nrow)
#      else 1L
#    ncol =
#      if ("ncol" %in% names(args)) value(args$ncol)
#      else 1L
#    dimension = c(nrow, ncol)

#    value(atom) = UnknownValue()

#    makeVector(atom, dimension)
#  }),
#
#":" = ConditionalType(
#  # `:()` downcasts to integer whenever possible, and works on vector
#  # arguments by taking the first elements.
#  function(args) {
#    # FIXME: This function can also return RealType.
#    x = args[[1]]
#    y = args[[2]]

#    length = abs(value(x, NA) - value(y, NA)) + 1
#    makeVector(IntegerType(), length)
#  }),
#"c" = ConditionalType(
#  function(args) {
#    # TODO: Propagate value if all args are literal?
#    length = sum(sapply(args, length))
#    makeVector(upcast(args), length)
#  })
