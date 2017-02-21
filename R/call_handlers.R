
CALL_HANDLERS = list(
  "+" = function(args) {
    x = args[[1]]
    y = args[[2]]

    type =
      if (is(x, "ComplexType") || is(y, "ComplexType"))
        typesys::ComplexType()
      else if (is(x, "RealType") || is(y, "RealType"))
        typesys::RealType()
      else if (is(x, "IntegerType") || is(y, "IntegerType"))
        typesys::IntegerType()
      else if (is(x, "BooleanType") || is(y, "BooleanType"))
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
      if (is(from, "RealType") || is(to, "RealType"))
        typesys::RealType()
      else if (is(from, "IntegerType"))
        typesys::IntegerType()
      else
        stop("Invalid types for `:`.")

    return (typesys::ArrayType(type, NA))
  }

  , "[[" = function(args) {
    x = args[[1]]

    return (typesys::element_type(x))
  }
)
