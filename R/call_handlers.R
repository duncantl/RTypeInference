
call_handlers = list(

  length = function(node, soln, ...) {
    return (IntegerType())
  }

  ,
  "+" = function(node, soln, ...) {
    args = get_arg_types(node, soln)

    x = args[[1]]
    y = args[[2]]
    
    type =
      if (is(x, "character") || is(y, "character"))
        NULL
      else if (is(x, "ComplexType") || is(y, "ComplexType"))
        ComplexType()
      else if (is(x, "RealType") || is(y, "RealType"))
        RealType()
      else if (is(x, "IntegerType") || is(y, "IntegerType"))
        IntegerType()
      else if (is(x, "BooleanType") || is(y, "BooleanType"))
        IntegerType()
      else
        NULL

    # TODO: vectors
    return (type)
  }

)
