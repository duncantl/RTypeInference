# Description:
#   Core type inference functions.
#
# TODO:
#   * What about non-assignments? ie, the last line / return line.
#   * Resolve any variable references.
#   * Figure out the types of returns.
#   * Should typeCollector always have a default?
#   * Unify return type.
#   * Scoping?

# inferTypes
# =========

inferTypes =
function(x, typeCollector = TypeCollector(), ...)
  # Walk the tree and make a symbol table.
{
  UseMethod("inferTypes")
}


inferTypes.function =
function(x, typeCollector = TypeCollector(), ...)
{    
  body = body(x)

  # Rewrite with { and delegate work to inferTypes.{
  # TODO: Move to rewrite package?
  if(class(body) != "{")
    body = substitute({body}, list(body = body))
  
  # Return last value. What we really need to do is work with the CFG, so exit
  # blocks will always have only one return type.
  inferTypes(body, typeCollector)
}


`inferTypes.<-` = `inferTypes.=` =
function(x, typeCollector = TypeCollector(), ...)
{
  # When we see an assignment, try to infer the type of the RHS and then add
  # the name on the LHS to the symbol table.
  # Arrays and function call assignments `foo(x)<-` are special cases.

  # Try to infer type of RHS. This is potentially recursive.
  rhs = x[[3]]
  type = inferTypes(rhs, typeCollector, ...)
  # Get the atomic type for iterator types.
  if (is(type, "IteratorType"))
    type = atomicType(type)

  # Update the symbol table.
  lhs = x[[2]]

  if (class(lhs) == "name") {
    name = as.character(lhs)
    typeCollector$addType(name, type)

  } else if (class(lhs) == "call") {
    # Array or function assignment. For now, do nothing.
  }

  # Handle return from recursive x = y = value
  # The order of the definitions is reversed here since we process
  # y = value before x = y
  # We control this and could do it in the appropriate order.
  #if(is.call(rhs) && as.character(rhs[[1]]) %in% c("=", "<-")) {
      # FIXME: Doesn't yet handle x = y[i] = value
      # use getVarName(x[[3]][[2]]) ?
  #    var_type = typeCollector$getType(var_type)
  #} 

  #if(is.call(x[[2]])) {
  #    varname = getVarName(x[[2]])
  #    ty = UpdateType(var_type, varname)
  #    typeCollector$addType(varname, ty)
  #} else {
  #    varname = as.character(x[[2]])[1] 
  #    typeCollector$addType(varname, var_type)
  #}

  return(type)
}


inferTypes.call =
function(x, typeCollector = TypeCollector(), ...)
{
  # Math and logical operators
  # This is quite similar to what we are doing in the RLLVMCompile so we
  # should consolidate the code.
  call_name = as.character(x[[1]])

  # TODO: 
  #   * Move all call handlers to a separate file.
  #   * Clean up this mess.
  if(call_name == "return")
    typeCollector$addReturn(inferTypes(x[[2]], typeCollector, ...))
  else if (call_name == ".typeInfo") {
    # Get types from annotation and add to collector.
    type_list = evalTypeInfo(x)
    typeCollector$mergeTypeList(type_list)
    # TODO: Unclear what type we should return for this.
    NullType()
  } else if(call_name == "[")
    inferSubsetType(x, typeCollector, ...)
  else if(call_name %in% MATH_OPS)
    inferMathOpType(x, typeCollector, ...)
  else if(call_name %in% LOGIC_OPS)
    inferLogicOpType(x, typeCollector, ...)

  else if(call_name %in% names(knownFunctionTypes)) {
    type = knownFunctionTypes[[ call_name ]]

    if (is(type, "ConditionalType")) {
      # Infer argument types and pass to handler.
      arguments = standardise_call(x)[-1]
      arg_types = lapply(arguments, inferTypes, typeCollector, ...)
      type = infer(type, arg_types)
    }

    return(type)
  } else
    UnknownType()
}


inferTypes.name =
function(x, typeCollector = TypeCollector(), ...)
{
  # TODO: Constants are already propagated, but we might want to set up a
  # reference for to the variable for non-constants.

  # Try to retrieve type.
  typeCollector$getType(x)
}


# Flow Control --------------------------------------------------

inferTypes.if =
function(x, typeCollector = TypeCollector(), ...)
{
  # Infer type of each branch.
  # TODO: What if a branch contains multiple instructions?
  # We need (unit) tests to check we have the correct behavior.
  if_type = inferTypes(x[[3]], typeCollector, ...)
  
  else_type = 
    # if (<condition>) <body> else
    if (length(x) == 4)
      inferTypes(x[[4]], typeCollector, ...)
    else
      NullType()

  if (is(else_type, "ConditionalType")) {
    # Else branch is really an else if; merge into one ConditionalType object.
    pushCondition_q(else_type, x[[2]], if_type)

  } else if (identicalType(if_type, else_type)) {
    # Branches have the same type, so collapse to that type.
    value(if_type) = UnknownValue()
    if_type

  } else {
    # Branches have different types, so construct a ConditionalType object.
    pushCondition_q(ConditionalType(), x[[2]], if_type, else_type)
  }
}


inferTypes.for =
function(x, typeCollector = TypeCollector(), ...)
{
  # TODO: Handle variables that are composed from an iterator variable.

  atom = inferTypes(x[[3]], typeCollector, ...)
  atom = atomicType(atom)
  type = IteratorType(atom = atom)

  typeCollector$addType(as.character(x[[2]]), type)
    
  # Infer type of contents.
  inferTypes(x[[4]], typeCollector, ...)

  return(type)
}


`inferTypes.{` =
function(x, typeCollector = TypeCollector(), ...)
{
  types = lapply(x[-1], inferTypes, typeCollector, ...)
  if (length(types) == 0)
    NullType()
  else
    tail(types, 1)[[1]]
}


`inferTypes.(` =
function(x, typeCollector = TypeCollector(), ...)
{
  # Infer type of contents.
  inferTypes(x[[2]], typeCollector, ...)
}


# Atomic Types --------------------------------------------------

inferTypes.logical =
function(x, typeCollector = TypeCollector(), ...)
{
  type = makeVector(LogicalType(), length(x))
  value(type) = x

  return(type)
}


inferTypes.integer =
function(x, typeCollector = TypeCollector(), ...)
{
  type = makeVector(IntegerType(), length(x))
  value(type) = x

  return(type)
}


inferTypes.numeric =
function(x, typeCollector = TypeCollector(), ...)
{
  # TODO: Is it really a good idea to cast double to int?
  is_integer = all(x == floor(x))
  if (is_integer)
    return(inferTypes.integer(x))

  type = makeVector(NumericType(), length(x))
  value(type) = x

  return(type)
}


inferTypes.complex =
function(x, typeCollector = TypeCollector(), ...)
{
  type = makeVector(ComplexType(), length(x))
  value(type) = x

  return(type)
}


inferTypes.character =
function(x, typeCollector = TypeCollector(), ...)
{
  # TODO: Distinguish between characters and strings.
  type = makeVector(CharacterType(), length(x))
  value(type) = x

  return(type)
}


inferTypes.list =
function(x, typeCollector = TypeCollector(), ...)
{
  # FIXME:
  stop("Lists are not yet supported!")
}


inferTypes.NULL =
function(x, typeCollector = TypeCollector(), ...)
{
  NullType()
}
