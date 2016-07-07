# Description:
#   Core type inference functions.
#
# TODO:
#   * What about non-assignments? ie, the last line / return line.
#   * Resolve any variable references.
#   * Figure out the types of returns.
#   * Unify return type.
#   * Scoping?

#' Infer Types
#'
#' This function infers the types of an expression.
#'
#' @param expr unquoted R expression
#' @param tc TypeCollector object
#' @param ... additional arguments
#' @export
infer_types = function(expr, tc, ...) {
  if (missing(tc))
    tc = TypeCollector()

  .infer_types(expr, tc, ...)

  return(tc)
}


.infer_types =
function(x, tc = TypeCollector(), ...)
  # Walk the tree and make a symbol table.
{
  UseMethod(".infer_types")
}


#' @export
.infer_types.function =
function(x, tc = TypeCollector(), ...)
{    
  # TODO: Type annotations should be handled from a top-level function.

  # Check for type annotations in the arguments.
  type_list = list(...)[[".typeInfo"]]
  tc$mergeVariableTypeList(type_list, force = TRUE)

  # Check for type annotations as an attribute.
  type_list = attr(x, ".typeInfo")
  tc$mergeVariableTypeList(type_list, force = TRUE)

  # Get type information from the function's default arguments.
  type_list = lapply(formals(x), .infer_types, tc, ...)
  tc$mergeVariableTypeList(type_list)

  body = body(x)

  # Rewrite with { and delegate work to .infer_types.{
  # TODO: Move to rewrite package?
  if(class(body) != "{")
    body = substitute({body}, list(body = body))
  
  # Return last value. What we really need to do is work with the CFG, so exit
  # blocks will always have only one return type.
  return_type = .infer_types(body, tc)

  # If the last line wasn't a `return()`, add the return type.
  # TODO: This is a hack; we should find a more elegant solution.
  last_line = body[[length(body)]]
  if (!is.call(last_line) || as.character(last_line[[1]]) != "return")
    tc$addReturn(return_type)

  return(return_type)
}


#' @export
`.infer_types.<-` =
function(x, tc = TypeCollector(), ...)
{
  # When we see an assignment, try to infer the type of the RHS and then add
  # the name on the LHS to the symbol table.
  # Arrays and function call assignments `foo(x)<-` are special cases.

  # Try to infer type of RHS. This is potentially recursive.
  rhs = x[[3]]
  type = .infer_types(rhs, tc, ...)

  # Update the symbol table.
  lhs = x[[2]]

  if (class(lhs) == "name") {
    name = as.character(lhs)
    # TODO: If the variable already has a type, see if it's compatible for
    # casting; throw an error on incompatible types.
    tc$setVariableType(name, type, force = TRUE)

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
  #    var_type = tc$getType(var_type)
  #} 

  #if(is.call(x[[2]])) {
  #    varname = getVarName(x[[2]])
  #    ty = UpdateType(var_type, varname)
  #    tc$addType(varname, ty)
  #} else {
  #    varname = as.character(x[[2]])[1] 
  #    tc$addType(varname, var_type)
  #}

  return(type)
}
#' @export
`.infer_types.=` = `.infer_types.<-`


#' @export
.infer_types.call =
function(x, tc = TypeCollector(), ...)
{
  call_name = as.character(x[[1]])

  # TODO: 
  #   Ideally, all calls would be treated as known functions and handled that
  #   way. Are there any calls that need special treatment besides `return()`
  #   and `.typeInfo()`?
  if(call_name == "return")
    tc$addReturn(.infer_types(x[[2]], tc, ...))
  else if (call_name == ".typeInfo") {
    # Get types from annotation and add to collector.
    type_list = evalTypeInfo(x)
    tc$mergeVariableTypeList(type_list, force = TRUE)
    # TODO: Unclear what type we should return for this.
    NullType()

  } else if(call_name == "[")
    inferSubsetType(x, tc, ...)
  else if(call_name == "[[")
    stop("[[ is not yet supported.")
  else if(call_name == "$")
    stop("$ is not yet supported.")
  else if(call_name %in% MATH_OPS)
    inferMathOpType(x, tc, ...)
  else if(call_name %in% LOGIC_OPS)
    inferLogicOpType(x, tc, ...)

  else if(call_name %in% names(knownFunctionTypes)) {
    type = knownFunctionTypes[[ call_name ]]

    if (is(type, "ConditionalType")) {
      # Infer argument types and pass to handler.
      arguments = pryr::standardise_call(x)[-1]
      arg_types = lapply(arguments, .infer_types, tc, ...)
      type = infer(type, arg_types)
    }

    return(type)
  } else
    UnknownType()
}


#' @export
.infer_types.name =
function(x, tc = TypeCollector(), ...)
{
  # TODO: Constants are already propagated, but we might want to set up a
  # reference for to the variable for non-constants.

  # Try to retrieve type.
  tc$getVariableType(x)
}


# Flow Control --------------------------------------------------

#' @export
.infer_types.if =
function(x, tc = TypeCollector(), ...)
{
  # Infer type of each branch.
  # TODO: What if a branch contains multiple instructions?
  # We need (unit) tests to check we have the correct behavior.
  if_type = .infer_types(x[[3]], tc, ...)
  
  else_type = 
    # if (<condition>) <body> else
    if (length(x) == 4)
      .infer_types(x[[4]], tc, ...)
    else
      NullType()

  if (is(else_type, "ConditionalType")) {
    # Else branch is really an else if; merge into one ConditionalType object.
    pushCondition_q(else_type, x[[2]], if_type)

  } else if (same_type(if_type, else_type)) {
    # Branches have the same type, so collapse to that type.
    value(if_type) = UnknownValue()
    if_type

  } else {
    # Branches have different types, so construct a ConditionalType object.
    pushCondition_q(ConditionalType(), x[[2]], if_type, else_type)
  }
}


#' @export
.infer_types.for =
function(x, tc = TypeCollector(), ...)
{
  # TODO: Handle variables that are composed from an iterator variable.

  type = .infer_types(x[[3]], tc, ...)
  type = add_context(element_type(type), "iterator")

  tc$setVariableType(as.character(x[[2]]), type)
    
  # Infer type of contents.
  .infer_types(x[[4]], tc, ...)

  return(type)
}


#' @export
.infer_types.while =
function(x, tc = TypeCollector(), ...)
{
  # TODO: Iterator detection for while loops.
  # FIXME: Why does inference on x + 1 with Unknown x return IntegerType?

  atom = .infer_types(x[[3]], tc, ...)
  atom = element_type(atom)

  return(atom)
}


#' @export
`.infer_types.{` =
function(x, tc = TypeCollector(), ...)
{
  types = lapply(x[-1], .infer_types, tc, ...)
  if (length(types) == 0)
    NullType()
  else
    tail(types, 1)[[1]]
}


#' @export
`.infer_types.(` =
function(x, tc = TypeCollector(), ...)
{
  # Infer type of contents.
  .infer_types(x[[2]], tc, ...)
}


# Atomic Types --------------------------------------------------

#' @export
.infer_types.logical =
function(x, tc = TypeCollector(), ...)
{
  type = makeVector(BooleanType(), length(x))
  value(type) = x

  return(type)
}


#' @export
.infer_types.integer =
function(x, tc = TypeCollector(), ...)
{
  type = makeVector(IntegerType(), length(x))
  value(type) = x

  return(type)
}


#' @export
.infer_types.numeric =
function(x, tc = TypeCollector(), ...)
{
  # TODO: Is it really a good idea to cast double to int?
  is_integer = all(x == floor(x))
  if (is_integer)
    return(.infer_types.integer(x))

  type = makeVector(RealType(), length(x))
  value(type) = x

  return(type)
}


#' @export
.infer_types.complex =
function(x, tc = TypeCollector(), ...)
{
  type = makeVector(ComplexType(), length(x))
  value(type) = x

  return(type)
}


#' @export
.infer_types.character =
function(x, tc = TypeCollector(), ...)
{
  # TODO: Distinguish between characters and strings.
  type = makeVector(CharacterType(), length(x))
  value(type) = x

  return(type)
}


#' @export
.infer_types.list =
function(x, tc = TypeCollector(), ...)
{
  # FIXME:
  stop("Lists are not yet supported!")
}


#' @export
.infer_types.NULL =
function(x, tc = TypeCollector(), ...)
{
  NullType()
}
