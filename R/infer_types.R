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
#' @param scope Scope object
#' @param ... additional arguments
#' @export
infer_types = function(expr, scope = Scope(), ...) {

  type = .infer_types(expr, scope, ASTWalkerState(), ...)

  # TODO: is this the best way to handle scopes?
  list(type = type, scope = scope)
}


.infer_types =
function(expr, scope = Scope(), state, ...)
{
  UseMethod(".infer_types")
}


#' @export
.infer_types.function =
function(expr, scope = Scope(), state, ...)
{
  # FIXME: parent is not necessarily the enclosing scope
  func_scope = Scope()

  # Get type information from the function's default arguments.
  # FIXME:
  params = formals(expr)
  if (!is.null(params)) {
    type_list = lapply(params, .infer_types, func_scope, ...)
    func_scope$set_l(type_list, force = TRUE)
  }

  # Check for type annotations in the arguments.
  type_list = list(...)[[".typeInfo"]]
  if (!is.null(type_list))
    func_scope$set_l(type_list, force = TRUE)

  # Check for type annotations as an attribute.
  type_list = attr(expr, ".typeInfo")
  if (!is.null(type_list))
    func_scope$set_l(type_list, force = TRUE)

  # Run inference on the body of the function.
  body = body(expr)
  if (class(body) != "{")
    # TODO: Move to rewrite package?
    body = call("{", body)
  
  return_type = .infer_types(body, func_scope, state)
  state$return_flag = FALSE

  return (FunctionType(return_type, func_scope))
}


#' @export
`.infer_types.<-` =
function(expr, scope = Scope(), state, ...)
{
  # When we see an assignment, try to infer the type of the RHS and then add
  # the name on the LHS to the symbol table.
  # Arrays and function call assignments `foo(x)<-` are special cases.

  # Try to infer type of RHS. This is potentially recursive.
  rhs = expr[[3]]
  type = .infer_types(rhs, scope, state, ...)

  # Update the symbol table.
  lhs = expr[[2]]

  if (class(lhs) == "name") {
    name = as.character(lhs)
    # TODO: If the variable already has a type, see if it's compatible for
    # casting; throw an error on incompatible types.
    # FIXME:
    scope$set(name, type, force = TRUE)

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
  #    var_type = scope$getType(var_type)
  #} 

  #if(is.call(x[[2]])) {
  #    varname = getVarName(x[[2]])
  #    ty = UpdateType(var_type, varname)
  #    scope$addType(varname, ty)
  #} else {
  #    varname = as.character(x[[2]])[1] 
  #    scope$addType(varname, var_type)
  #}

  return (type)
}
#' @export
`.infer_types.=` = `.infer_types.<-`


#' @export
.infer_types.call =
function(expr, scope = Scope(), state, ...)
{
  call_name = as.character(expr[[1]])

  # TODO: 
  #   Ideally, all calls would be treated as known functions and handled that
  #   way. Are there any calls that need special treatment besides `return()`
  #   and `.typeInfo()`?
  # FIXME:
  #   Look up call handlers in an environment-like object instead of using a
  #   giant if-else switch.
  if(call_name == "return") {
    type = .infer_types(expr[[2]], scope, state, ...)
    #scope$set_return(type)
    # FIXME: This will depend on whether we're in a branch or not.
    state$return_flag = TRUE
    type

  } else if (call_name == ".typeInfo") {
    # Get types from annotation and add to collector.
    type_list = evalTypeInfo(expr)
    scope$set_l(type_list, force = TRUE)
    # TODO: Unclear what type we should return for this.
    NullType()

  } else if(call_name == "[")
    inferSubsetType(expr, scope, ...)
  else if(call_name == "[[")
    stop("[[ is not yet supported.")
  else if(call_name == "$")
    stop("$ is not yet supported.")
  else if(call_name %in% MATH_OPS)
    inferMathOpType(expr, scope, ...)
  else if(call_name %in% LOGIC_OPS)
    inferLogicOpType(expr, scope, ...)

  else if(call_name %in% names(knownFunctionTypes)) {
    type = knownFunctionTypes[[ call_name ]]

    if (is(type, "ConditionalType")) {
      # Infer argument types and pass to handler.
      arguments = pryr::standardise_call(expr)[-1]
      arg_types = lapply(arguments, .infer_types, scope, ...)
      type = infer(type, arg_types)
    }

    return(type)
  } else
    UnknownType()
}


#' @export
.infer_types.name =
function(x, scope = Scope(), state, ...)
{
  # TODO: Constants are already propagated, but we might want to set up a
  # reference for to the variable for non-constants.

  # Try to retrieve type.
  scope$get(x)
}


# Flow Control --------------------------------------------------

#' @export
.infer_types.if =
function(x, scope = Scope(), state, ...)
{
  # Infer type of each branch.
  # TODO: What if a branch contains multiple instructions?
  # We need (unit) tests to check we have the correct behavior.
  if_type = .infer_types(x[[3]], scope, state, ...)
  
  else_type = 
    # if (<condition>) <body> else
    if (length(x) == 4)
      .infer_types(x[[4]], scope, state, ...)
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
function(x, scope = Scope(), state, ...)
{
  # TODO: Handle variables that are composed from an iterator variable.

  type = .infer_types(x[[3]], scope, state, ...)
  type = add_context(element_type(type), "iterator")

  scope$set(x[[2]], type)
    
  # Infer type of contents.
  .infer_types(x[[4]], scope, state, ...)

  return(type)
}


#' @export
.infer_types.while =
function(x, scope = Scope(), state, ...)
{
  # TODO: Iterator detection for while loops.
  # FIXME: Why does inference on x + 1 with Unknown x return IntegerType?

  atom = .infer_types(x[[3]], scope, state, ...)
  atom = element_type(atom)

  return(atom)
}


#' @export
`.infer_types.{` =
function(expr, scope = Scope(), state, ...)
{
  # NOTE: This is not a scope marker! R's scopes are function boundaries.
  # However, sometimes {} may behave like a scope.

  #types = lapply(expr[-1], .infer_types, scope, ...)
  exprs = as.list(expr[-1])

  if (length(exprs) < 1)
    return(NullType())

  for (term in exprs) {
    type = .infer_types(term, scope, state, ...)

    if (state$return_flag)
      break
  }

  return(type)
}


#' @export
`.infer_types.(` =
function(x, scope = Scope(), state, ...)
{
  # Infer type of contents.
  .infer_types(x[[2]], scope, state, ...)
}


# Atomic Types --------------------------------------------------

#' @export
.infer_types.logical =
function(x, scope = Scope(), state, ...)
{
  type = makeVector(BooleanType(), length(x))
  value(type) = x

  return(type)
}


#' @export
.infer_types.integer =
function(x, scope = Scope(), state, ...)
{
  type = makeVector(IntegerType(), length(x))
  value(type) = x

  return(type)
}


#' @export
.infer_types.numeric =
function(x, scope = Scope(), state, ...)
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
function(x, scope = Scope(), state, ...)
{
  type = makeVector(ComplexType(), length(x))
  value(type) = x

  return(type)
}


#' @export
.infer_types.character =
function(x, scope = Scope(), state, ...)
{
  # TODO: Distinguish between characters and strings.
  type = makeVector(CharacterType(), length(x))
  value(type) = x

  return(type)
}


#' @export
.infer_types.list =
function(x, scope = Scope(), state, ...)
{
  # FIXME:
  stop("Lists are not yet supported!")
}


#' @export
.infer_types.NULL =
function(x, scope = Scope(), state, ...)
{
  NullType()
}
