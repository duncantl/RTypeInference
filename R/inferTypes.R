# Description:
#   Core type inference functions.
#
# TODO:
#   * What about non-assignments? ie, the last line / return line.
#   * Resolve any variable references.
#   * Figure out the types of inputs.
#   * Figure out the types of returns.
#   * Should typeCollector always have a default?
#   * Unify return type.
#   * Scoping?
#   * Modify logic operator handler to evaluate simple conditions.

#infer_rhs = function(rhs, typeCollector, ...) {
#  
#  if(length(rhs) > 1) {
#    
#    # pick apart the rhs expression
#    rhs_types = sapply(rhs, infer_rhs)
#    
#    # we just don't know which yet
#    return(paste(rhs_types, sep = "", collapse = ","))
#    
#  }
#  else {
#    num_type = suppressWarnings(as.numeric(as.character(rhs)))
#    
#    # don't know yet
#    if(is.na(num_type)) {
#      # FIXME
#      if(exists(as.character(rhs))) { # ?? - don't get the function. We know this is a call. FIX
#        if(class(get(as.character(rhs))) == "function") {
#          
#          # do we know about this function?
#          var_type = subset(known_table, varname == as.character(rhs))
#          if(nrow(var_type) > 0)
#             return(as.character(var_type$type)) 
#          
#          return(rhs)
#        }
#      }
#      else 
#         return(as.character(rhs)) 
#    }
#    
#    # int? or double?
#    if(as.integer(as.character(rhs)) == as.numeric(as.character(rhs)))
#       return("int")
#    else
#       return("double")
#    
#  }
#}
#
## given an assignment line, tries to figure out
## what the type on the RHS is.
#infer_assignment = function(x, typeCollector = TypeCollector(), ...) {
#  # check the right side first
#  #  infer_rhs(x[[3]])
#   inferTypes(x[[3]], typeCollector, ...)
#}
#
## collapse this table down into best guesses for each variable
#unify = function(tb) {
#  
#  guesses = lapply(1:nrow(tb), function(x) {
#    r = tb[x,]
#    t(sapply(unlist(strsplit(as.character(r[2][[1]]), ",")), function(g) {
#      cbind(as.character(r[1][[1] ]), g)
#    }))
#    
#  })
#  
#  # for each of these guesses, reduce to unique
#  unique_guess = matrix(unlist(lapply(guesses, function(g) {
#    # for each unique type
#    lapply(unique(g[,2]), function(x) {
#      c(unique(g[,1]), x)
#    })
#  })), ncol = 2, byrow = T)
#  
#  colnames(unique_guess) = c("name", "type")
#  
#  # reduce types
#  #sapply(unique(unique_guess$name), function(x) {
#  #  s = subset(unique_guess, name == x)
#  #})
#  
#  return(unique_guess)
#  
#}

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
    # TODO: Need better handling for primitives.
    call = as.list(standardise_call(x))
    type = knownFunctionTypes[[ call_name ]]

    is_literal = !sapply(call[-1, drop = FALSE], is.language)

    if (is(type, "ConditionalType") && all(is_literal))
      infer(type, call[-1, drop = FALSE])
    else
      type

  } else
    NA
}


inferTypes.name =
function(x, typeCollector = TypeCollector(), ...)
{
  # Try to retrieve type.
  typeCollector$getType(x)

  # TODO: Might want to set up a reference to this name.
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

  } else if (identical(if_type, else_type)) {
    # Branches have the same type, so collapse to that type.
    # TODO: Is using identical() robust enough?
    if_type

  } else {
    # Branches have different types, so construct a ConditionalType object.
    pushCondition_q(ConditionalType(), x[[2]], if_type, else_type)
  }
}


inferTypes.for =
function(x, typeCollector = TypeCollector(), ...)
{
  # TODO: Handle loop variables that get assigned iterator.

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
  makeVector(LogicalType(), length(x))
}


inferTypes.integer =
function(x, typeCollector = TypeCollector(), ...)
{
  makeVector(IntegerType(), length(x))
}


inferTypes.numeric =
function(x, typeCollector = TypeCollector(), ...)
{
  # TODO: Is it really a good idea to cast double to int?
  is_integer = all(x == floor(x))
  if (is_integer)
    return(inferTypes.integer(x))

  makeVector(NumericType(), length(x))
}


inferTypes.complex =
function(x, typeCollector = TypeCollector(), ...)
{
  makeVector(ComplexType(), length(x))
}


inferTypes.character =
function(x, typeCollector = TypeCollector(), ...)
{
  # TODO: Distinguish between characters and strings.
  makeVector(CharacterType(), length(x))
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
