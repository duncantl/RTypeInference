#' Generate Type Constraint Set from Expression
#'
#' This function generates a set of type constraints from the given expression,
#' which can then be solved to infer types for the expression.
#' 
#' @param node (ASTNode) An expression from which to generate type constraints.
#' @param map (SymbolMap) A mapping between program variables and type
#' variables.
#'
#' @return An S4 `Result` object, with slots `contraints` and `map`. The former
#' is the list of constraints, and the latter is the mapping between program
#' variables and type variables.
#'
#' @export
constrain =
function(node, map = SymbolMap()) 
{
  .constrain(node, list(), map)
}


.constrain =
function(node, constraints, map)
{
  UseMethod(".constrain")
}


.constrain.Parenthesis =
function(node, constraints, map)
{
  .constrain(node$args$contents[[1L]], constraints, map)
}


.constrain.BlockList =
function(node, constraints, map)
{
  # Let's try the naive thing and just call constrain on each block.

  # Process everything except the exit block.
  # TODO: Find a more elegant way to split off the exit block.
  blocks = node$contents[-node$exit_index]
  for (block in blocks) {
    result = .constrain(block, constraints, map)
    constraints = result@constraints
    map = result@map
  }

  # Process the exit block.
  exit = rstatic::exit_block(node)
  result = .constrain(exit, constraints, map)

  result
}


.constrain.Block =
function(node, constraints, map)
{
  for (phi in node$phi) {
    result = .constrain(phi, constraints, map)
    constraints = result@constraints
    map = result@map
  }


  for(exp in node$contents) {
    result = .constrain(exp, constraints, map)
    constraints = result@constraints
    map = result@map
  }

  result
}


.constrain.If =
function(node, constraints, map)
{
  result = .constrain(node$condition, constraints, map)

  # Constrain condition to be a logical value.
  con = typesys::Equivalence(result@type, typesys::RLogical)
  result@constraints = append(result@constraints, con)

  # TODO: An if-expression returns the union of the types at the end of each
  # branch. An if-statement returns no type (instead the types are captured in
  # phi-expressions).
  result@type = typesys::RNull

  result
}


.constrain.For =
function(node, constraints, map)
{
  # Generate constraints for the iterator.
  result = .constrain(node$iterator, constraints, map)

  # Record generic type for the iteration variable.
  #
  # FIXME: The type should be an element of tdef, not tdef itself!
  result@map = set_defined_as(result@map, node$variable$ssa_name, result@type)

  result
}


.constrain.Phi =
function(node, constraints, map)
{
  # Record use for each of the inputs.
  n = length(node$contents)
  types = vector("list", n)
  for (i in seq(n)) {
    elt = node$contents[[i]]

    result = .constrain(elt, constraints, map)
    types[[i]] = result@type
    constraints = result@constraints
    map = result@map
  }

  # Create a union from the input types.
  result@type = typesys::OneOf(types)

  # Now proceed like this is an Assign node.
  result@map = set_defined_as(result@map, node$write$ssa_name, result@type)

  result
}


.constrain.Branch =
function(node, constraints, map)
{
  # Do nothing.
  ConstrainResult(typesys::RNull, constraints, map)
}


.constrain.Brace =
function(node, constraints, map)
{
  # Call .constrain on each element.
  for (exp in node$contents) {
    result = .constrain(exp, constraints, map)
    constraints = result@constraints
    map = result@map
  }

  result
}


.constrain.Call = function(node, constraints, map) {
  # Infer type for called function.
  # FIXME: What if called function is anonymous?
  result = .constrain(node$fn, constraints, map)
  t1 = result@type

  # Infer types for arguments.
  n = length(node$args$contents)
  targs = vector("list", n)

  for (i in seq_len(n)) {
    exp = node$args$contents[[i]]

    result = .constrain(exp, result)
    targs[[i]] = result@type
  }

  # Create new type variable for return.
  tvar = new_variable(result@map)
  result@type = tvar

  # Add constraint t1 ~ (targs -> tvar)
  tfun = typesys::RFunction(targs, tvar)
  con = typesys::Equivalence(t1, tfun)
  result@constraints = append(result@constraints, con)

  result
}


.constrain.Symbol =
function(node, constraints, map)
{
  # Create new type variable.
  tvar = new_variable(map)

  # Add to uses set.
  map = add_use(map, node$ssa_name, tvar)

  is_parameter = get_is_parameter(map, node$ssa_name)
  if (is.na(is_parameter)) {
    # Symbol does not correspond to a definition.
    # Do nothing.

  } else {
    tdef = get_defined_as(map, node$ssa_name)
    if (is_parameter) {
      # Symbol corresponds to a parameter, so add equivalence constraint.
      con = typesys::Equivalence(tvar, tdef)

    } else {
      # Symbol corresponds to a variable, so add instance constraint.
      if (length(typesys::vars(tdef)) == 0L) {
        # No variables in the RHS so just make an equality constraint.
        # We could check this case in the solver instead, but it's currently not
        # clear what the benefit would be.
        con = typesys::Equivalence(tvar, tdef)

      } else {
        # FIXME: Consider scopes when determining active parameters.

        # Symbols with no known definition don't count as parameters (so
        # globals can be polymorphic).
        params = get_parameters(map, include_na = FALSE)
        m = lapply(params, `[[`, "defined_as")

        con = typesys::ImplicitInstance(tvar, tdef, m)
      }
    }

    constraints = append(constraints, con)
  }

  ConstrainResult(type, constraints, map)
}


.constrain.Assign =
function(node, constraints, map)
{
  # Generate constraints for the definition.
  result = .constrain(node$read, constraints, map)

  # Record generic type for variable.
  result@map = set_defined_as(result@map, node$write$ssa_name, result@type)

  result
}


.constrain.Return = .constrain.Assign


.constrain.Function =
function(node, constraints, map)
{
  # Create a type variable for each parameter.
  tparams = lapply(node$params$contents, function(param) {
    tvar = new_variable(map)

    # FIXME: Helper might be tainted with outer scope variables. Need to keep
    # track of scopes somehow.
    map <<- set_defined_as(map, param$ssa_name, tvar, is_parameter = TRUE)

    tvar
  })

  # Infer types for the body.
  result = .constrain(node$body, constraints, map)

  # TODO: Record parameter names.
  result@type = typesys::RFunction(tparams, result@type)

  # FIXME: Remove parameters from the active set.
  for (p in node$params$contents) {
    result@map = remove_entry(result@map, p$ssa_name)
  }

  result
}


# Literals ----------------------------------------

.constrain.Character =
function(node, constraints, map)
{
  ConstrainResult(typesys::RString, constraints, map)
}


.constrain.Numeric =
function(node, constraints, map)
{
  ConstrainResult(typesys::RNumeric, constraints, map)
}


.constrain.Integer =
function(node, constraints, map)
{
  ConstrainResult(typesys::RInteger, constraints, map)
}


.constrain.Logical =
function(node, constraints, map)
{
  ConstrainResult(typesys::RLogical, constraints, map)
}
