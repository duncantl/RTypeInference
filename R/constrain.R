# Implementation of Heeren et al. variant of Damas-Milner.
#
# Uses typesys 0.3 and latest version of rstatic.


#' Generate Type Constraint Set from Expression
#'
#' This function generates a set of type constraints from the given expression,
#' which can then be solved to infer types for the expression.
#' 
#' @param node ASTNode. Expression from which to generate type constraints.
#' @param helper InferHelper. Helper object for constraint generation.
#' @param counter Counter. Counter for uniquely naming type variables.
#'
#' @return A list with two elements. The first is the list of constraints, and
#' the second is the final state of the helper object.
#'
#' @export
constrain =
function(node
  , helper = InferHelper(counter)
  , counter = rstatic::Counter$new()
  ) 
{
  c(type, constraints, helper) := .constrain(node, list(), helper)

  # TODO: Return uses list instead of entire helper.
  list(constraints = constraints, helper = helper)
}

.constrain = function(node, constraints, helper) UseMethod(".constrain")

.constrain.Brace = function(node, constraints, helper) {
  # Call .constrain on each element.
  for (exp in node$contents)
    c(type, constraints, helper) := .constrain(exp, constraints, helper)

  list(type = type, constraints = constraints, helper = helper)
}

.constrain.Call = function(node, constraints, helper) {
  # Infer type for called function.
  # FIXME: What if called function is anonymous?
  c(t1, constraints, helper) := .constrain(node$fn, constraints, helper)

  # Infer types for arguments.
  targs = lapply(node$args$contents, function(exp) {
    c(targ, constraints, helper) := .constrain(exp, constraints, helper)
    constraints <<- constraints
    helper <<- helper
    targ
  })

  # Create new type variable for return.
  tvar = typesys::Variable(new_name(helper))

  # Add constraint t1 ~ (targs -> tvar)
  tfun = typesys::RFunction(targs, tvar)
  con = typesys::Equivalence(t1, tfun)
  constraints = append(constraints, con)

  list(type = tvar, constraints = constraints, helper = helper)
}

.constrain.Symbol = function(node, constraints, helper) {
  # Create new type variable.
  tvar = typesys::Variable(new_name(helper))

  # Add to uses set.
  helper = add_use(helper, node$ssa_name, tvar)

  # Constrain tvar to an implicit instance if there is a definition type.
  # TODO: Constrain tvar to equality if there is a parameter definition type.
  tdef = get_def(helper, node$ssa_name)
  if (!is.null(tdef)) {
    if (length(typesys::vars(tdef)) == 0L) {
      # No variables in the RHS so just make an equality constraint.
      # We could check this case in the solver instead, but it's currently not
      # clear what the benefit would be.
      con = typesys::Equivalence(tvar, tdef)

    } else {
      # TODO: Get m, the set of active parameters.
      con = typesys::ImplicitInstance(tvar, tdef, list())
    }

    constraints = append(constraints, con)
  }

  list(type = tvar, constraints = constraints, helper = helper)
}


.constrain.Assign = function(node, constraints, helper) {
  # Generate constraints for the definition.
  c(tdef, constraints, helper) := .constrain(node$read, constraints, helper)

  # Record generic type for variable.
  helper = add_def(helper, node$write$ssa_name, tdef)

  list(type = tdef, constraints = constraints, helper = helper)
}

.constrain.Function = function(node, constraints, helper) {
  # Create a type variable for each parameter.
  
  # Record these type variables as *parameter* definitions.

  # Constrain uses 
}

# Literals ----------------------------------------

.constrain.Character = function(node, constraints, helper) {
  list(type = typesys::RString, constraints = constraints, helper = helper)
}

.constrain.Numeric = function(node, constraints, helper) {
  list(type = typesys::RNumeric, constraints = constraints, helper = helper)
}

.constrain.Integer = function(node, constraints, helper) {
  list(type = typesys::RInteger, constraints = constraints, helper = helper)
}

.constrain.Logical = function(node, constraints, helper) {
  list(type = typesys::RLogical, constraints = constraints, helper = helper)
}
