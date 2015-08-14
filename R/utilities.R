# Description:
#   Utility functions.

evalTypeInfo =
function(annotation)
  # Evaluate the contents of a .typeInfo annotation.
{
  lapply(as.list(annotation)[-1], eval)
}

makeVector =
function(atom, length) 
  # Make a vector unless length is 1.
{
  if(!is.na(length) && length == 1)
    atom
  else
    VectorType(atom = atom, length = length)
}

any_is =
function(objects, class2)
  # Test inheritance for multiple objects.
{
  any(vapply(objects, is, TRUE, class2))
}

upcast =
function(types)
  # Upcast to the most general type.
{
  # character > complex > numeric > integer > logical
  # TODO:
  #   * Merge with math operator upcasting?
  #   * Test on semantic types

  types = lapply(types, atomicType)

  if (any_is(types, "CharacterType"))
    CharacterType()
  else if (any_is(types, "ComplexType"))
    ComplexType()
  else if (any_is(types, "NumericType"))
    NumericType()
  else if (any_is(types, "IntegerType"))
    IntegerType()
  else if (any_is(types, "LogicalType"))
    LogicalType()
  else
    # FIXME:
    stop("Upcast fell through!")
}

getVarName =
    # determine the variable being updated. x is the LHS of an assignment.
function(x)
{
  if(is.name(x))
     return(as.character(x))
  
  a = x[[2]]
  if(is.name(a))
      return(as.character(a))
  else if(is.call(a))
     return(getVarName(a))
  else {
      stop("not handled yet")
  }
}
