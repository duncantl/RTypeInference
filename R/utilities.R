# Description:
#   Utility functions.

.typeInfo =
function(...) 
  # No-op function for type annotations.
{}

evalTypeInfo =
function(annotation)
  # Evaluate the contents of a .typeInfo annotation.
{
  lapply(as.list(annotation)[-1], eval)
}

makeVector =
function(atom, dimension) 
  # Make a vector unless length is 1.
{
  length = prod(dimension)
  if(!is.na(length) && length == 1)
    atom
  else
    VectorType(atom, dimension)
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

  # FIXME: This breaks for things that make lists.

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
