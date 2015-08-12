# Description:
#   Utility functions.

to_type_list =
function(annotation)
  # Convert an annotation expression to a list of types.
{
  annotation = as.list(annotation)[-1]
  lapply(annotation,
    function(text) {
      type = as.list(text)
      type[[1]] = as.character(type[[1]])
      do.call(new, type)
    })
}

any_is =
function(object, class2)
  # Test inheritance for multiple objects.
{
  any(vapply(object, is, TRUE, class2))
}

upcast =
function(types)
  # Upcast to the most general type.
{
  # character > complex > numeric > integer > logical
  # TODO:
  #   * Merge with math operator upcasting?
  #   * Semantic types
  #   * Vector types
  is_character = sapply(types, is, "CharacterType")
  if (any_is(types, "CharacterType"))
    new("CharacterType")
  else if (any_is(types, "ComplexType"))
    new("ComplexType")
  else if (any_is(types, "NumericType"))
    new("NumericType")
  else if (any_is(types, "IntegerType"))
    new("IntegerType")
  else if (any_is(types, "LogicalType"))
    new("LogicalType")
  else
    NA
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
