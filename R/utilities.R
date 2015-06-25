# Description:
#   Utility functions.


any_is =
function(object, class2)
  # Test inheritance for multiple objects.
{
  any(vapply(object, is, TRUE, class2))
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
