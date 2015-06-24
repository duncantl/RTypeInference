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
