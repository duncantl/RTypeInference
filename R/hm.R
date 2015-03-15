infer_rhs = function(rhs, typeCollector, ...) {
  
  if(length(rhs) > 1) {
    
    # pick apart the rhs expression
    rhs_types = sapply(rhs, infer_rhs)
    
    # we just don't know which yet
    return(paste(rhs_types, sep = "", collapse = ","))
    
  }
  else {
    num_type = suppressWarnings(as.numeric(as.character(rhs)))
    
    # don't know yet
    if(is.na(num_type)) {
      # FIXME
      if(exists(as.character(rhs))) { # ?? - don't get the function. We know this is a call. FIX
        if(class(get(as.character(rhs))) == "function") {
          
          # do we know about this function?
          var_type = subset(known_table, varname == as.character(rhs))
          if(nrow(var_type) > 0)
             return(as.character(var_type$type)) 
          
          return(rhs)
        }
      }
      else 
         return(as.character(rhs)) 
    }
    
    # int? or double?
    if(as.integer(as.character(rhs)) == as.numeric(as.character(rhs)))
       return("int")
    else
       return("double")
    
  }
}

# given an assignment line, tries to figure out
# what the type on the RHS is.
infer_assignment = function(x, typeCollector = typeInferenceCollector(), ...) {
  # check the right side first
#  infer_rhs(x[[3]])
   inferType(x[[3]], typeCollector, ...)
}

# walk the tree and make a table
inferType =
function(x, typeCollector = typeInferenceCollector(), ...)
  UseMethod("inferType")


inferType.numeric =
function(x, typeCollector = typeInferenceCollector(), ...) {
   isInt = all(x == floor(x))
  if(length(x) == 1) {
      if(isInt)
         "int"
      else
         "double"
  } else
      if(isInt)
          "integer"
      else
          "numeric"
}

inferType.logical =
function(x, typeCollector = typeInferenceCollector(), ...)
{
   if(length(x) == 1) "boolean" else "logical"
}

inferType.character =
function(x, typeCollector = typeInferenceCollector(), ...)
{
   if(length(x) == 1) "string" else "character"
}


`inferType.(` =
function(x, typeCollector = typeInferenceCollector(), ...)
{
    inferType(x[[2]], typeCollector, ...)
}

inferType.function =
function(x, typeCollector = typeInferenceCollector(), ...)
{    
  b = body(x)
  if(class(b) != "{")
      b = substitute({ b }, list(b = b))
  
  foo = lapply(b[-1], inferType, typeCollector, ...)

#  foo = foo[!sapply(foo, is.null)]
#  foo =  matrix(unlist(foo), , 2, byrow = TRUE)
#  foo = unify(as.data.frame(foo))  
  
  return(typeCollector)
}


`inferType.<-` = `inferType.=` =
function(x, typeCollector, ...)
{
    # assignments are easy; add them to a type table
 varname = as.character(x[[2]])
# var_type = infer_assignment(x, typeCollector, ...)
 var_type = inferType(x[[3]], typeCollector, ...)  
 typeCollector$addType(varname, var_type)
}

inferType.call =
function(x, typeCollector, ...)
{
        # Math and logical operators
        # This is quite similar to what we are doing in the RLLVMCompile so we should consolidate the code.
   fnName = as.character(x[[1]])

   if(fnName == "return")
      return(typeCollector$addReturn(inferType(x[[2]], typeCollector, ...)))
   
   if(fnName %in% names(knownFunctionTypes))
      return(knownFunctionTypes[[ fnName ]])

   if(fnName %in% c("+", "-", "*", "/")) {
      return(inferMathOpType(x, typeCollector, ...))
   }

   if(fnName %in% c("<", ">", "<=", ">=", "==", "!=")) {
      return(inferLogicOpType(x, typeCollector, ...))
   }

   return(NA)
}

inferType.name =
function(x, typeCollector, ...)
{
  typeCollector$getType(x)
}

inferType.if =
function(x, typeCollector, ...)
{
  types = lapply(x[-(1:2)], inferType, typeCollector, ...)

    # if all branches have the same type, then collapse this down.
  tmp = unique(unlist(types))
  if(length(tmp) == 1)
      types[[1]]
  else
      types
}

inferType.for =
function(x, typeCollector, ...)
{
  inferType(x[[4]], typeCollector, ...)
}


`inferType.{` =
function(x, typeCollector, ...)
{
  lapply(x[-1], inferType, typeCollector, ...)
}

    # TODO, what about non-assignments?
    # ie, the last line / return line
    



# collapse this table down into best guesses for each variable
unify = function(tb) {
  
  guesses = lapply(1:nrow(tb), function(x) {
    r = tb[x,]
    t(sapply(unlist(strsplit(as.character(r[2][[1]]), ",")), function(g) {
      cbind(as.character(r[1][[1] ]), g)
    }))
    
  })
  
  # for each of these guesses, reduce to unique
  unique_guess = matrix(unlist(lapply(guesses, function(g) {
    # for each unique type
    lapply(unique(g[,2]), function(x) {
      c(unique(g[,1]), x)
    })
  })), ncol = 2, byrow = T)
  
  colnames(unique_guess) = c("name", "type")
  
  # reduce types
  #sapply(unique(unique_guess$name), function(x) {
  #  s = subset(unique_guess, name == x)
  #})
  
  return(unique_guess)
  
}

# resolve any variable references

# figure out the types of inputs


# figure out the types of returns

