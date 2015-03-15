
infer_rhs = function(rhs) {
  
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
infer_assignment = function(x) {
  # check the right side first
  infer_rhs(x[[3]])
}

# walk the tree and make a table
inferType =
function(x, ...)
  UseMethod("inferType")

inferType.function =
function(x, ...) {    
  
#  f_list = as.list(x)
#  f_text = as.vector(f_list[[2]])

  b = body(x)
  if(class(b) != "{")
      b = substitute({ b }, list(b = b))
  
  foo = lapply(b[-1], inferType, ...)
browser()
  foo = foo[!sapply(foo, is.null)]
#  foo = unify(as.data.frame(t(sapply(foo, unlist))))
  foo =  matrix(unlist(foo), , 2, byrow = TRUE)
  foo = unify(as.data.frame(foo))  
  
  return(foo)

}


`inferType.<-` = `inferType.=` =
function(x, ...)
{
    # assignments are easy; add them to a type table
 data.frame(varname = as.character(x[[2]]),
            var_type = infer_assignment(x),
            stringsAsFactors = FALSE) 
}

inferType.call =
function(x, ...)
{
 
}

inferType.if =
function(x, ...)
{
  lapply(x[-(1:2)], inferType, ...)
  
}

`inferType.{` =
function(x, ...)
{
  lapply(x[-1], inferType, ...)
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

