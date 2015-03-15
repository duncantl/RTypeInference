
library(RCIndex)

f = sprintf("%s/src/main/names.c", R.home())
if(!file.exists(f))
  stop("cannot find names.c")

tu = createTU(f, args = "-DHAVE_CONFIG_H", includes = c(sprintf("%s/src/include", R.home()), sprintf("%s/include", R.home()), sprintf("%s/src/include", R.home())))

gvars = getGlobalVariables(tu)

gvars$R_FunTab
gvars$R_FunTab@def$kind # VarDecl

gvars$R_FunTab@def[[2]]$kind  # InitListExpr

length(gvars$R_FunTab@def[[2]]) # 682; 688 now

init = gvars$R_FunTab@def[[2]]
k = sapply(children(init), function(x) x$kind)
table(k) # all of type 119 - InitListExpr

primitiveNames = structure(sapply(children(init), function(x) getCursorTokens(x[[1]])[1]), names = NULL)

primitiveNames = primitiveNames[!is.na(primitiveNames)]
primitiveNames = gsub('"', '', primitiveNames)


con = file("../../R/primitiveNames.R", "w")
cat("PrimitiveNames = ", file = con)
dput(primitiveNames, con)
close(con)

