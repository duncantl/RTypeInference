# Description:
#   R6 class

#' AST Walker State
#' 
#' This R6 class stores the state of the AST walker, in order to signal to the
#' walker when and how far to ascend from an AST node.
ASTWalkerState = function() {
  .ASTWalkerState$new()
}

.ASTWalkerState = R6::R6Class("ASTWalkerState",
  # public ----------
  public = list(
    return_flag = FALSE
  )
)
