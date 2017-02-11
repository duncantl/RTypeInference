# Description:
# 
# Functions to generate type constraint systems for a control-flow graph.

#' @export
infer_types = function(cfg) {
  # Traverse the CFG with a preorder traversal of the dominator tree.
  # FIXME: Dominator tree should be cached.
  dom_t = dom_tree(cfg)
  set = ConstraintSet$new()

  dom_traverse(1L, cfg, dom_t, set)

  return (set)
}


dom_traverse = function(id, cfg, dom_t, set) {
  # Iterate over Phi nodes.
  lapply(cfg[[id]]$phi, detect_types, set)

  # Iterate over body, generating type constraints.
  lapply(cfg[[id]]$body, detect_types, set)

  # Descend to next blocks.
  children = setdiff(which(dom_t == id), id)
  lapply(children, dom_traverse, cfg, dom_t, set)

  return (set)
}
