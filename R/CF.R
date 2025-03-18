################################################################################
##                         Chen and Friedman (2016)                           ##
##                                                                            ##
################################################################################
CF <- function(X1, X2, dist.fun = stats::dist, graph.fun = MST, 
               n.perm = 0, dist.args = NULL, graph.args = NULL, seed = 42) {
  gTestWrapper(X1, X2, dist.fun, graph.fun, n.perm, dist.args, graph.args, 
               type = "generalized", seed = seed)
}

CF_cat <- function(X1, X2, dist.fun, agg.type, graph.type = "mstree",
                   K = 1, n.perm = 0, seed = 42) {
  gTestWrapperCat(X1, X2, dist.fun, agg.type, graph.type, K, n.perm, 
                  type = "generalized", seed = seed)
}