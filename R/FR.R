################################################################################
##                        Friedman and Rafsky (1979)                          ##
##                                                                            ##
################################################################################
FR <- function(X1, X2, dist.fun = stats::dist, graph.fun = MST, 
               n.perm = 0, dist.args = NULL, graph.args = NULL, seed = NULL) {
  gTestWrapper(X1, X2, dist.fun, graph.fun, n.perm, dist.args, graph.args, 
               type = "original", seed = seed)
}

FR_cat <- function(X1, X2, dist.fun, agg.type, graph.type = "mstree", 
                   K = 1, n.perm = 0, seed = NULL) {
  gTestWrapperCat(X1, X2, dist.fun, agg.type, graph.type, K, n.perm, 
                  type = "original", seed = seed)
}