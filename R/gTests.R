################################################################################
##                      Wrapper for all gTests methods                        ##
##                                                                            ##
################################################################################
gTests <- function(X1, X2, dist.fun = stats::dist, graph.fun = MST, 
                   n.perm = 0, dist.args = NULL, graph.args = NULL,
                   maxtype.kappa = 1.14,  seed = 42) {
  gTestWrapper(X1, X2, dist.fun, graph.fun, n.perm, dist.args, graph.args, 
               type = "all", seed = seed, maxtype.kappa = maxtype.kappa)
}

gTests_cat <- function(X1, X2, dist.fun = function(x, y) sum(x != y), graph.type = "mstree", 
                       K = 1, n.perm = 0, maxtype.kappa = 1.14, seed = 42) {
  gTestWrapperCat(X1, X2, dist.fun, agg.type = "a", graph.type, K, n.perm, 
                  type = "all", seed = seed, maxtype.kappa = maxtype.kappa)
}
