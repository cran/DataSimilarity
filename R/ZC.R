################################################################################
##                           Zhang and Chen (2022)                            ##
##                                                                            ##
################################################################################
ZC <- function(X1, X2, dist.fun = stats::dist, graph.fun = MST, n.perm = 0,
               dist.args = NULL, graph.args = NULL, maxtype.kappa = 1.14, 
               seed = 42) {
  gTestWrapper(X1, X2, dist.fun, graph.fun, n.perm, dist.args, graph.args, 
               type = "maxtype", maxtype.kappa = maxtype.kappa, seed = seed)
}


ZC_cat <- function(X1, X2, dist.fun, agg.type, graph.type = "mstree", K = 1, 
                   n.perm = 0, maxtype.kappa = 1.14, seed = 42) {
  gTestWrapperCat(X1, X2, dist.fun, agg.type, graph.type, K, n.perm, 
                  type = "maxtype", maxtype.kappa = maxtype.kappa, seed = seed)
}