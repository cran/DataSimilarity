################################################################################
##                         Chen, Chen and Su (2016)                           ##
##                                                                            ##
################################################################################
CCS <- function(X1, X2, dist.fun = stats::dist, graph.fun = MST, 
                n.perm = 0, dist.args = NULL, graph.args = NULL, seed = NULL) {
  gTestWrapper(X1, X2, dist.fun, graph.fun, n.perm, dist.args, graph.args, 
               type = "weighted", seed = seed)
}

CCS_cat <- function(X1, X2, dist.fun, agg.type, graph.type = "mstree",
                    K = 1, n.perm = 0, seed = NULL) {
  gTestWrapperCat(X1, X2, dist.fun, agg.type, graph.type, K, n.perm, 
                  type = "weighted", seed = seed)
}