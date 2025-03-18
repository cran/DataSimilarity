################################################################################
##                                CRAMER TEST                                 ##
##                                                                            ##
################################################################################
Cramer <- function(X1, X2, n.perm = 0, just.statistic = (n.perm <= 0), sim = "ordinary", 
                   maxM = 2^14, K = 160, seed = 42) {
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  
  cramerWrapper(X1, X2, dname = dname, n.perm = n.perm, just.statistic = just.statistic, 
                kernel = "phiCramer", sim = sim, maxM = maxM, K = K, 
                seed = seed)
}

################################################################################
##                                  BAHR TEST                                 ##
##                                                                            ##
################################################################################
Bahr <- function(X1, X2, n.perm = 0, just.statistic = n.perm <= 0, sim = "ordinary", 
                 maxM = 2^14, K = 160, seed = 42) {
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  cramerWrapper(X1, X2, dname = dname, n.perm = n.perm, just.statistic = just.statistic, 
                kernel = "phiBahr", sim = sim, maxM = maxM, K = K, 
                seed = seed)
}

################################################################################
##                        Baringhaus & Franz (2010)                           ##
##                                                                            ##
################################################################################
BF <- function(X1, X2, n.perm = 0, just.statistic = n.perm <= 0, 
               kernel = "phiLog", sim = "ordinary", maxM = 2^14, K = 160, 
               seed = 42) {
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  cramerWrapper(X1, X2, dname = dname, n.perm = n.perm, just.statistic = just.statistic, 
                kernel = kernel, sim = sim, maxM = maxM, K = K, 
                seed = seed)
}