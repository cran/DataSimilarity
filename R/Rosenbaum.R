################################################################################
##                             CROSS-MATCH TEST                               ##
##                                                                            ##
################################################################################
Rosenbaum <- function(X1, X2, exact = FALSE, dist.fun = stats::dist, 
                      dist.args = NULL, seed = NULL) {
  if(!requireNamespace("crossmatch", quietly = TRUE)) {
    stop("Package \"crossmatch\" required for using method Rosenbaum().")
  }
  if(!is.null(seed)) {
    set.seed(seed)
  }
  if(!(inherits(X1, "matrix") | inherits(X1, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(!(inherits(X2, "matrix") | inherits(X2, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(ncol(X1) != ncol(X2)) {
    stop("All datasets must have the same number of variables.")
  }
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  colnames(X1) <- colnames(X2) <- paste0("X", 1:ncol(X1))
  dists <- do.call(dist.fun, c(list(rbind(X1, X2)), dist.args))
  z <- c(rep(0, nrow(X1)), rep(1, nrow(X2)))
  res <- crossmatch::crossmatchtest(z, as.matrix(dists))
  stat <- res[["dev"]]
  names(stat) <- "z"
  est <- res$a1
  names(est) <- "edge.count"
  stderr <- sqrt(res$Va1)
  mu0 <- res$Ea1
  if(exact && length(z) >= 340) {
    warning("Number of samples is too high for exact test. Asymptotic test is performed instead.")
  }
  res <- list(statistic = stat, 
              p.value = ifelse(exact && length(z) < 340, res$pval, res$approxpval), 
              estimate = est,
              alternative = paste0("The distributions of ", 
                                   paste0(dname, collapse = " and "), 
                                   " are unequal."), 
              method = paste0(ifelse(exact && length(z) < 340, "Exact ", 
                                     "Approximative "), 
                              "cross-match test"),  
              data.name = paste0(dname, collapse = " and "), 
              stderr = stderr, mu0 = mu0)
  class(res) <- "htest"
  return(res)
}
