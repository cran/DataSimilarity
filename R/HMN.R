################################################################################
##                            Hediger et al. (2020)                           ##
##                                                                            ##
################################################################################
HMN <- function(X1, X2, n.perm = 0, statistic = "PerClassOOB", normal.approx = FALSE, 
                seed = 42, ...) {
  if(!requireNamespace("hypoRF", quietly = TRUE)) {
    stop("Package \"hypoRF\" required for using method HMN().")
  }
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  if(!(inherits(X1, "matrix") | inherits(X1, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(!(inherits(X2, "matrix") | inherits(X2, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(ncol(X1) != ncol(X2)) {
    stop("All datasets must have the same number of variables.")
  }
  X1 <- as.data.frame(X1)
  X2 <- as.data.frame(X2)
  colnames(X1) <- colnames(X2) <- paste0("X", 1:ncol(X1))
  res <- hypoRF::hypoRF(X1, X2, K = max(n.perm, 1), statistic = statistic, 
                        normalapprox = normal.approx, seed = seed, alpha = 0.05, 
                        ...)
  
  stat <- res$obs
  if(statistic == "PerClassOOB" & n.perm <= 0) {
    param <- NULL
    names(stat) <- "z"
  } else if(statistic == "OverallOOB" & n.perm <= 0) {
    param <- c(round(nrow(X1) * 0.5) + round(nrow(X2) * 0.5), 0.5)
    names(param) <- c("size", "prob")
    names(stat) <- "x"
  } else if(statistic == "PerClassOOB") {
    param <- NULL
    names(stat) <- "p0.hat + p1.hat"
  } else {
    param <- NULL
    names(stat) <- "p.hat"
  }
  res <- list(statistic = stat, parameter = param, 
              p.value = res$pvalue, 
              estimate = NULL, 
              alternative = paste0("The distributions of ", paste0(dname, collapse = " and "),
                                   " are unequal."),
              method = paste0(ifelse(n.perm > 0, "Permutation ", "Approximative "),
                              statistic, " random forest based two-sample test", 
                              if(normal.approx & n.perm > 0) 
                                " using normal approximation of permutation distribution"), 
              data.name = paste0(dname, collapse = " and "), 
              val = res$val, varest = res$varest, 
              importance.ranking = res$importance_ranking, 
              importance.distribution = res$importancedistribution,
              cut.off = res$cutoff)
  class(res) <- "htest"
  return(res)
}
