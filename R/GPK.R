################################################################################
##              GENERALIZED PERMUTATION-BASED KERNEL TWO-SAMPLE TEST          ##
##                                                                            ##
################################################################################
findSigma <- function(X1, X2) {
  if(!requireNamespace("kerTests", quietly = TRUE)) {
    stop("Package \"kerTests\" required for using method findSigma().")
  }
  kerTests::med_sigma(X1, X2)
}

GPK <- function(X1, X2, n.perm = 0, fast = (n.perm == 0), M = FALSE, 
                sigma = findSigma(X1, X2), r1 = 1.2, r2 = 0.8, 
                seed = 42) {
  if(!requireNamespace("kerTests", quietly = TRUE)) {
    stop("Package \"kerTests\" required for using method GPK().")
  }
  set.seed(seed)
  if(!(inherits(X1, "matrix") | inherits(X1, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(!(inherits(X2, "matrix") | inherits(X2, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(ncol(X1) != ncol(X2)) {
    stop("All datasets must have the same number of variables.")
  }
  if(n.perm <= 0 && !fast) {
    warning("For n.perm == 0, only the fast test is available. Setting fast to TRUE.")
    fast <- TRUE
  }
  if(M && !fast) {
    warning("Only fast test available for MMD test. Setting fast to TRUE")
    fast <- TRUE
  }
  res <- kerTests::kertests(X1, X2, sigma, r1, r2, n.perm)
  if(n.perm > 0) {
    if(fast & !M) {
      stat <- unlist(res$teststat[c("ZW1", "ZW2", "ZD")])
      names(stat) <- c("ZW1", "ZW2", "ZD")
      pval <- res$pval$fGPK_perm
    } else if(fast & M) {
      stat <- unlist(res$teststat[c("ZW1", "ZW2")])
      names(stat) <- c("ZW1", "ZW2")
      pval <- res$pval$fGPKM_perm
    } else {
      stat <- res$teststat$GPK
      names(stat) <- "GPK"
      pval <- res$pval$GPK_perm
    }
  } else if(M){
    stat <- unlist(res$teststat[c("ZW1", "ZW2")])
    names(stat) <- c("ZW1", "ZW2")
    pval <- res$pval$fGPKM_appr
  } else {
    stat <- unlist(res$teststat[c("ZW1", "ZW2", "ZD")])
    names(stat) <- c("ZW1", "ZW2", "ZD")
    pval <- res$pval$fGPK_appr
  }
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  res <- list(statistic = stat, 
              p.value = pval, estimate = NULL,
              alternative = paste0("The distributions of ", 
                                   paste0(dname, collapse = " and "),
                                   " are unequal."), 
              method = paste0(if(fast) "Fast " else "", 
                              if(n.perm > 0) "" else "Approximative ", 
                              if(M) "MMD " else "",
                              "Generalized Permutation-Based Kernel Two-Sample Test"), 
              data.name = paste0(dname, collapse = " and "))
  class(res) <- "htest"
  return(res)
}
