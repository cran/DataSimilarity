################################################################################
##              GENERALIZED PERMUTATION-BASED KERNEL TWO-SAMPLE TEST          ##
##                                                                            ##
################################################################################
kerTests <- function(X1, X2, n.perm = 0, sigma = findSigma(X1, X2), 
                     r1 = 1.2, r2 = 0.8, seed = 42) {
  if(!requireNamespace("kerTests", quietly = TRUE)) {
    stop("Package \"kerTests\" required for using method kerTests().")
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
  res <- kerTests::kertests(X1, X2, sigma, r1, r2, n.perm)
  stat <- unlist(res$teststat)
  pval <- unlist(res$pval[grepl(ifelse(n.perm > 0, "perm", "appr"), names(res$pval))])
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  res <- list(statistic = stat, 
              p.value = pval, estimate = NULL,
              alternative = paste0("The distributions of ", paste0(dname, collapse = " and "),
                                   " are unequal."), 
              method = "Generalized Permutation-Based Kernel Two-Sample Test", 
              data.name = paste0(dname, collapse = " and "))
  # class(res) <- "htest"
  return(res)
}
