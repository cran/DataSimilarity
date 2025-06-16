################################################################################
##                          WASSERSTEIN DISTANCE                              ##
##                                                                            ##
################################################################################
Wasserstein <- function(X1, X2, n.perm = 0, fast = (nrow(X1) + nrow(X2)) > 1000, 
                        S = max(1000, (nrow(X1) + nrow(X2)) / 2), seed = NULL, ...) {
  if(!requireNamespace("Ecume", quietly = TRUE)) {
    stop("Package \"Ecume\" required for using method Wasserstein().")
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
  res <- Ecume::wasserstein_permut(as.matrix(X1), as.matrix(X2), 
                                   iterations = n.perm, fast = fast, S = S, ...)
  names(res$statistic) <- "W"
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  res$estimate <- NULL
  res$alternative <- paste0("The distributions of ",
                           paste0(dname, collapse = " and "), 
                           " are unequal.")
  res$method <- "Wasserstein distance permutation test"
  res$data.name <- paste0(dname, collapse = " and ")
  class(res) <- "htest"
  return(res)
}
