################################################################################
##                              Li et al. (2022)                              ##
##                                                                            ##
################################################################################
LHZ <- function(X1, X2, n.perm = 0, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
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
  X1 <- as.matrix(X1)
  X2 <- as.matrix(X2)
  t <- LHZStatistic(X1, X2)
  
  # permutation test
  p.val <- NULL
  if(n.perm > 0) {
    colnames(X1) <- colnames(X2) <- paste0("X", 1:ncol(X1))
    X <- rbind(X1, X2)
    set.seed(seed)
    dist <- sapply(1:n.perm, function(i) {
      ind1 <- sample(nrow(X), nrow(X1), replace = FALSE)
      x <- X[ind1, , drop = FALSE]
      y <- X[-(ind1), , drop = FALSE]
      LHZStatistic(X1 = x, X2 = y)
    })
    p.val <- mean(dist >= t) 
  }
  
  names(t) <- "statistic"
  
  res <- list(statistic = t, 
              p.value = p.val, 
              estimate = NULL, 
              alternative = paste0("The characteristic functions of ",  
                                   paste0(dname, collapse = " and "), 
                                   " are unequal."), 
              method = "Li et al. (2022) permutation test",  
              data.name = paste0(dname, collapse = ", "),
              parameters = NULL)
  class(res) <- "htest"
  return(res)
}
