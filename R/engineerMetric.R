################################################################################
##                            ENGINEER METRIC                                 ##
##                                                                            ##
################################################################################
engineerMetric <- function(X1, X2, type = "F", seed = 42) {
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
  stat <- norm(as.matrix(colMeans(X1) - colMeans(X2)), type = type)
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  names(stat) <- "E"
  res <- list(statistic = stat, 
              p.value = NULL, 
              alternative = paste0("The distributions of ", 
                                   paste0(dname, collapse = " and "), 
                                   " are unequal."), 
              method = "Engineer Metric", 
              data.name = paste0(dname, collapse = " and "))
  class(res) <- "htest"
  return(res)
}
