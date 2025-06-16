################################################################################
##                            Deb and Sen (2021)                              ##
##                                                                            ##
################################################################################
DS <- function(X1, X2, n.perm = 0, rand.gen = NULL, seed = NULL) {
  if(!requireNamespace("randtoolbox", quietly = TRUE) | 
     !requireNamespace("clue", quietly = TRUE)) {
    stop("Packages \"randtoolbox\" and \"clue\" required for using method DS().")
  }
  if(is.null(rand.gen)) {
    rand.gen <- randtoolbox::halton
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
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  m <- nrow(X1)
  n <- nrow(X2)
  p <- ncol(X1)
  stopifnot(ncol(X2) == p)
  grid <- as.matrix(rand.gen(m + n, p))
  obs.stat <- computeStatistic(as.matrix(X1), as.matrix(X2), gridch = grid)
  if (n.perm > 0){
    dist.h0 <- genNullDist(m, n, p, niter = n.perm, fixgrid = grid)
    pval <- mean(dist.h0 > obs.stat)
  } else {
    pval <- NULL
  }
  
  stat <- obs.stat
  names(stat) <- "RE^2"
  res <- list(statistic = stat, p.value = pval, 
              alternative = paste0("Distributions of ", dname[1], " and ", dname[2], 
                                   " are unequal"), 
              method = "Rank-based Energy Statistic",  
              data.name = paste0(dname, collapse = " and "))
  class(res) <- "htest"
  return(res)
}
