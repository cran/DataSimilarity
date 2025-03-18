################################################################################
##                            ENERGY STATISTIC                                ##
##                                                                            ##
################################################################################
Energy <- function(X1, X2, ..., n.perm = 0, seed = 42) {
  if(!requireNamespace("energy", quietly = TRUE)) {
    stop("Package \"energy\" required for using method Energy().")
  }
  data.list <- c(list(X1, X2), list(...))
  if(length(data.list) == 2) {
    dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  } else {
    mc <- as.list(match.call())
    mc <- mc[!names(mc) %in% c("n.perm", "method")]
    dname <- sapply(mc[-1], deparse)
  }
  energyWrapper(data.list, n.perm = n.perm, dname = dname, seed = seed)
}