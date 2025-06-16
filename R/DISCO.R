################################################################################
##                                  DISCO                                     ##
##                                                                            ##
################################################################################
DISCOF <- function(X1, X2, ..., n.perm = 0, alpha = 1, seed = NULL) {
  if(!requireNamespace("energy", quietly = TRUE)) {
    stop("Package \"energy\" required for using method DISCOF().")
  }
  data.list <- c(list(X1, X2), list(...))
  if(length(data.list) == 2) {
    dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  } else {
    mc <- as.list(match.call())
    mc <- mc[!names(mc) %in% c("n.perm", "alpha", "seed")]
    dname <- sapply(mc[-1], deparse)
  }
  discoWrapper(data.list, n.perm = n.perm, method = "discoF", dname = dname, 
                seed = seed, alpha = alpha)
}

DISCOB <- function(X1, X2, ..., n.perm = 0, alpha = 1, seed = NULL) {
  if(!requireNamespace("energy", quietly = TRUE)) {
    stop("Package \"energy\" required for using method DISCOB().")
  }
  data.list <- c(list(X1, X2), list(...))
  if(length(data.list) == 2) {
    dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  } else {
    mc <- as.list(match.call())
    mc <- mc[!names(mc) %in% c("n.perm", "alpha", "seed")]
    dname <- sapply(mc[-1], deparse)
  }
  discoWrapper(data.list, n.perm = n.perm, method = "discoB", dname = dname, 
                seed = seed, alpha = alpha)
}
