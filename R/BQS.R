################################################################################
##                          Barakat et al. (1996)                             ##
##                                                                            ##
################################################################################
BQS <- function(X1, X2, dist.fun = stats::dist,
               n.perm = 0, dist.args = NULL, seed = 42) {
  set.seed(seed)
  stopifnot(n.perm >= 0)
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
  colnames(X1) <- colnames(X2) <- paste0("X", 1:ncol(X1))
  dists <- do.call(dist.fun, c(list(rbind(X1, X2)), dist.args))
  n1 <- nrow(X1)
  n2 <- nrow(X2)
  N <- n1 + n2
  R <- apply(as.matrix(dists), 2, order)[-1, ]
  W <- sum(cbind(apply(R[, 1:n1] <= n1, 2, cumsum),  apply(R[, (n1+1):N] > n1, 2, cumsum)))
  if(n.perm <= 0) {
    pval <- NULL
  } else {
    calcStatBQSBoot <- function(dists, ind) {
      dists <- dists[ind, ind]
      R.mat <- apply(as.matrix(dists), 2, order)[-1, ]
      W <- sum(cbind(apply(R.mat[, 1:n1] <= n1, 2, cumsum),  apply(R.mat[, (n1+1):N] > n1, 2, cumsum)))
    }
    perm.dist <- boot::boot(as.matrix(dists), statistic = calcStatBQSBoot, R = n.perm, 
                            sim = "permutation")$t
    pval <- mean(perm.dist > W)
  }
  names(W) <- "W"
  res <- list(statistic = W, 
              p.value = pval, 
              estimate = NULL,
              alternative = paste0("The distributions of ", paste0(dname, collapse = " and "), 
                                   " are unequal."), 
              method = paste0(ifelse(n.perm > 0, "Permutation ", "Approximative "), 
                              " Barakat et al. (1996) Nearest-Neighbor Test"), 
              data.name = paste0(dname, collapse = " and "))
  class(res) <- "htest"
  return(res)
}
