################################################################################
##                            BISWAS & GHOSH (2014)                           ##
##                                                                            ##
################################################################################
BG2 <- function(X1, X2, n.perm = 0, seed = 42) {
  set.seed(seed)
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  n1 <- nrow(X1)
  n2 <- nrow(X2)
  N <- n1 + n2
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
  dists <- as.matrix(stats::dist(rbind(X1, X2)))
  mu.FF <- mean(dists[1:n1, 1:n1][upper.tri(dists[1:n1, 1:n1], diag = FALSE)]) 
  mu.GG <- mean(dists[(n1 + 1):N, (n1 + 1):N][upper.tri(dists[(n1 + 1):N, (n1 + 1):N], 
                                                        diag = FALSE)])
  mu.FG <- mean(dists)
  T.n1n2 <- sum(c(mu.FF - mu.FG, mu.FG - mu.GG)^2)
  S1 <- 0 
  for(i in 1:(n1 - 2)) {
    for(j in (i + 1):(n1 - 1)) {
      for(k in (j + 1):n1) {
        S1 <- S1 + dists[i, j] * dists[i, k]
      }
    }
  }
  S1 <- S1 / choose(n1, 3) - mu.FF^2
  S2 <- 0 
  for(i in (n1 + 1):(N - 2)) {
    for(j in (i + 1):(N - 1)) {
      for(k in (j + 1):N) {
        S2 <- S2 + dists[i, j] * dists[i, k]
      }
    }
  }
  S2 <- S2 / choose(n2, 3) - mu.GG^2
  T.n1n2.star <- n1 * n2 / 2 / (n1 * S1 + n2 * S2) * T.n1n2
  if(n.perm <= 0) {
    pval <- pchisq(T.n1n2.star, df = 1, lower.tail = FALSE)
    names(T.n1n2.star) <- "chisq"
    param <- 1
    names(param) <- "df"
  } else {
    calcStat <- function(dists, ind) {
      dists <- dists[ind, ind]
      mu.FF <- mean(dists[1:n1, 1:n1][upper.tri(dists[1:n1, 1:n1], diag = FALSE)])
      mu.GG <- mean(dists[(n1 + 1):N, (n1 + 1):N][upper.tri(dists[(n1 + 1):N, (n1 + 1):N], 
                                                                               diag = FALSE)])
      mu.FG <- mean(dists)
      T.n1n2 <- sum(c(mu.FF - mu.FG, mu.FG - mu.GG)^2)
    }
    perm.dist <- boot::boot(dists, calcStat, sim = "permutation", R = n.perm)$t
    pval <- mean(perm.dist > T.n1n2)
    names(T.n1n2.star) <- "T.star"
    param <- NULL
  }
  names(T.n1n2) <- "T"
  res <- list(statistic = T.n1n2.star, parameter = param,
              p.value = pval, 
              estimate = T.n1n2,
              alternative = paste0("The distributions of ", paste0(dname, collapse = " and "), 
                                   " are unequal."), 
              method = paste0(ifelse(n.perm > 0, "Permutation ", "Approximative "), 
                              " Biswas and Ghosh (2014) Two-Sample Test"), 
              data.name = paste0(dname, collapse = " and "))
  class(res) <- "htest"
  return(res)
}
