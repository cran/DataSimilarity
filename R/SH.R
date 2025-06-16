################################################################################
##                    Schilling (1986) and Henze (1988)                       ##
##                                                                            ##
################################################################################
checkEdgeInE <- function(e, E) {
  any(apply(E, 1, function(x) all(x == e)))
}

SH <- function(X1, X2, K = 1, graph.fun = knn.bf, dist.fun = stats::dist,
               n.perm = 0, dist.args = NULL, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  stopifnot(n.perm >= 0)
  if(!(inherits(X1, "matrix") | inherits(X1, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(!(inherits(X2, "matrix") | inherits(X2, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(ncol(X1) != ncol(X2)) {
    stop("All datasets must have the same number of variables.")
  }
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  colnames(X1) <- colnames(X2) <- paste0("X", 1:ncol(X1))
  dists <- do.call(dist.fun, c(list(rbind(X1, X2)), dist.args))
  E <- graph.fun(dists, K = K)
  n1 <- nrow(X1)
  n2 <- nrow(X2)
  N <- n1 + n2
  L <- sum((E[, 1] <= n1 & E[, 2] <= n1) | (E[, 1] > n1 & E[, 2] > n1))
  mu0 <- K * (n1 * (n1 - 1) + n2 * (n2 - 1)) / (N - 1)
  # Calculate all quantities needed for calculating variance under H0
  # Calculate v_n^(k)
  sum.v <- 0
  for(i in 1:nrow(E)) {
    if(checkEdgeInE(rev(E[i, ]), E)) {
      sum.v <- sum.v + 1
    }
  }
  v <- 1 / N / K * sum.v
  # Calculate q(n_1, n_2)
  q <- 4 * (n1 - 1) * (n2 - 1) / (N - 2) / (N - 3)
  # Calculate in-degrees 
  d <- table(factor(E[, 2], levels = 1:N))
  # Calculate c_n^(k)
  c <- 1 / N / K * sum((d - K)^2)
  v0 <- K * n1 * n2 / (N - 1) * (q * (1 + v - 2 * K / (N - 1)) + (1 - q) * c)
  # Standardized test statistic
  Z <- (L - mu0) / sqrt(v0)
  if(n.perm <= 0) {
    pval <- pnorm(Z, lower.tail = FALSE)
  } else {
    calcStatSHBoot <- function(dists, ind) {
      dists <- dists[ind, ind]
      E <- graph.fun(dists, K = K)
      L <- sum((E[, 1] <= n1 & E[, 2] <= n1) | (E[, 1] > n1 & E[, 2] > n1))
      return(L)
    }
    perm.dist <- boot::boot(as.matrix(dists), statistic = calcStatSHBoot,
                            R = n.perm, sim = "permutation")$t
    pval <- mean(perm.dist > L)
  }
  names(Z) <- "Z"
  names(L) <- "edge.count"
  res <- list(statistic = Z, 
              p.value = pval, 
              estimate = L,
              alternative = paste0("The distributions of ",
                                   paste0(dname, collapse = " and "), 
                                   " are unequal."), 
              method = paste0(ifelse(n.perm > 0, "Permutation ", "Approximative "), 
                              " Schilling and Henze Nearest-Neighbor Test"), 
              data.name = paste0(dname, collapse = " and "))
  class(res) <- "htest"
  return(res)
}
