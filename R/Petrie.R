################################################################################
##                                Petrie (2016)                               ##
##                                                                            ##
################################################################################
Petrie <- function(X1, X2, ..., dist.fun = stats::dist, dist.args = NULL, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  data.list <- c(list(X1, X2), list(...))
  if(any(!sapply(data.list, function(x) inherits(x, "matrix") | inherits(x, "data.frame")))) {
    stop("All datasets must be provided as data.frames or matrices.")
  }
  p <- sapply(data.list, ncol)
  if(length(unique(p)) > 1) {
    stop("All datasets must have the same number of variables")
  }
  n.vec <- sapply(data.list, nrow)
  N <- sum(n.vec)
  K <- length(data.list)
  data.list <- lapply(data.list, function(X) {
    colnames(X) <- paste0("X", 1:p[1])
    X
  })
  
  # calculate crossmatch and return vector a_N
  a_N <- nbmatch(data.list, n.vec, dist.fun, dist.args)
  # MCM / Petrie statistic
  R_KN <- sum(a_N)
  
  # caluclate null distribution
  n.vec_squared <- n.vec %*% t(n.vec)
  G_1 <- sum(n.vec_squared[upper.tri(n.vec_squared)])
  E_0 <- G_1/(N-1)
  G_2 <- 0.5 * sum(n.vec * (N-n.vec) * (N-n.vec-1))
  Var_0 <- E_0 * (1 - E_0) + (G_1^2 - G_1 - 2*G_2)/((N-1) * (N-3)) 
  
  Q_KN <- (R_KN - E_0) / sqrt(Var_0)
  lower.pval <- stats::pnorm(Q_KN)
  
  stat <- Q_KN
  names(stat) <- "z"
  mc <- as.list(match.call())
  mc <- mc[!names(mc) %in% c("dist.fun", "dist.args", "seed")]
  est <- R_KN
  names(est) <- "mult.edge.count"
  stderr <- sqrt(Var_0)
  mu0 <- E_0
  dname <- paste0(sapply(mc[-1], deparse), 
                  collapse = ifelse(length(data.list) > 2, ", ", " and "))
  res <- list(statistic = stat, 
              p.value = lower.pval, estimate = est, 
              alternative = ifelse(K > 2, "At least one pair of distributions are unequal.", 
                                   paste0("The distributions of ", dname, " are unequal.")), 
              method = "Approximative Petrie (2016) test",  
              data.name = dname, 
              stderr = stderr, mu0 = mu0)
  class(res) <- "htest"
  return(res)
}