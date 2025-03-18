################################################################################
##                      CONSTRAINED MINIMUM DISTANCE                          ##
##                                                                            ##
################################################################################
CMDistanceBinary <- function(X1, X2, cov = FALSE, seed = 42) {
  set.seed(seed)
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
  stopifnot(max(apply(X1, 2, function(x) length(unique(x)))) <= 2 && 
              max(apply(X2, 2, function(x) length(unique(x)))) <= 2)
  X1 <- apply(X1, 2, function(x) as.numeric(as.character(x)))
  X2 <- apply(X2, 2, function(x) as.numeric(as.character(x)))
  if(cov) { # Example 4 in Tatti (2007)
    gamma <- colMeans(X1) - colMeans(X2)
    d.CM <- 0
    for(j in 1:(ncol(X1) - 1)) {
      for(k in (j+1):ncol(X1)) {
        gamma.jk <- mean(X1[, j] * X1[, k]) - mean(X2[, j] * X2[, k])
        d.CM <- d.CM + (gamma[j] + gamma[k] - 2 * gamma.jk)^2
      }
    }
    d.CM <- sqrt(4 * d.CM + 4 * sum(gamma^2))
  } else { # example 3 in Tatti (2007)
    d.CM <- 2 * norm(colMeans(X1) - colMeans(X2), "2")
  }
  
  names(d.CM) <- "CMD"
  res <- list(statistic = d.CM, p.value = NULL, estimate = NULL, 
              alternative = paste0("The distributions of ", paste0(dname, collapse = " and "),
                                   " are unequal."), 
              method = "Constrained Minimum Distance for binary data", 
              data.name = paste0(dname, collapse = " and "), 
              binary = TRUE, cov = cov, S.fun = ifelse(cov, "mean", "mean and cov"), 
              cov.S = 0.25 * diag(ncol(X1)), Omega = NULL)
  class(res) <- "htest"
  return(res)
}

calc.cov.S <- function(Omega, S.fun) {
  S.Omega <- apply(Omega, 1, S.fun)
  if(is.null(dim(S.Omega))) S.Omega <- t(S.Omega)
  cov.S <- 1 / nrow(Omega) * Reduce('+', apply(S.Omega, 2, function(x) x %*% t(x), simplify = FALSE))
  cov.S <- drop(cov.S - rowMeans(S.Omega) %*% t(rowMeans(S.Omega)))
  return(cov.S)
}

check.binary <- function(X1, X2) {
  return(max(apply(cbind(X1, X2), 2, function(x)length(unique(x)))) == 2)
}

calc.Omega <- function(X1, X2) {
  return(expand.grid(lapply(rbind(X1, X2), function(x) 
    sort(unique(as.numeric(as.character(x)))))))
}


CMDistance <- function(X1, X2, binary = NULL, cov = FALSE,
                       S.fun = function(x) as.numeric(as.character(x)), 
                       cov.S = NULL, Omega = NULL, seed = 42) {
  set.seed(seed)
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
  # special case: binary data
  if(is.null(binary)) {
    cov.S <- check.binary(X1, X2)
  }
  if(binary) return(CMDistanceBinary(X1, X2, cov, seed = seed))
  # else: use supplied feature function and its covariance
  stopifnot(!is.null(cov.S) || !is.null(Omega))
  theta.1 <- apply(X1, 1, S.fun)
  theta.2 <- apply(X2, 1, S.fun)
  theta.1 <- if(is.null(dim(theta.1))) rowMeans(t(theta.1)) else rowMeans(theta.1)
  theta.2 <- if(is.null(dim(theta.2))) rowMeans(t(theta.2)) else rowMeans(theta.2)
  # if no sample space is given, calculate Omega
  if(is.null(Omega)) {
    Omega <- calc.Omega(X1, X2)
  }
  # if no covariance matrix is given, calculate cov(S)
  if(is.null(cov.S)) {
    cov.S <- calc.cov.S(Omega, S.fun)
  }
  d.CM <- sqrt(drop(t(theta.1 - theta.2) %*% solve(cov.S) %*% (theta.1 - theta.2)))
  names(d.CM) <- "CMD"
  res <- list(statistic = d.CM, p.value = NULL, estimate = NULL, 
              alternative = paste0("The distributions of ", paste0(dname, collapse = " and "),
                                   " are unequal."), 
              method = "Constrained Minimum Distance", 
              data.name = paste0(dname, collapse = " and "), 
              binary = binary, cov = cov, S.fun = S.fun, cov.S = cov.S, 
              Omega = Omega)
  class(res) <- "htest"
  return(res)
}


