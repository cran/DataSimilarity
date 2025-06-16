################################################################################
##                          Biau und Gyorfi (2005)                            ##
##                                                                            ##
################################################################################
BG <- function(X1, X2, partition = rectPartition, exponent = 0.8, eps = 0.01,
               seed = NULL, ...) {
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
  X1 <- as.matrix(X1)
  X2 <- as.matrix(X2)
  if(nrow(X1) != nrow(X2)) stop("Sample size of both datasets has to be equal.")
  p <- ncol(X1)
  if(p != ncol(X2)) stop("Numbers of variables of both datasets has to be equal.")
  n <- nrow(X1)
  
  part <- partition(X1, X2, n, p, exponent = exponent, eps = eps, ...)
  Acut <- part[[1]]
  m_n <- part[[2]]
  m_n_d <- part[[3]]
  
  a1p <- a2p <- vector(mode = "list", length = p)
  for(i in 1:p) {
    a1p[[i]] <- cut(X1[, i], Acut[[i]], labels = FALSE)
    a2p[[i]] <- cut(X2[, i], Acut[[i]], labels = FALSE)
  }
  
  a1 <- matrix(unlist(a1p), ncol = p)
  a2 <- matrix(unlist(a2p), ncol = p)
  A1 <- A2 <- rep(0, m_n)
  
  sub <- c(0, rep(1, p - 1)) 
  mult <- m_n_d^(0:(p - 1))
  
  for(i in 1:n) {
    index1 <- (a1[i, ] - sub) %*% mult
    index2 <- (a2[i, ] - sub) %*% mult
    A1[index1] <- A1[index1] + 1
    A2[index2] <- A2[index2] + 1
  }
  
  Tn <- sum(abs(A1 - A2)) / n
  # constants according to corollary to theorem 2 in Biau and Gyorfi (2005)
  c2 <- 2 / sqrt(pi)
  C <- 0.7655
  sigma_sq <- 2 * (1 - (2 / pi))
  T_asymp <- (Tn - (c2 * sqrt(m_n / n) + C * (m_n / n))) / (sqrt(sigma_sq) / sqrt(n))
  pval <- (1 - pnorm(T_asymp))

  names(T_asymp) <- "T_asymp"
  
  res <- list(statistic = T_asymp, 
              p.value = pval, 
              estimate = NULL,  
              alternative = paste0("The distributions of ",  
                                   paste0(dname, collapse = " and "),
                                   " are unequal."), 
              method = "Asymptotic Biau and Gyorfi (2005) test",  
              data.name = paste0(dname, collapse = ", "),
              parameters = NULL)
  class(res) <- "htest"
  return(res)
}
