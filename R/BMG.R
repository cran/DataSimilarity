################################################################################
##                            Biswas et al. (2014)                            ##
##                                                                            ##
################################################################################
BMG <- function(X1, X2, seed = NULL, asymptotic = TRUE) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  if(ncol(X1) != ncol(X2)) {
    stop("All datasets must have the same number of variables.")
  }
  if(!(inherits(X1, "matrix") | inherits(X1, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(!(inherits(X2, "matrix") | inherits(X2, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  X1 <- as.matrix(X1)
  X2 <- as.matrix(X2)
  m <- nrow(X1)
  n <- nrow(X2)
  N <- m + n
  if(N >= 1030 & !asymptotic) {
    warning(paste0("Calculation of the run statistic breaks for N this large. ", 
                   "Instead the asymptotic statistic will be calculated."))
    asymptotic <- TRUE
  }
  
  # method name for output
  mthd <- "Biswas et al. (2014) test"
  
  E <- HamiltonPath(X1, X2, seed = seed)
  
  # T_mn <- 1 + sum(U_i[1:(N-1)]) where U_i indicates if edge i connects 
  # vertices of different distributions (-> nrow(E) = (N-1))
  T_mn <- 1 + sum(apply(E, 1, function(x) (x[1] <= m) != (x[2] <= m)))
  
  if(!asymptotic) {
    # small m, n: run test
    r_e <- ifelse(T_mn %% 2 == 0, T_mn / 2, (T_mn - 1) / 2)
    r_o <- ifelse(T_mn %% 2 == 0, T_mn / 2, (T_mn + 1) / 2)
    p_e <- sum(sapply(1:r_e, function(k) 
      2 * choose(m - 1, k - 1) * choose(n - 1, k - 1) / choose(N, m)))
    p_o <- sum(sapply(2:r_o, function(k) {
      (choose(m - 1, k - 1) * choose(n - 1, k - 2) + 
         choose(m - 1, k - 2) * choose(n - 1, k - 1)) / choose(N, m)
    }))
    p <- p_e + p_o
    pval <- 2 * min(p, 1 - p)
  }
  
  if(asymptotic) {
    # asymptotic null distribution
    lambda <- m / N
    T_ast <- sqrt(N) * (T_mn / N - 2 * lambda * (1 - lambda))
    p <- pnorm(abs(T_ast), mean = 0, sd = sqrt(4 * lambda^2 * (1 - lambda)^2))
    pval <- 2 * min(p, 1 - p)
    # add 'Asymptotic' to method name
    mthd <- paste("Asymptotic", mthd)
  }
  
  names(T_mn) <- "T_mn"
  
  res <- list(statistic = T_mn, 
              p.value = pval, 
              estimate = NULL, 
              alternative = paste0("The distributions of ",  
                                   paste0(dname, collapse = " and "),
                                   " are unequal."), 
              method = mthd,  
              data.name = paste0(dname, collapse = ", "),
              parameters = NULL)
  class(res) <- "htest"
  return(res)
}
