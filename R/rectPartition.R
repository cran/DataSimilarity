################################################################################
##                            RECTANGULAR PARTITION                           ##
##                                                                            ##
################################################################################
rectPartition <- function(X1, X2, n, p, exponent = 0.8, eps = 0.01) {
  m_n <- floor(n^exponent)
  if(m_n <= 2^p) {
    warning(paste0("number of subsets m_n of chosen partition is smaller or equal ", 
                   "to the number of dimensions p. Set m_n to 2^p to get two ", 
                   "subsets per dimension."))
    m_n <- 2^p
  }
  m_n_d <- floor(m_n^(1 / p))
  A <- lapply(1:p, function(i) seq(from = min(X1[, i], X2[, i]) - eps, 
                                   to = max(X1[, i], X2[, i]), 
                                   length.out = m_n_d + 1))
  return(list(A, m_n, m_n_d))
}
