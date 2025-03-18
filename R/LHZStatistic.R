################################################################################
##                              HELPER FUNCTION                               ##
##                                                                            ##
################################################################################
LHZStatistic <- function(X1, X2) {
  n <- nrow(X1)
  m <- nrow(X2)
  
  sum1 <- matrix(0, ncol = n, nrow = n)
  for(j in 1:(n - 1)) {
    for(q in j:n) {
      arg1 <- X1 %*% (X1[j, ] - X1[q, ])
      arg2 <- X2 %*% (X1[j, ] - X1[q, ])
      mean1 <- mean(complex(real = cos(arg1), imaginary = sin(arg1)))
      mean2 <- mean(complex(real = cos(arg2), imaginary = sin(arg2)))
      sum1[j, q] <- Mod(mean1 - mean2)^2
    }
  }
  
  sum2 <- matrix(0, ncol = m, nrow = m)
  for(j in 1:(m - 1)) {
    for(q in j:m) {
      arg1 <- X1 %*% (X2[j, ] - X2[q, ])
      arg2 <- X2 %*% (X2[j, ] - X2[q, ])
      mean1 <- mean(complex(real = cos(arg1), imaginary = sin(arg1)))
      mean2 <- mean(complex(real = cos(arg2), imaginary = sin(arg2)))
      sum2[j, q] <- Mod(mean1 - mean2)^2
    }
  }
  
  return(2 / n^2 * sum(sum1) + 2 / m^2 * sum(sum2))
}
