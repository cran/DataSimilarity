################################################################################
##                          SHORTEST HAMILTON PATH                            ##
##                                                                            ##
################################################################################
HamiltonPath <- function(X1, X2, seed = 42) {
  if(!requireNamespace("rlemon", quietly = TRUE)) {
    stop("Package \"rlemon\" required for using method HamiltonPath().")
  }
  m <- nrow(X1)
  n <- nrow(X2)
  N <- m + n
  stopifnot(ncol(X1) == ncol(X2))
  colnames(X1) <- colnames(X2) <- paste0("X", 1:ncol(X1))
  dists <- as.matrix(dist(rbind(X1, X2), upper = TRUE))
  # extract vertices corresponding to (sorted) edges
  set.seed(seed)
  v1 <- unlist(sapply(1:(N-1), function(n) 1:n))
  v2 <- unlist(sapply(1:(N-1), function(n) rep(n + 1, n)))
  E <- cbind(v1, v2)
  ord <- order(dists[upper.tri(dists)])
  E <- E[ord, ]
  
  # store used edges
  resVer <- numeric(N)
  resVer[E[1, ]] <- 1
  indices <- 1
  
  for(i in 2:nrow(E)) {
    cand <- E[i, ]
    ## check for cycle (requires package rlemon) and degree condition
    if(all(resVer[cand] < 2) & rlemon::IsAcyclic(E[c(indices, i), 1], E[c(indices, i), 2], N)) {
      resVer[cand] <- resVer[cand] + 1
      indices <- c(indices, i)
    }
    if(sum(resVer) == 2 * (N - 1)) {
      break
    }
  }
  
  return(E[indices, ])
}
