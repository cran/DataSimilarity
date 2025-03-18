################################################################################
##        CALCULATE EDGE MATRIX OF KNN GRAPH BY DIFFERENT ALGORITHMS          ##
##                                                                            ##
################################################################################
knn.bf <- function(dists, K = 1) {
  dists <- as.matrix(dists)
  N <- nrow(dists)
  # initialize edge matrix
  E <- matrix(NA_integer_, nrow = K * N, ncol = 2)
  E[, 1] <- rep(1:N, each = K)
  # for each observations note the K nearest neighbors
  for(i in 1:N) {
    nns <- order(dists[i, -i])[1:K]
    E[((i-1) * K + 1):(i * K), 2] <- ifelse(nns < i, nns, nns + 1)
  }
  return(E)
}

knn.fast <- function(dists, K = 1) {
  if(!requireNamespace("FNN", quietly = TRUE)) {
    stop("Package \"FNN\" required for using method knn.fast().")
  }
  mat <- FNN::get.knn(dists, k = K)$nn.index
  N <- if(inherits(dists, "dist")) attr(dists, "Size") else ncol(dists)
  E <- cbind(1:N, as.numeric(mat))
  return(E)
}

knn <- function(dists, K = 1) {
  if(!requireNamespace("dbscan", quietly = TRUE)) {
    stop("Package \"dbscan\" required for using method knn().")
  }
  mat <- dbscan::kNN(dists, k = K, sort = FALSE)$id
  N <- if(inherits(dists, "dist")) attr(dists, "Size") else ncol(dists)
  E <- cbind(1:N, as.numeric(mat))
  return(E)
}