################################################################################
##                      OPTIMAL TRANSPORT DATASET DISTANCE                    ##
##                                                                            ##
################################################################################
augmentedDataset <- function(X, target) {
  # calculate statistic
  means <- do.call(cbind, tapply(X[, colnames(X) != target], factor(X[, target]),
                                 colMeans, simplify = FALSE))
  covs <- do.call(cbind, tapply(X[, colnames(X) != target], X[, target], function(x) 
    as.numeric(cov(x))))
  old.ncol <- ncol(X)
  num.means <- ncol(X) - 1
  num.covs <- num.means^2
  # append statistics corresponding to y values
  X <- do.call(cbind, c(list(X), rep(list(NA_real_), num.means + num.covs)))
  for(y.val in unique(X[, target])) {
    X[X[, target] == y.val, (old.ncol + 1):ncol(X)] <- c(means[, as.character(y.val)], 
                                                         covs[, as.character(y.val)])
  }
  colnames(X)[(old.ncol + 1):ncol(X)] <- c(paste0("mu", 1:num.means), paste0("sigma", 1:num.covs))
  return(X[, colnames(X) != target])
}

hammingDist <- function(x) {
  dist <- matrix(NA_real_, nrow = nrow(x), ncol = nrow(x))
  for(i in 1:nrow(x)) {
    for(j in i:nrow(x)) {
      dist[i, j] <- dist[j, i] <- sum(x[i, ] != x[j, ])
    }
  }
  return(dist)
}

OTDD <- function(X1, X2, target1 = "y", target2 = "y", method = "precomputed.labeldist", 
                 feature.cost = stats::dist, lambda.x = 1, lambda.y = 1, 
                 p = 2, ground.p = 2, sinkhorn = FALSE, debias = FALSE,
                 inner.ot.method = "exact", inner.ot.p = 2, inner.ot.ground.p = 2, 
                 inner.ot.sinkhorn = FALSE,  inner.ot.debias = FALSE,
                 seed = NULL) {
  if(!requireNamespace("approxOT", quietly = TRUE) & !requireNamespace("expm", quietly = TRUE)) {
    stop("Packages \"approxOT\" and \"expm\" required for using method OTDD().")
  }
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
  if(ncol(X1) != ncol(X2)) {
    stop("All datasets must have the same number of variables.")
  }
  if(method == "augmentation") {
    if(lambda.x != 1 | lambda.y != 1) 
      stop("Unevenly weighted feature/label not available for method = \"augmentation\" yet.")
    XA <- augmentedDataset(X1, target = target1) 
    XB <- augmentedDataset(X2, target = target2)
    dist <- approxOT::wasserstein(XA, XB, p = p, ground.p = ground.p, 
                                  method = ifelse(sinkhorn, "sinkhorn", "networkflow"), 
                                  unbiased = debias, observation.orientation = "rowwise")
  } else if(method == "precomputed.labeldist") {
    colnames(X1)[colnames(X1) != target1] <- 
      colnames(X2)[colnames(X2) != target2] <- paste0("X", 1:(ncol(X1) - 1))
    
    label.dist <- function(XX1, XX2, targetX1, targetX2, target.ind1, target.ind2, 
                           feat.cost = NULL) {
      if(inner.ot.method == "gaussian.approx") {
        means1 <- tapply(XX1[, -target.ind1], factor(XX1[, targetX1]), 
                         colMeans, simplify = FALSE)
        means2 <- tapply(XX2[, -target.ind2], factor(XX2[, targetX2]), 
                         colMeans, simplify = FALSE)
        covs1 <- tapply(XX1[, -target.ind1], XX1[, targetX1],
                        function(x) cov(x))
        covs2 <- tapply(XX2[, -target.ind2], XX2[, targetX2],
                        function(x) cov(x))
        cost.labels <- matrix(NA_real_, nrow = length(means1), ncol = length(means2))
        for(i in seq(along = means1)) {
          for(j in seq(along = means2)) {
            sqrt.sigA <- expm::sqrtm(covs1[[i]])
            if("complex" %in% typeof(sqrt.sigA)) {
              stop(paste0("Estimated covariance matrix for label ",
                          levels(factor(XX1[, targetX1]))[i], 
                          " for the first dataset is numerically not psd. Gaussian ", 
                          "approximation of label distance cannot be computed. ", 
                          "Please use different inner.ot.method."))
            }
            cost.labels[i, j] <- sum((means1[[i]] - means2[[j]])^2) + 
              sum(diag(covs1[[i]]) + diag(covs2[[j]]) - 
                    2 * diag(expm::sqrtm(sqrt.sigA %*% covs2[[j]] %*% sqrt.sigA)))
          }
        }
      } else if(inner.ot.method == "exact") {
        c1 <- unique(XX1[, targetX1])
        c2 <- unique(XX2[, targetX2])
        cost.labels <- matrix(NA_real_, nrow = length(c1), ncol = length(c2))
        for(i in seq(along = c1)) {
          for(j in seq(along = c2)) {
            cost <- feat.cost[XX1[, targetX1] == c1[i], XX2[, targetX2] == c2[j], drop = FALSE]^(1 / inner.ot.ground.p)
            if(inner.ot.sinkhorn & inner.ot.debias) {
              cost11 <- feat.cost[XX1[, targetX1] == c1[i], XX1[, targetX1] == c1[i], drop = FALSE]^(1 / inner.ot.ground.p)
              cost22 <- feat.cost[XX2[, targetX2] == c2[j], XX2[, targetX2] == c2[j], drop = FALSE]^(1 / inner.ot.ground.p)
            }
            cost.labels[i, j] <- approxOT::wasserstein(a = rep(1/sum(XX1[, targetX1] == c1[i]), 
                                                               sum(XX1[, targetX1] == c1[i])),
                                                       b = rep(1/sum(XX2[, targetX2] == c2[j]), 
                                                               sum(XX2[, targetX2] == c2[j])), 
                                                       cost = cost,
                                                       cost_a = cost11, 
                                                       cost_b = cost22,
                                                       p = inner.ot.p, 
                                                       ground.p = inner.ot.ground.p, 
                                                       method = ifelse(inner.ot.sinkhorn, 
                                                                       "sinkhorn", 
                                                                       "networkflow"), 
                                                       unbiased = inner.ot.debias, 
                                                       observation.orientation = "rowwise")^inner.ot.p
          }
        }
      } else if(inner.ot.method == "naive.upperbound") {
        means1 <- tapply(XX1[, -target.ind1], factor(XX1[, targetX1]), 
                         colMeans, simplify = FALSE)
        means2 <- tapply(XX2[, -target.ind2], factor(XX2[, targetX2]), 
                         colMeans, simplify = FALSE)
        covs1 <- tapply(XX1[, -target.ind1], XX1[, targetX1],
                        function(x) cov(x))
        covs2 <- tapply(XX2[, -target.ind2], XX2[, targetX2],
                        function(x) cov(x))
        cost.labels <- matrix(NA_real_, nrow = length(means1), ncol = length(means2))
        for(i in seq(along = means1)) {
          for(j in seq(along = means2)) {
            cost.labels[i, j] <- sum((means1[[i]] - means2[[j]])^2) + 
              sum(diag(covs1[[i]]) + diag(covs2[[j]]))
          }
        }
      } else if(inner.ot.method == "means.only") {
        means1 <- tapply(XX1[, -target.ind1], factor(XX1[, targetX1]), 
                         colMeans, simplify = FALSE)
        means2 <-tapply(XX2[, -target.ind2], factor(XX2[, targetX2]), 
                        colMeans, simplify = FALSE)
        cost.labels <- matrix(NA_real_, nrow = length(means1), ncol = length(means2))
        for(i in seq(along = means1)) {
          for(j in seq(along = means2)) {
            cost.labels[i, j] <- sum((means1[[i]] - means2[[j]])^2)
            
          }
        }
      } else {
        stop(paste0("inner.ot.method must be one of \"gaussian.approx\", \"exact\", ", 
                    "\"naive.upperbound\" or \"means.only\""))
      }
      return(cost.labels)
    }
    
    target.ind1 <- which(colnames(X1) == target1)
    target.ind2 <- which(colnames(X1) == target2)
    n1 <- nrow(X1)
    n2 <- nrow(X2)
    
    # Feature distances
    cost.X <- as.matrix(feature.cost(rbind(X1[, -target.ind1], X2[, -target.ind2])))^p
    
    # Label distances
    WX1X2 <- label.dist(X1, X2, target1, target2, target.ind1, target.ind2, 
                        cost.X[1:n1, (n1 + 1):(n1 + n2)])
    
    # Cost matrix
    cost <- matrix(NA_real_, nrow = n1, ncol = n2)
    c1 <- unique(X1[, target1])
    c2 <- unique(X2[, target2])
    for(i in 1:n1) {
      for(j in 1:n2) {
        cost[i, j] <- lambda.x * cost.X[i, n1 + j] + 
          lambda.y * WX1X2[which(c1 == X1[i, target1]), 
                           which(c2 == X2[j, target2])]
      }
    }
    if(sinkhorn & debias) {
      WX1X1 <- label.dist(X1, X1, target1, target1, target.ind1, target.ind1, 
                          cost.X[1:n1, 1:n1])
      WX2X2 <- label.dist(X2, X2, target2, target2, target.ind2, target.ind2, 
                          cost.X[(n1 + 1):(n1 + n2), 
                                 (n1 + 1):(n1 + n2)])
      cost.X1 <- matrix(NA_real_, nrow = n1, ncol = n1)
      for(i in 1:n1) {
        for(j in 1:n1) {
          cost.X1[i, j] <- lambda.x * cost.X[i, j] + 
            lambda.y * WX1X1[which(c1 == X1[i, target1]), 
                             which(c2 == X1[i, target1])]
        }
      }
      cost.X2 <- matrix(NA_real_, nrow = n2, ncol = n2)
      for(i in 1:n2) {
        for(j in 1:n2) {
          cost.X2[i, j] <- lambda.x * cost.X[n1 + i, n1 + j] + 
            lambda.y * WX2X2[which(c1 == X1[i, target2]), 
                             which(c2 == X2[i, target2])]
        }
      }
    }
    
    # OT = Wasserstein_p^p
    dist <- approxOT::wasserstein(a = rep(1/n1, n1),
                                  b = rep(1/n2, n2),
                                  p = p, cost = cost^(1/p),
                                  method = ifelse(sinkhorn,
                                                  "sinkhorn",
                                                  "networkflow"),
                                  cost_a = cost.X1^(1/p),
                                  cost_b = cost.X2^(1/p),
                                  unbiased = debias)^p
  } else {
    stop("Method must be either \"augmentation\" or \"precomputed.labeldist\".")
  }
  stat <- dist
  names(stat) <- "OTDD"
  res <- list(statistic = stat, p.value = NULL, 
              alternative = paste0("Distributions of ", dname[1], " and ", dname[2], 
                                   " are unequal"), 
              method = "Optimal Transport Dataset Distance",  
              data.name = paste0(dname, collapse = " and "))
  class(res) <- "htest"
  return(res)
}
