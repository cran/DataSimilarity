################################################################################
##              HELPER FUNCTIONS FOR CROSS MATCH BASED FUNCTIONS              ##
##                                                                            ##
################################################################################
MMCMStatistic <- function(a_N, n.vec) {
  K <- length(n.vec)
  N <- sum(n.vec)
  # create an object to track the indices of the objects in a_N
  inds <- utils::combn(K, 2)
  len_a_N <- ncol(inds)
  
  # Calculate MMCM statistic and moments of exact distribution
  E_0 <- outer(n.vec, n.vec) / (N - 1)
  E_0 <- t(E_0)[upper.tri(t(E_0))]
  
  # create covariance matrix
  Cov_0 <- matrix(0, ncol = len_a_N, nrow = len_a_N)
  for(i in 1:len_a_N) {
    for(j in 1:len_a_N) {
      inds1 <- inds[, i]
      inds2 <- inds[, j]
      # if i==j use variance
      if(i == j) {
        s1 <- (n.vec[inds1[1]] * n.vec[inds1[2]] * (n.vec[inds1[1]]-1) * 
                 (n.vec[inds1[2]]-1)) / ((N-1)*(N-3))
        s2 <- ((n.vec[inds1[1]] * n.vec[inds1[2]]) / (N-1)) * 
          (1 - ((n.vec[inds1[1]] * n.vec[inds1[2]]) / (N-1)))
        Cov_0[i, j] <- s1 + s2
      } else { # ...else: calculate covariances
        if(any(inds1 %in% inds2)) {
          # calculate 'common' covariance
          # therefore, extract common index
          current_inds <- sort(c(inds1, inds2))
          common_ind <- unique(current_inds)[!diff(current_inds)]
          # common_ind <- current_inds[duplicated(current_inds)]
          uncommon_ind <- current_inds[!(current_inds %in% common_ind)]
          s1 <- (n.vec[common_ind] * (n.vec[common_ind]-1) * n.vec[uncommon_ind[1]] * 
                   n.vec[uncommon_ind[2]]) / ((N-1)*(N-3))
          s2 <- (n.vec[common_ind]^2 * n.vec[uncommon_ind[1]] *
                   n.vec[uncommon_ind[2]]) / (N-1)^2
          Cov_0[i, j] <- s1 - s2
        } else {
          # calculate 'uncommon' covariance
          Cov_0[i, j] <- (2 * n.vec[inds1[1]] * n.vec[inds1[2]] * n.vec[inds2[1]] * 
                            n.vec[inds2[2]]) / ((N-1)^2 * (N-3))
        }
      }
    }
  }
  S_KN <- drop(t(as.matrix(a_N) - as.matrix(E_0)) %*% solve(Cov_0) %*% 
                 (as.matrix(a_N) - as.matrix(E_0)))
  df <- len_a_N
  return(list(S_KN = S_KN, df = df))
}

nbmatch <- function(data.list, n.vec, dist.fun, dist.args) {
  if(!requireNamespace("nbpMatching", quietly = TRUE)) {
    stop("Package \"nbpMatching\" required for using method nbmatch().")
  }
  K <- length(n.vec)
  N <- sum(n.vec)
  data.pooled <- do.call(rbind, data.list)
  # create distance matrix of pooled data
  D <- do.call(dist.fun, c(list(data.pooled), dist.args))
  # restructure for nonbimatch
  D_nbpm <- nbpMatching::distancematrix(as.matrix(D))
  # rename row- and colnames to identify from which dataset the data comes
  #   skip ghost entry in case of odd pooled sample size
  if(N %% 2 != 0) {
    ghost_ind <- which(colnames(D_nbpm) == "ghost")
    rownames(D_nbpm)[-ghost_ind] <- rep(1:K, n.vec)
    colnames(D_nbpm)[-ghost_ind] <- rep(1:K, n.vec)
  } else {
    rownames(D_nbpm) <- rep(1:K, n.vec)
    colnames(D_nbpm) <- rep(1:K, n.vec)
  } 
  # perform nonbimatching
  res <- nbpMatching::nonbimatch(D_nbpm)
  A_N <- table(factor(res$halves[, "Group1.ID"], levels = 1:K),
               factor(res$halves[, "Group2.ID"], levels = 1:K))
  
  # create 'vector' a_N, in the same order as in the paper
  a_N <- matrix(0, ncol = K, nrow = K)
  for(i in 1:(K-1)) {
    for(j in (i+1):K) {
      a_N[i, j] <- A_N[i, j] + A_N[j, i]
    }
  }
  a_N <- t(a_N)[lower.tri(t(a_N))]
  return(a_N)
}
