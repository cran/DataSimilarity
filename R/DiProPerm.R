################################################################################
##                   DIRECTION PROJECTION PERMUTATION TEST                    ##
##                                                                            ##
################################################################################
DiProPerm <- function(X1, X2, n.perm = 0, dipro.fun = dwdProj, stat.fun = MD, 
                      direction = "two.sided", seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  stopifnot(n.perm >= 0)
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
  colnames(X1) <- colnames(X2) <- paste0("X", 1:ncol(X1))
  
  proj <- dipro.fun(X1, X2)
  stat <- stat.fun(proj[1:nrow(X1)], proj[(nrow(X1) + 1):(nrow(X1) + nrow(X2))])
  
  if(n.perm <= 0) {
    pval <- NULL
  } else {
    calcStatDiProPermBoot <- function(pooled.samp, ind, n1, n2) {#
      pooled.samp <- pooled.samp[ind, , drop = FALSE]
      proj <- dipro.fun(pooled.samp[1:n1, , drop = FALSE], 
                        pooled.samp[(n1+1):(n1+n2),  , drop = FALSE])
      stat <- stat.fun(proj[1:n1], proj[(n1 + 1):(n1 + n2)])
      return(stat)
    }
    perm.dist <- boot::boot(rbind(X1, X2), statistic = calcStatDiProPermBoot, R = n.perm, 
                            sim = "permutation", n1 = nrow(X1), n2 = nrow(X2))$t
    if(direction == "two.sided") {
      pval <- 2 * min(mean(perm.dist > stat), mean(perm.dist < stat))
    } else if(direction == "greater") {
      pval <- mean(perm.dist > stat)
    } else if(direction == "smaller") {
      pval <- mean(perm.dist < stat)
    } else {
      stop("direction must be \"two.sided\", \"greater\", or \"smaller\".")
    }
  }
  names(stat) <- deparse1(substitute(stat.fun))
  res <- list(statistic = stat, 
              p.value = pval, 
              estimate = NULL,
              alternative = paste0("The distributions of ", paste0(dname, collapse = " and "), 
                                   " are unequal."), 
              method = "DiProPerm Test", 
              data.name = paste0(dname, collapse = " and "))
  class(res) <- "htest"
  return(res)
}
