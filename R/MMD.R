################################################################################
##                          MAXIMUM MEAN DISCREPANCY                          ##
##                                                                            ##
################################################################################
MMD <- function(X1, X2, n.perm = 0, alpha = 0.05, asymptotic = FALSE, replace = TRUE, 
                n.times = 150, frac = 1, seed = 42, ...) {
  if(!requireNamespace("kernlab", quietly = TRUE)) {
    stop("Package \"kernlab\" required for using method MMD().")
  }
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
  if(requireNamespace("purrr", quietly = TRUE)){
    mmd.fun <- purrr::quietly(kernlab::kmmd)
  } else {
    mmd.fun <- kernlab::kmmd
  }
  res <- mmd.fun(X1, X2, alpha = alpha, asymptotic = asymptotic, replace = replace, 
                 ntimes = n.times, frac = frac, ...)
  if(requireNamespace("purrr", quietly = TRUE)){
    if(length(res$warnings) > 0) {
      warning(res$warnings)
    }
    stat <- kernlab::mmdstats(res$result)[1]
  } else {
    stat <- kernlab::mmdstats(res)[1]
  }
  if(n.perm > 0) {
    colnames(X1) <- colnames(X2) <- paste0("X", 1:ncol(X1))
    if(requireNamespace("purrr", quietly = TRUE)){
      perm.dist <- as.numeric(boot::boot(rbind(X1, X2), function(pooled.samp, ind) {
        kernlab::mmdstats(do.call(mmd.fun, c(list(x = as.matrix(pooled.samp[ind[1:nrow(X1)], , 
                                                                            drop = FALSE]), 
                                                  y = as.matrix(pooled.samp[ind[nrow(X1) + 1:nrow(X2)], , 
                                                                            drop = FALSE]),
                                                  alpha = alpha,
                                                  asymptotic = asymptotic,
                                                  replace = replace, 
                                                  ntimes = n.times, 
                                                  frac = frac, ...)))$result)[1]
      }, R = n.perm, sim = "permutation")$t)
    } else {
      perm.dist <- as.numeric(boot::boot(rbind(X1, X2), function(pooled.samp, ind) {
        kernlab::mmdstats(do.call(mmd.fun, c(list(x = as.matrix(pooled.samp[ind[1:nrow(X1)], , 
                                                                            drop = FALSE]), 
                                                  y = as.matrix(pooled.samp[ind[nrow(X1) + 1:nrow(X2)], , 
                                                                            drop = FALSE]),
                                                  alpha = alpha,
                                                  asymptotic = asymptotic,
                                                  replace = replace, 
                                                  ntimes = n.times, 
                                                  frac = frac, ...)))$result)[1]
      }, R = n.perm, sim = "permutation")$t)
    }
  }
  
  names(stat) <- "MMD"
  res <- list(statistic = stat, 
              p.value = ifelse(n.perm > 0, 
                               mean(perm.dist > stat), 
                               NA), 
              estimate = NULL,
              alternative = paste0("The distributions of ", 
                                   paste0(dname, collapse = " and "), 
                                   " are unequal."),
              method = "1st Order Maximum Mean Discrepancy (MMD)", 
              data.name = paste0(dname, collapse = " and "), 
              H0 = kernlab::H0(res$result), asymp.H0 = kernlab::AsympH0(res$result), 
              kernel.fun = kernlab::kernelf(res$result), 
              Rademacher.bound = kernlab::Radbound(res$result), 
              asymp.bound = ifelse(asymptotic, kernlab::Asymbound(res$result), NA))
  class(res) <- "htest"
  return(res)
}
