################################################################################
##                            Ganti et al. (1999)                             ##
##                                                                            ##
################################################################################
GGRL <- function(X1, X2, target1 = "y", target2 = "y", n.perm = 0, m = 1, 
                 diff.fun = f.a, agg.fun = sum, 
                 tune = TRUE, k = 5, n.eval = 100, seed = 42, ...) {
  set.seed(seed)
  if(!(inherits(X1, "matrix") | inherits(X1, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(!(inherits(X2, "matrix") | inherits(X2, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(ncol(X1) != ncol(X2)) {
    stop("All datasets must have the same number of variables.")
  }
  dname <- c(deparse(substitute(X1)), deparse(substitute(X2)))
  colnames(X1)[colnames(X1) != target1] <- 
    colnames(X2)[colnames(X2) != target2] <- paste0("X", 1:(ncol(X1) - 1))
  colnames(X1)[colnames(X1) == target1] <- 
    colnames(X2)[colnames(X2) == target2] <- "y"
  lev <- unique(c(X1[, "y"], X2[, "y"]))
  if(length(lev) < 2) {
    stop("Target variables of datasets must have at least two levels.")
  }
  if(length(lev) > 10) {
    message(paste0("Your response has more than 10 classes. Are you sure that ", 
                   "your response is categorical?"))
  }
  X1[, "y"] <- factor(X1[, "y"], levels = lev)
  X2[, "y"] <- factor(X2[, "y"], levels = lev)
  GCR <- calculateGCR(X1, X2, tune, k, n.eval, ...)
  res.intersect <- GCR$res.intersect
  sec.parti <- res.intersect$parti
  delta <- agg.fun(diff.fun(sec.parti, X1, X2))
  pvalue <- NA
  if(n.perm > 0) {
    nm <- round(nrow(X1) * m)
    delta0 <- numeric(n.perm + 1)
    for(i in 1:n.perm) {
      X.temp1 <- X1[sample(nrow(X1))[1:nm], ]
      X.temp2 <- X1[sample(nrow(X1))[1:nm], ]
      GCR.tmp <- calculateGCR(X.temp1, X.temp2, tune, k, n.eval, ...)
      sec.parti.tmp <- GCR.tmp$res.intersect$parti
      delta0[i] <- agg.fun(diff.fun(sec.parti.tmp, X.temp1, X.temp2))
    }
    delta0[n.perm + 1] <- 0
    pvalue <- mean(delta0 <= delta)
  }
  names(delta) <- "delta"
  res <- list(statistic = delta, 
              p.value = pvalue, 
              estimate = NULL, 
              alternative = paste0("The distributions of ", 
                                   paste0(dname, collapse = " and "), 
                                   " are unequal."), 
              method = paste0("Data similarity accorcing to Ganti et al. (1999)"),  
              data.name = paste0(dname, collapse = " and "), 
              diff.fun = diff.fun, agg.fun = agg.fun)
  class(res) <- "htest"
  return(res)
}

GGRLCat <- function(X1, X2, target1 = "y", target2 = "y", n.perm = 0, m = 1, 
                    diff.fun = f.aCat, agg.fun = sum, 
                    tune = TRUE, k = 5, n.eval = 100, seed = 42, ...) {
  set.seed(seed)
  if(!(inherits(X1, "matrix") | inherits(X1, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(!(inherits(X2, "matrix") | inherits(X2, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(ncol(X1) != ncol(X2)) {
    stop("All datasets must have the same number of variables.")
  }
  dname <- c(deparse(substitute(X1)), deparse(substitute(X2)))
  X1 <- as.data.frame(X1)
  X2 <- as.data.frame(X2)
  colnames(X1)[colnames(X1) != target1] <- 
    colnames(X2)[colnames(X2) != target2] <- paste0("X", 1:(ncol(X1) - 1))
  colnames(X1)[colnames(X1) == target1] <- 
    colnames(X2)[colnames(X2) == target2] <- "y"
  lev <- unique(c(X1[, "y"], X2[, "y"]))
  if(length(lev) < 2) {
    stop("Target variables of datasets must have at least two levels.")
  }
  if(length(lev) > 10) {
    message(paste0("Your response has more than 10 classes. Are you sure that ", 
                   "your response is categorical?"))
  }
  X1[, "y"] <- factor(X1[, "y"], levels = lev)
  X2[, "y"] <- factor(X2[, "y"], levels = lev)
  GCR <- calculateGCRCat(X1, X2, tune, k, n.eval, ...)
  res.intersect <- GCR$res.intersect
  sec.parti <- res.intersect$parti
  delta <- agg.fun(diff.fun(sec.parti, X1, X2))
  pvalue <- NA
  if(n.perm > 0) {
    nm <- round(nrow(X1) * m)
    delta0 <- numeric(n.perm + 1)
    for(i in 1:n.perm) {
      X.temp1 <- X1[sample(nrow(X1))[1:nm], ]
      X.temp2 <- X1[sample(nrow(X1))[1:nm], ]
      GCR.tmp <- calculateGCRCat(X.temp1, X.temp2, tune, k, n.eval, ...)
      sec.parti.tmp <- GCR.tmp$res.intersect$parti
      delta0[i] <- agg.fun(diff.fun(sec.parti.tmp, X.temp1, X.temp2))
    }
    delta0[n.perm + 1] <- 0
    pvalue <- mean(delta0 <= delta)
  }
  names(delta) <- "delta"
  res <- list(statistic = delta, 
              p.value = pvalue, 
              estimate = NULL, 
              alternative = paste0("The distributions of ", 
                                   paste0(dname, collapse = " and "), 
                                   " are unequal."), 
              method = paste0("Data similarity accorcing to Ganti et al. (1999)"),  
              data.name = paste0(dname, collapse = " and "), 
              diff.fun = diff.fun, agg.fun = agg.fun)
  class(res) <- "htest"
  return(res)
}
