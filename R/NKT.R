################################################################################
##                          Ntoutsi et al. (2008)                             ##
##                                                                            ##
################################################################################
NKT <- function(X1, X2, target1 = "y", target2 = "y", method = 1, tune = TRUE, k = 5, 
                n.eval = 100, seed = 42, ...) {
  set.seed(seed)
  stopifnot(method %in% 1:3)
  if(!(inherits(X1, "matrix") | inherits(X1, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(!(inherits(X2, "matrix") | inherits(X2, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(ncol(X1) != ncol(X2)) {
    stop("All datasets must have the same number of variables.")
  }
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  colnames(X1)[colnames(X1) != target1] <- 
    colnames(X2)[colnames(X2) != target2] <- paste0("X", 1:(ncol(X1) - 1))
  colnames(X1)[colnames(X1) == target1] <- 
    colnames(X2)[colnames(X2) == target2] <- "y"
  lev <- unique(c(X1[, "y"], X2[, "y"]))
  if(length(lev) > 10) {
    message(paste0("Your response has more than 10 classes. Are you sure that your ", 
                   "response is categorical?"))
  }
  X1[, "y"] <- factor(X1[, "y"], levels = lev)
  X2[, "y"] <- factor(X2[, "y"], levels = lev)
  GCR <- calculateGCR(X1, X2, tune, k, n.eval, ...)
  res.intersect <- GCR$res.intersect
  sec.parti <- res.intersect$parti
  if(method == 1) {
    P1 <- colSums(calcP(sec.parti, X1, "joint"))
    P2 <- colSums(calcP(sec.parti, X2,"joint"))
    if(!(abs(sum(P1) - 1) < 1e-8 && abs(sum(P2) - 1) < 1e-8)) {
      warning(sprintf(paste0("Something went wrong in the calculation of the attribute ", 
                             "space probability distributions. sum(P1) - 1 = %e ", 
                             "and sum(P2) - 1 = %e."), 
                      abs(sum(P1) - 1), abs(sum(P2) - 1) ))
    }
    s <- affinityCoef(P1, P2)
  } else if(method == 3) {
    P1 <- calcP(GCR$parti1, X1, "conditional")
    P2 <- calcP(GCR$parti2, X2, "conditional")
    S.C.A <- apply(res.intersect$combis, 1, function(comb) {
      s <- affinityCoef(as.numeric(P1[, comb[1]]), as.numeric(P2[, comb[2]]))
    })
    if(!(all(abs(colSums(P1) - 1) < 1e-8) && all(abs(colSums(P2) - 1) < 1e-8))) {
      warning(sprintf(paste0("Something went wrong in the calculation of the attribute ", 
                             "space probability distributions. sum(P1) - 1 = %e ", 
                             "and sum(P2) - 1 = %e."), 
                      max(abs(colSums(P1) - 1)), max(abs(colSums(P2) - 1) )))
    }
    P.A <- colSums(calcP(sec.parti, rbind(X1, X2), "joint"))
    s <- drop(t(S.C.A) %*% P.A)
  } else if(method == 2) {
    P1 <- calcP(sec.parti, X1, "joint")
    P2 <- calcP(sec.parti, X2, "joint")
    if(!(abs(sum(P1) - 1) < 1e-8 && abs(sum(P2) - 1) < 1e-8)) {
      warning(sprintf(paste0("Something went wrong in the calculation of the attribute ", 
                             "space probability distributions. sum(P1) - 1 = %e ", 
                             "and sum(P2) - 1 = %e."), 
                      abs(sum(P1) - 1), abs(sum(P2) - 1) ))
    }
    s <- affinityCoef(as.numeric(P1), as.numeric(P2))
  }
  names(s) <- "s"
  res <- list(statistic = s, 
              p.value = NULL, 
              estimate = NULL,
              alternative = paste0("The distributions of ", 
                                   paste0(dname, collapse = " and "), 
                                   " are unequal."), 
              method = paste0("Data similarity accorcing to Ntoutsi et al. (2008), method ", method),  
              data.name = paste0(dname, collapse = " and "), 
              method = method)
  class(res) <- "htest"
  return(res)
}