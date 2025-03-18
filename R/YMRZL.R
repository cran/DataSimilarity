################################################################################
##                              Yu et al. (2007)                              ##
##                                                                            ##
################################################################################
YMRZL <- function(X1, X2, n.perm = 0, split = 0.7, 
                  control = NULL, train.args = NULL, 
                  seed = 42) {
  if(is.null(control)) {
    if(!requireNamespace("Ecume", quietly = FALSE)){
      stop("Package \"Ecume\" required for using method YMRZL().")
    }
    control <- caret::trainControl(method = "boot")
  }
  res <- C2ST(X1, X2, split = split, thresh = 0, method =  "rpart", 
                  control = control, train.args = train.args, seed = seed)
  res$statistic <- 1 - res$statistic
  if(n.perm > 0) {
    calcStat <- function(dat.list, ind) {
      dat.list <- dat.list[ind, , drop = FALSE]
      1 - C2ST(dat.list[1:nrow(X1), , drop = FALSE], dat.list[1:nrow(X2), , drop = FALSE], 
               split = split, thresh = 0, method =  "rpart", control = control, 
               train.args = train.args, seed = seed)$statistic
    }
    colnames(X1) <- colnames(X2) <- paste0("X", 1:ncol(X1))
    perm.dist <- boot::boot(as.matrix(rbind(X1, X2)), calcStat, sim = "permutation", 
                            R = n.perm)$t
    res$p.value <- mean(c(0, perm.dist) < res$statistic)
    res$method <- gsub("Approximative", "Permutation", res$method)
  }
  return(res)
}
