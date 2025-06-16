################################################################################
##                      CLASSIFIER TWO-SAMPLE TEST (C2ST)                     ##
##                                                                            ##
################################################################################
C2ST <- function(X1, X2, ..., split = 0.7, thresh = 0, classifier = "knn", 
                 control = NULL, train.args = NULL, 
                 seed = NULL) {
  if(!requireNamespace("Ecume", quietly = TRUE)) {
    stop("Package \"Ecume\" required for using method C2ST().")
  }
  if(is.null(control)) {
    control <- caret::trainControl(method = "cv")
  }
  if(!is.null(seed)) {
    set.seed(seed)
  }
  data.list <- c(list(X1, X2), list(...))
  if(any(!sapply(data.list, function(x) inherits(x, "matrix") | inherits(x, "data.frame")))) {
    stop("All datasets must be provided as data.frames or matrices.")
  }
  p <- sapply(data.list, ncol)
  if(length(unique(p)) > 1) 
    stop("all datasets must have the same number of variables")
  K <- length(data.list)
  n.vec <- sapply(data.list, nrow)
  data.list <- lapply(data.list, function(X) {
    colnames(X) <- paste0("X", 1:p[1])
    X
  })
  res <- do.call(Ecume::classifier_test, 
                 c(list(x = data.list, split = split, 
                        thresh = thresh, method = classifier, 
                        control = control), 
                   train.args))
  
  param <- c(round(split * sum(n.vec)), max(n.vec) / sum(n.vec))
  names(param) <- c("size", "prob")
  names(res$statistic) <- "p.hat"
  if(K == 2) {
    dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  } else {
    mc <- as.list(match.call())
    mc <- mc[!names(mc) %in% c("split", "thresh", "classifier", "control", "train.args", 
                               "seed")]
    dname <- sapply(mc[-1], deparse)
  }
  dname <- paste0(dname, collapse = ifelse(K > 2, ", ", " and "))
  res <- list(statistic = res$statistic, parameter = param, 
              p.value = res$p.value, 
              estimate = NULL, 
              alternative = ifelse(K > 2, "At least one pair of distributions are unequal.", 
                                   paste0("The distributions of ", dname, " are unequal.")),
              method = paste0("Approximative Classifier Two-Sample Test using ", 
                              classifier), 
              data.name = dname, 
              classifier = classifier)
  class(res) <- "htest"
  return(res)
}
