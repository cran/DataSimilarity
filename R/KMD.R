################################################################################
##              KERNEL MEASURE OF MULTI-SAMPLE DISSIMILARITY                  ##
##                                                                            ##
################################################################################
KMD <- function(X1, X2, ..., n.perm = 0, graph = "knn", k = ceiling(N/10), 
                kernel = "discrete", seed = NULL) {
  if(!requireNamespace("KMD", quietly = TRUE)) {
    stop("Package \"KMD\" required for using method KMD().")
  }
  if(!is.null(seed)) {
    set.seed(seed)
  }
  graph <- match.arg(graph, c("knn", "mst"))
  data.list <- c(list(X1, X2), list(...))
  if(any(!sapply(data.list, function(x) 
    inherits(x, "matrix") | inherits(x, "data.frame")))) {
    stop("All datasets must be provided as data.frames or matrices.")
  }
  p <- sapply(data.list, ncol)
  if(length(unique(p)) > 1) 
    stop("All datasets must have the same number of variables.")
  K <- length(data.list)
  n.vec <- sapply(data.list, nrow)
  for(i in seq_along(data.list)) {
    colnames(data.list[[i]]) <- paste0("X", 1:p[1])
  }
  ap.mat <- do.call(rbind, data.list)
  sample.ids <- rep(1:K, n.vec)
  N <- sum(n.vec)
  stat <- KMD::KMD(ap.mat, sample.ids, Knn = ifelse(graph == "knn", k, toupper(graph)), 
                   Kernel = kernel)
  pval <- KMD::KMD_test(ap.mat, sample.ids, Knn = ifelse(graph == "knn", k, toupper(graph)), 
                        Kernel = kernel, Permutation = (n.perm > 0), B = n.perm)
  if(n.perm <= 0) {
    est <- stat
    names(est) <- "KMD"
    stat <- pval[, "z value"]
    names(stat) <- "z"
    pval <- pval[, "p value"]
  } else {
    est <- NULL
    names(stat) <- "KMD"
  }
  mc <- as.list(match.call())
  mc <- mc[!names(mc) %in% c("n.perm", "seed", "graph", "k", "kernel")]
  dname <- paste0(sapply(mc[-1], deparse), 
                  collapse = ifelse(K > 2, ", ", " and "))
  res <- list(statistic = stat, 
              p.value = pval,
              estimate = est, 
              alternative = ifelse(K > 2, "At least one pair of distributions are unequal.", 
                                   paste0("The distributions of ", dname, " are unequal.")), 
              method = paste0("Kernel Measure of Multi-Sample Dissimilarity (KMD) ", 
                              ifelse(n.perm > 0, "Permutation ", "Approximative "), 
                              "Test"), 
              data.name = dname, graph = graph, k = k, kernel = kernel)
  class(res) <- "htest"
  return(res)
}