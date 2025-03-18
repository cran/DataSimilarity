################################################################################
##                          Song and Chen (2022)                              ##
##                                                                            ##
################################################################################
gTestsMulti <- function(X1, X2, ..., n.perm = 0, dist.fun = stats::dist, graph.fun = MST, 
                        dist.args = NULL, graph.args = NULL, seed = 42) {
  if(!requireNamespace("gTestsMulti", quietly = TRUE)) {
    stop("Package \"gTestsMulti\" required for using method gTestsMulti().")
  }
  set.seed(seed)
  data.list <- c(list(X1, X2), list(...))
  if(any(!sapply(data.list, function(x) inherits(x, "matrix") | inherits(x, "data.frame")))) {
    stop("All datasets must be provided as data.frames or matrices.")
  }
  p <- sapply(data.list, ncol)
  if(length(unique(p)) > 1) 
    stop("All datasets must have the same number of variables")
  for(i in seq_along(data.list)) {
    colnames(data.list[[i]]) <- paste0("X", 1:p[1])
  }
  K <- length(data.list)
  ap.mat <- do.call(rbind, data.list)
  E <- getEdgeMat(ap.mat, dist.fun, graph.fun, dist.args, graph.args)
  res <- gTestsMulti::gtestsmulti(E = E, data_list = data.list, perm = n.perm)
  stat <- unlist(res$teststat)
  pval <- unlist(res$pval)[grepl(ifelse(n.perm > 0, "perm", "appr"), names(res$pval))]
  mc <- as.list(match.call())
  mc <- mc[!names(mc) %in% c("n.perm", "dist.fun", "graph.fun", "dist.args", 
                             "graph.args", "seed")]
  dname <- paste0(paste0(sapply(mc[-1], deparse), 
                         collapse = ifelse(K > 2, ", ", " and ")))
  
  res <- list(statistic = stat, 
              p.value = pval, 
              estimate = NULL,
              alternative = ifelse(K > 2, "At least one pair of distributions are unequal.", 
                                   paste0("The distributions of ", dname, " are unequal.")), 
              method = paste0(ifelse(n.perm > 0, "Permutation ", "Approximative "), 
                              "multi-sample test of Song and Chen (2022)"), 
              data.name = dname)
  return(res)
}