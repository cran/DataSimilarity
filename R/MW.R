################################################################################
##                         Mukhopadhyay & Wang (2020)                         ##
##                                                                            ##
################################################################################
MW <- function(X1, X2, ..., sum.all = FALSE, m.max = 4, components = NULL, 
               alpha = 0.05, c.poly = 0.5, clust.alg = 'kmeans', n.perm = 0,
               combine.criterion = 'kernel', multiple.comparison = TRUE,
               compress.algorithm = FALSE, nbasis = 8, seed = NULL) {
  if(!requireNamespace("LPKsample", quietly = TRUE)) {
    stop("Package \"LPKsample\" required for using method MW().")
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
    stop("All datasets must have the same number of variables.")
  n.vec <- sapply(data.list, nrow)
  K <- length(data.list)
  for(i in seq_along(data.list)) {
    colnames(data.list[[i]]) <- paste0("X", 1:p[1])
  }
  ap.mat <- do.call(rbind, data.list)
  sample.ids <- rep(1:K, n.vec)
  res <- LPKsample::GLP(as.matrix(ap.mat), sample.ids, m.max = m.max, 
                        components = components, 
                        alpha = alpha, c.poly = c.poly, clust.alg = clust.alg, 
                        perm = n.perm, combine.criterion = combine.criterion, 
                        multiple.comparison = multiple.comparison, 
                        compress.algorithm = compress.algorithm, nbasis = nbasis, 
                        return.LPT = FALSE, return.clust = FALSE)
  if(combine.criterion == "pvalue") {
    stat <- NA
  }
  if(sum.all) {
    stat <- sum(res$table[, "comp.GLP"])
  } else {
    stat <- res$GLP
  }
  
  names(stat) <- "chisq"
  mc <- as.list(match.call())
  mc <- mc[!names(mc) %in% c("sum.all", "m.max", "components", "alpha", 
                             "c.poly", "clust.alg", "n.perm", "combine.criterion", 
                             "multiple.comparison", "compress.algorithm", "nbasis", 
                             "seed")]
  dname <- paste0(paste0(sapply(mc[-1], deparse), 
                         collapse = ifelse(K > 2, ", ", " and ")))
  df <- (K - 1)^2
  names(df) <- "df"
  
  res <- list(statistic = stat, parameter = df,
              p.value = res$pval, 
              estimate = NULL,
              alternative = ifelse(K > 2, "At least one pair of distributions are unequal.", 
                                   paste0("The distributions of ", dname, " are unequal.")),
              method = "Approximative GLP test",  
              data.name = dname, 
              components = res$table)
  class(res) <- "htest"
  return(res)
}
