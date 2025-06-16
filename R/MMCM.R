################################################################################
##                          Mukherjee et al. (2022)                           ##
##                                                                            ##
################################################################################
MMCM <- function(X1, X2, ..., dist.fun = stats::dist, dist.args = NULL, seed = NULL) {
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
  
  a_N <- nbmatch(data.list, n.vec, dist.fun, dist.args)
  MMCM_stat <- MMCMStatistic(a_N, n.vec)
  
  stat <- MMCM_stat$S_KN
  names(stat) <- "chisq"
  param <- MMCM_stat$df
  names(param) <- "df"
  
  pval <- 1 - stats::pchisq(stat, param)
  
  mc <- as.list(match.call())
  mc <- mc[!names(mc) %in% c("dist.fun", "dist.args", "seed")]
  dname <- paste0(sapply(mc[-1], deparse), 
                  collapse = ifelse(length(data.list) > 2, ", ", " and "))
  
  res <- list(statistic = stat, parameter = param,
              p.value = pval, estimate = NULL,
              alternative = ifelse(K > 2, "At least one pair of distributions are unequal.", 
                                   paste0("The distributions of ", dname, " are unequal.")), 
              method = "Approximative MMCM test",  
              data.name = dname)
  class(res) <- "htest"
  return(res)
}
