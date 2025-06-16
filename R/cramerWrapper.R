################################################################################
##                                HELPER FUNCTIONS                            ##
##                                                                            ##
################################################################################
cramerWrapper <- function(X1, X2, dname, n.perm = 0, just.statistic = (n.perm <= 0), 
                          kernel = "phiCramer", sim = "ordinary", maxM = 2^14,
                          K = 160, seed = NULL) {
  if(!requireNamespace("cramer", quietly = TRUE)) {
    stop("Package \"cramer\" required for using methods Cramer(), BF() and Bahr().")
  }
  if(!is.null(seed)) {
    set.seed(seed)
  }
  stopifnot(n.perm > 0 || just.statistic)
  X1 <- as.matrix(X1)
  X2 <- as.matrix(X2)
  res <- cramer::cramer.test(X1, X2, replicates = n.perm, just.statistic = just.statistic,  
                             kernel = kernel, sim = sim, maxM = maxM, K = K)
  names(res$statistic) <- "Tm,n"
  class(res) <- "list"
  res$data.name <- paste0(dname, collapse = " and ")
  res$alternative <- paste0("The distributions of ", res$data.name, " are unequal.")
  if(just.statistic || n.perm <= 0) {
    res$p.value <- NULL
    res$sim <- NULL
    res$hypdist <- NULL 
    res$ev <- NULL
  }
  res$conf.level <- NULL 
  res$crit.value <- NULL 
  res$result <- NULL 
  names(res)[grepl("replicates", names(res))] <- "n.perm"
  class(res) <- "htest"
  return(res)
}
