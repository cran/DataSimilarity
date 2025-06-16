################################################################################
##                              BALL DIVERGENCE                               ##
##                                                                            ##
################################################################################
BallDivergence <- function (X1, X2,..., n.perm = 0, seed = NULL, num.threads = 0, 
                            kbd.type = "sum", 
                            weight = c("constant", "variance"), 
                            args.bd.test = NULL) {
  if(!requireNamespace("Ball", quietly = TRUE)) {
    stop("Package \"Ball\" required for using method BallDivergence().")
  }
  if(!is.null(seed)) {
    set.seed(seed)
  }
  genv <- globalenv()
  # Make sure to leave '.Random.seed' as-is on exit
  old_seed <- genv$.Random.seed
  on.exit(suspendInterrupts({
    if (is.null(old_seed)) {
      rm(".Random.seed", envir = genv, inherits = FALSE)
    } else {
      assign(".Random.seed", value = old_seed, envir = genv, inherits = FALSE)
    }
  }))
  data.list <- c(list(X1, X2), list(...))
  if(any(!sapply(data.list, function(x) inherits(x, "matrix") | inherits(x, "data.frame")))) {
    stop("All datasets must be provided as data.frames or matrices.")
  }
  p <- sapply(data.list, ncol)
  if(length(unique(p)) > 1) { 
    stop("All datasets must have the same number of variables")
  }
  n.vec <- sapply(data.list, nrow)
  K <- length(data.list)
  data.list <- lapply(data.list, function(X) {
    colnames(X) <- paste0("X", 1:p[1])
    X
  })
  res <- do.call(Ball::bd.test, c(list(x = data.list, num.permutations = max(n.perm, 1),
                                       method = ifelse(n.perm > 0, "permutation", "limit"), 
                                       distance = FALSE, size = NULL, seed = seed, 
                                       num.threads = num.threads, kbd.type = kbd.type, 
                                       weight = weight), args.bd.test))#
  if(K > 2 & n.perm <= 0) {
    mc <- as.list(match.call())
    mc <- mc[!names(mc) %in% c("n.perm", "seed", "num.threads", "kbd.type", 
                               "weight", "args.bd.test")]
    res <- list(statistic = res[paste0("kbd.", kbd.type, ".", weight[1])],
                p.value = NULL, replicates = n.perm, size = n.vec, 
                alternative = NA, method = paste0(K, "-sample Ball Divergence Test (", 
                                                  ifelse(n.perm > 0, "Permutation", "Limit"), 
                                                  " Distribution)"), 
                data.name = paste0(paste0(sapply(mc[-1], deparse), collapse = ", "), 
                                   "\nnumber of observations = ", sum(n.vec), 
                                   ", group sizes: ", paste0(n.vec, collapse = " "), 
                                   "\nreplicates = ", n.perm, ", weight: ", 
                                   match.arg(weight)))
  }
  
  names(res)[3] <- "n.perm"
  res <- res[names(res) != "complete.info"]
  res$alternative <- ifelse(K > 2, "At least one pair of distributions are unequal.", 
                            paste0("The distributions of ", res$data.name, " are unequal."))
  class(res) <- "htest"
  return(res)
}
