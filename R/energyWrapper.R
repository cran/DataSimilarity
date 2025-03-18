################################################################################
##                                HELPER FUNCTIONS                            ##
##                                                                            ##
################################################################################
energyWrapper <- function(data.list, n.perm = 0, dname, seed = 42) {
  set.seed(seed)
  if(any(!sapply(data.list, function(x) inherits(x, "matrix") | inherits(x, "data.frame")))) {
    stop("All datasets must be provided as data.frames or matrices.")
  }
  p <- sapply(data.list, ncol)
  if(length(unique(p)) > 1) 
    stop("All datasets must have the same number of variables")
  n.vec <- sapply(data.list, nrow)
  for(i in seq_along(data.list)) {
    colnames(data.list[[i]]) <- paste0("X", 1:p[1])
  }
  ap.mat <- do.call(rbind, data.list)
  res <- energy::eqdist.etest(ap.mat, n.vec, method = "original", R = n.perm)
  res$data.name <- paste0(paste0(dname, 
                                 collapse = ifelse(length(data.list) > 2, ", ", " and ")),
                          "\n", res$data.name)
  res$estimate <- NULL
  res$alternative <- ifelse(length(data.list) > 2, "At least one pair of distributions are unequal.", 
                            paste0("The distributions of ", res$data.name, " are unequal."))
  return(res)
}

discoWrapper <- function(data.list, n.perm = 0, method, dname, alpha, seed = 42) {
  set.seed(seed)
  if(any(!sapply(data.list, function(x) inherits(x, "matrix") | inherits(x, "data.frame")))) {
    stop("All datasets must be provided as data.frames or matrices.")
  }
  p <- sapply(data.list, ncol)
  if(length(unique(p)) > 1) 
    stop("All datasets must have the same number of variables")
  n.vec <- sapply(data.list, nrow)
  for(i in seq_along(data.list)) {
    colnames(data.list[[i]]) <- paste0("X", 1:p[1])
  }
  ap.mat <- do.call(rbind, data.list)
  res <- energy::disco(ap.mat, rep(1:length(data.list), n.vec), method = method,
                       R = n.perm, index = alpha)
  if(method == "discoB") {
    if(n.perm <= 0){
      names(res) <- "DISCO between statistic"
      res <- list(call = match.call(), method = "DISCO (Between-sample)",
                  statistic = res, p.value = NULL, 
                  data.name = paste0("sample sizes ", 
                                     paste0(sapply(data.list, nrow), 
                                            collapse = " "), 
                                     ", replicates ", n.perm))
      class(res) <- "htest"
    } else {
      res$data.name <- paste0("sample sizes ", paste0(sapply(data.list, nrow), collapse = " "), 
                              ", replicates ", n.perm)
    }
  }
  res$data.name <- paste0(paste0(dname, 
                                 collapse = ifelse(length(data.list) > 2, ", ", " and ")),
                          "\n", res$data.name)
  res$estimate <- NULL
  res$alternative <- ifelse(length(data.list) > 2, "At least one pair of distributions are unequal.", 
                            paste0("The distributions of ", res$data.name, " are unequal."))
  return(res)
}
