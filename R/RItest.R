################################################################################
##                                RAND INDEX TEST                             ##
##                                                                            ##
################################################################################
RItest <- function(X1, X2, ..., n.clust, randomization = TRUE, 
                   version = "original",
                   mult.test = "Holm",
                   kmax = 2 * n.clust, s.psi = 1, s.h = 1, lb = 1, n.perm =   1 / alpha, 
                   alpha = 0.05, seed = NULL) {
  if(!requireNamespace("HDLSSkST", quietly = TRUE)) {
    stop("Package \"HDLSSkST\" required for using method RItest().")
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
  n.vec <- sapply(data.list, nrow)
  for(i in seq_along(data.list)) {
    colnames(data.list[[i]]) <- paste0("X", 1:p[1])
  }
  ap.mat <- do.call(rbind, data.list)
  K <- length(n.vec)
  N <- sum(n.vec)
  if(missing(n.clust)) n.clust <- K
  mc <- as.list(match.call())
  mc <- mc[!names(mc) %in% c("n.clust", "randomization", "seed", "version", 
                             "mult.test", "kmax", "s.psi", "s.h", "lb", 
                             "n.perm", "alpha")]
  dname <- paste0(sapply(mc[-1], deparse), 
                  collapse = ifelse(length(data.list) > 2, ", ", " and "))
  
  if(version %in% c("original", "modified")) {
    tmp.res <- HDLSSkST::RItest(M = ap.mat, labels = rep(1:K, times = n.vec), 
                                sizes = n.vec, n_clust = n.clust, 
                                randomization = randomization, 
                                clust_alg = if(version == "original") "knwClustNo" 
                                else "estClustNo", 
                                kmax = kmax, s_psi = s.psi, s_h = s.h, lb = lb, 
                                n_sts = n.perm, alpha = alpha)
    stat <- tmp.res$ObservedRI
    names(stat) <- if(version == "original") "RI" else "MRI"
    res <- list(statistic = stat, 
                p.value = tmp.res$estPvalue, estimate = NULL,
                alternative = ifelse(K > 2, "At least one pair of distributions are unequal.", 
                                     paste0("The distributions of ", dname, " are unequal.")), 
                method = "k-sample RI test",  
                data.name = dname, 
                est.cluster.label = tmp.res$estClustLabel, 
                observed.cont.table = tmp.res$obsCtyTab, 
                crit.value = tmp.res$RIutoff, 
                random.gamma = tmp.res$randomGamma, 
                decision = tmp.res$decisionRI, 
                est.cluster.no = tmp.res$estClustNo)
    class(res) <- "htest"
  } else if(version == "multiscale") {
    tmp.res <- HDLSSkST::MTRItest(M = ap.mat, labels = rep(1:K, times = n.vec), 
                                  sizes = n.vec, 
                                  k_max = kmax, multTest = mult.test,
                                  s_psi = s.psi, s_h = s.h, lb = lb, 
                                  n_sts = n.perm, alpha = alpha)
    
    res <- list(statistic = tmp.res$RIvec, 
                p.value = tmp.res$Pvalues, estimate = NULL,
                alternative = ifelse(K > 2, "At least one pair of distributions are unequal.", 
                                     paste0("The distributions of ", dname, " are unequal.")), 
                method = "k-sample multiscale RI test",  
                data.name = dname,  
                observed.cont.table = tmp.res$contTabs, 
                decision = tmp.res$decisionMTRI, 
                decision.per.k = tmp.res$mulTestdec)
  } else if(version %in% c("aggregated-est", "aggregated-knw")){
    tmp.res <- HDLSSkST::ARItest(M = ap.mat, sizes = n.vec, 
                                 randomization = randomization, 
                                 clust_alg = if(version == "aggregated-knw") "knwClustNo" 
                                 else "estClustNo", 
                                 multTest = mult.test,
                                 kmax = kmax, s_psi = s.psi, s_h = s.h, lb = lb, 
                                 n_sts = n.perm, alpha = alpha)
    stat <- tmp.res$ARIStat
    names(stat) <- "ARI"
    res <- list(statistic = stat, 
                p.value = tmp.res$estPvalue, estimate = NULL,
                alternative = ifelse(K > 2, "At least one pair of distributions are unequal.", 
                                     paste0("The distributions of ", dname, " are unequal.")), 
                method = "k-sample aggregated RI test",  
                data.name = dname, 
                crit.value = tmp.res$Cutoff, 
                random.gamma = tmp.res$randomGamma, 
                decision = tmp.res$decisionARI, 
                est.cluster.no = tmp.res$multipleTest)
    class(res) <- "htest"
  } else {
    stop(paste0("\"version\" must be in c(\"original\", \"modified\", \"multiscale\", ", 
                "\"aggregated-est\", \"aggregated-knw\"."))
  }
  
 
  return(res)
}
