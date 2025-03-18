################################################################################
##                  HELPER FUNCTIONS FOR gTests BASED FUNCTIONS               ##
##                                                                            ##
################################################################################

MST <- function(dists, K = 1) {
  if(!requireNamespace("ade4", quietly = TRUE)) {
    stop("Package \"ade4\" required for using method MST().")
  }
  ade4::mstree(dists, K)
}

getEdgeMat <- function(pooled.mat, dist.fun, graph.fun, dist.args, graph.args) {
  dists <- do.call(dist.fun, c(list(pooled.mat), dist.args))
  E <- do.call(graph.fun, c(list(dists), graph.args))
  return(E)
}

prettyResultgTest <- function(res, n.perm, type, dname) {
  stat <- sapply(res, "[[", "test.statistic")
  names(stat) <- switch(type, "generalized" = "chi^2", 
                        "maxtype" = "M", "z", 
                        "all" = c("FR", "CF", "CCS", "ZC"))
  if(type == "generalized") {
    param <- 2
    names(param) <- "df"
  } else {
    param = NULL
  }
  res <- list(statistic = stat, 
              p.value = ifelse(rep(n.perm > 0, length(stat)), 
                               sapply(res, "[[", "pval.perm"), 
                               sapply(res, "[[", "pval.approx")), 
              estimate = NULL,
              alternative = paste0("The distributions of ", paste0(dname, collapse = " and "), 
                                   " are unequal."), 
              method = paste0(ifelse(n.perm > 0, "Permutation ", "Approximative "),
                              type, " edge-count test"), 
              data.name = paste0(dname, collapse = " and "))
  if(length(stat) == 1) class(res) <- "htest"
  return(res)
}

getEdgeMatCat <- function(X1, X2, dist.fun, graph.type, K) {
  # paste classes per observation together to easily access all distinct observations
  colnames(X1) <- colnames(X2) <- paste0("X", 1:ncol(X1))
  new.pooled.data <- apply(rbind(X1, X2), 1, paste0, collapse = "_")
  # count how often each combination is observed
  counts <- table(new.pooled.data, c(rep(1, nrow(X1)), rep(2, nrow(X2))))
  # calculate matrix of pairweise distances
  dists <- outer(rownames(counts), rownames(counts),
                 function(x, y) apply(cbind(x, y), 1, 
                                      function(z) dist.fun(strsplit(as.character(z[1]), "_")[[1]], 
                                                           strsplit(as.character(z[2]), "_")[[1]])))
  rownames(dists) <- colnames(dists) <- rownames(counts)
  E <- gTests::getGraph(counts, dists, K = K, graph.type = graph.type)
  return(list(E = E, counts = counts))
}

prettyResultgTestCat <- function(res, agg.type, n.perm, type, dname) {
  if(type == "all") {
    stat.a <- sapply(res, "[[", "test.statistic_a")
    stat.u <- sapply(res, "[[", "test.statistic_u")
    names(stat.a) <- paste0(c("FR_cat", "CF_cat", "CCS_cat", "ZC_cat"), ".a")
    names(stat.u) <- paste0(c("FR_cat", "CF_cat", "CCS_cat", "ZC_cat"), ".u")
    stat <- c(stat.a, stat.u)
    pval.a <- sapply(res, function(x) x[[ifelse(n.perm > 0, "pval.perm_a", "pval.approx_a")]])
    pval.u <- sapply(res, function(x) x[[ifelse(n.perm > 0, "pval.perm_u", "pval.approx_u")]])
    pval <- c(pval.a, pval.u)
    names(pval) <- names(stat)
  } else {
    res <- unlist(res)
    res <- res[grepl(paste0("test.statistic_", agg.type), names(res)) | 
                 grepl(paste0("pval.*_", agg.type), names(res))]
    if(n.perm == 0) {
      res <- c(res, NA)
    }
    names(res) <- c("statistic", "pvalue.approx", "pvalue.perm")
    stat <- res["statistic"]
    names(stat) <- switch(type, "generalized" = "chi^2", 
                          "maxtype" = "M", "z")
    pval <- ifelse(n.perm > 0, res["pvalue.perm"], res["pvalue.approx"])
  }
  if(type == "generalized" & n.perm <= 0) {
    param <- 2
    names(param) <- "df"
  } else {
    param = NULL
  }
  res <- list(statistic = stat, parameter = param,
              p.value = pval, 
              estimate = NULL,
              alternative = paste0("The distributions of ", paste0(dname, collapse = " and "), 
                                   " are unequal."), 
              method = paste0(ifelse(n.perm > 0, "Permutation ", "Approximative "), 
                              type, " edge-count test for categorical data using ", 
                              if(agg.type == "a") "averaging" else "union"), 
              data.name = paste0(dname, collapse = " and "))
  if(type != "all") class(res) <- "htest"
  return(res)
}

gTestWrapper <- function(X1, X2, dist.fun = stats::dist, graph.fun = MST, 
                         n.perm = 0, dist.args = NULL, graph.args = NULL, type,
                         seed = 42, ...) {
  if(!requireNamespace("gTests", quietly = TRUE)) {
    stop("Package \"gTests\" required for using method CCS(), CF(), FR(), and ZC().")
  }
  set.seed(seed)
  stopifnot(n.perm >= 0)
  if(!(inherits(X1, "matrix") | inherits(X1, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(!(inherits(X2, "matrix") | inherits(X2, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(ncol(X1) != ncol(X2)) {
    stop("All datasets must have the same number of variables.")
  }
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  colnames(X1) <- colnames(X2) <- paste0("X", 1:ncol(X1))
  E <- getEdgeMat(rbind(X1, X2), dist.fun, graph.fun, dist.args, graph.args)
  res <- gTests::g.tests(E, sample1ID = 1:nrow(X1), perm = n.perm,
                         sample2ID = (nrow(X1)+1):(nrow(X1)+nrow(X2)), 
                         test.type = type, ...)
  res <- prettyResultgTest(res, n.perm, type, dname)
  return(res)
}

gTestWrapperCat <- function(X1, X2, dist.fun, agg.type, graph.type = "mstree", 
                            K = 1, n.perm = 0, type, seed = 42, ...) {
  if(!requireNamespace("gTests", quietly = TRUE)) {
    stop("Package \"gTests\" required for using method CCS(), CF(), FR(), and ZC().")
  }
  set.seed(seed)
  stopifnot(K >= 1, n.perm >= 0, agg.type %in% c("a", "u"))
  if(!(inherits(X1, "matrix") | inherits(X1, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(!(inherits(X2, "matrix") | inherits(X2, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(ncol(X1) != ncol(X2)) {
    stop("All datasets must have the same number of variables.")
  }
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  E <- getEdgeMatCat(X1, X2, dist.fun, graph.type, K)
  res <- gTests::g.tests_discrete(E$E, E$counts, test.type = type, perm = n.perm, ...)
  res <- prettyResultgTestCat(res, agg.type, n.perm, type, dname)
  return(res)
}