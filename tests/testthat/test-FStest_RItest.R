testFS <- function(n.iter, new.fun, old.fun, new.opts = NULL, old.opts = NULL) {
  if(requireNamespace("HDLSSkST", quietly = TRUE) & requireNamespace("rmvnorm", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10),
                             mean = runif(10, -2, 2))
      X2 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      X3 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -2, 2))
      X4 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      X5 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -2, 2))
      X6 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      set.seed(i)
      res.old.perm <- do.call(old.fun, c(list(M = rbind(X1, X2, X3, X4, X5, X6), 
                                              labels = rep(1:6, each = 100), 
                                              sizes = rep(100, 6), 
                                              n_sts = 20), old.opts))
      res.new.perm <- do.call(new.fun, c(list(X1, X2, as.data.frame(X3), X4, X5, X6,
                                              n.perm = 20, seed = i), new.opts))
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.new.perm, 12)
        testthat::expect_named(res.new.perm, c("statistic",  "p.value", "estimate",
                                               "alternative", "method",  "data.name",
                                               "est.cluster.label", "observed.cont.table",
                                               "crit.value", "random.gamma",       
                                               "decision", "est.cluster.no"))
        # check p values in [0,1]
        testthat::expect_lte(res.new.perm$p.value, 1)
        testthat::expect_gte(res.new.perm$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.new.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.new.perm, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.new.perm$statistic, res.old.perm$ObservedProb,
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.new.perm$p.value, res.old.perm$estPvalue,
                               check.attributes = FALSE)
      })
      
      
      res.new.perm.1 <- do.call(new.fun, c(list(X1[, 1, drop = FALSE], 
                                                X2[, 1, drop = FALSE],
                                                as.data.frame(X3)[, 1, drop = FALSE],
                                                X4[, 1, drop = FALSE], 
                                                X5[, 1, drop = FALSE], 
                                                X6[, 1, drop = FALSE],
                                                n.perm = 20, seed = i), new.opts))
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.new.perm.1, 12)
        testthat::expect_named(res.new.perm.1, c("statistic",  "p.value", "estimate",
                                                 "alternative", "method",  "data.name",
                                                 "est.cluster.label", "observed.cont.table",
                                                 "crit.value", "random.gamma",       
                                                 "decision", "est.cluster.no"))
        # check p values in [0,1]
        testthat::expect_lte(res.new.perm.1$p.value, 1)
        testthat::expect_gte(res.new.perm.1$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.new.perm.1$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.new.perm.1, "htest")
      })
    }
  }
}

testAFS <- function(n.iter, new.fun, old.fun, new.opts = NULL, old.opts = NULL) {
  if(requireNamespace("HDLSSkST", quietly = TRUE) & requireNamespace("rmvnorm", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -2, 2))
      X2 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      X3 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -2, 2))
      X4 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      X5 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -2, 2))
      X6 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      set.seed(i)
      res.old.perm <- do.call(old.fun, c(list(M = rbind(X1, X2, X3, X4, X5, X6),  
                                              sizes = rep(100, 6), 
                                              n_sts = 20), old.opts))
      res.new.perm <- do.call(new.fun, c(list(X1, X2, as.data.frame(X3), X4, X5, X6, 
                                              n.perm = 20, seed = i), new.opts))
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.new.perm, 10)
        testthat::expect_named(res.new.perm, c('statistic', 'p.value', 'estimate', 
                                               'alternative', 'method', 'data.name', 
                                               'crit.value', 'random.gamma', 
                                               'decision', 'est.cluster.no'))
        # statistic is not NA
        testthat::expect_false(is.na(res.new.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.new.perm, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.new.perm$statistic, res.old.perm$AFSStat,
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.new.perm$est.cluster.no, res.old.perm$multipleTest,
                               check.attributes = FALSE)
      })
      
      # not working for p = 1
    }
  }
}

testMSFS <- function(n.iter, new.fun, old.fun, new.opts = NULL, old.opts = NULL) {
  if(requireNamespace("HDLSSkST", quietly = TRUE) & requireNamespace("rmvnorm", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -2, 2))
      X2 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      X3 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -2, 2))
      X4 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2),
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      X5 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10),
                             mean = runif(10, -2, 2))
      X6 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      set.seed(i)
      res.old.perm <- do.call(old.fun, c(list(M = rbind(X1, X2, X3, X4, X5, X6),  
                                              labels = rep(1:6, each = 100), 
                                              sizes = rep(100, 6),  
                                              n_sts = 20), old.opts))
      res.new.perm <- do.call(new.fun, c(list(X1, X2, as.data.frame(X3), X4, X5, X6, 
                                              n.perm = 20, seed = i), new.opts))
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.new.perm, 9)
        testthat::expect_named(res.new.perm, c('statistic', 'p.value', 'estimate', 
                                               'alternative', 'method', 'data.name', 
                                               'observed.cont.table', 'decision', 
                                               'decision.per.k'))
        # statistic is not NA
        testthat::expect_false(any(is.na(res.new.perm$statistic)))
        testthat::expect_false(any(is.na(res.new.perm$p.value)))
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.new.perm$statistic, res.old.perm$fpmfvec,
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.new.perm$p.value, res.old.perm$Pvalues,
                               check.attributes = FALSE)
      })
      
      res.new.perm.1 <- do.call(new.fun, c(list(X1[, 1, drop = FALSE], 
                                                X2[, 1, drop = FALSE], 
                                                as.data.frame(X3)[, 1, drop = FALSE], 
                                                X4[, 1, drop = FALSE], 
                                                X5[, 1, drop = FALSE], 
                                                X6[, 1, drop = FALSE], 
                                                n.perm = 20, seed = i), new.opts))
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.new.perm.1, 9)
        testthat::expect_named(res.new.perm.1, c('statistic', 'p.value', 'estimate', 
                                                 'alternative', 'method', 'data.name', 
                                                 'observed.cont.table', 'decision', 
                                                 'decision.per.k'))
        # statistic is not NA
        testthat::expect_false(any(is.na(res.new.perm.1$statistic)))
        testthat::expect_false(any(is.na(res.new.perm.1$p.value)))
      })
    }
  }
}

# original
set.seed(0305)
testFS(1, DataSimilarity::FStest, HDLSSkST::FStest, old.opts = list(n_clust = 6))

# modified
set.seed(0305)
testFS(1, DataSimilarity::FStest, HDLSSkST::FStest, new.opts = list(version = "modified"), 
       old.opts = list(clust_alg = "estClustNo", n_clust = 6))

# multiscale
set.seed(0305)
testMSFS(1, DataSimilarity::FStest, HDLSSkST::MTFStest, new.opts = list(version = "multiscale"), 
         old.opts = list(k_max = 12))

# aggregated
set.seed(0305)
testAFS(1, DataSimilarity::FStest, HDLSSkST::AFStest, new.opts = list(version = "aggregated-knw"))


set.seed(0305)
testAFS(1, DataSimilarity::FStest, HDLSSkST::AFStest, new.opts = list(version = "aggregated-est"), 
        old.opts = list(clust_alg = "estClustNo"))


testRI <- function(n.iter, new.fun, old.fun, new.opts = NULL, old.opts = NULL) {
  if(requireNamespace("HDLSSkST", quietly = TRUE) & requireNamespace("rmvnorm", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), mean = runif(10, -2, 2))
      X2 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      X3 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), mean = runif(10, -2, 2))
      X4 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      X5 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), mean = runif(10, -2, 2))
      X6 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      set.seed(i)
      res.old.perm <- do.call(old.fun, c(list(M = rbind(X1, X2, X3, X4, X5, X6), 
                                              labels = rep(1:6, each = 100), 
                                              sizes = rep(100, 6), 
                                              n_sts = 20), old.opts))
      res.new.perm <- do.call(new.fun, c(list(X1, X2, X3, X4, X5, X6, n.perm = 20, 
                                              seed = i), new.opts))
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.new.perm, 12)
        testthat::expect_named(res.new.perm, c("statistic",  "p.value", "estimate",
                                               "alternative", "method",  "data.name",
                                               "est.cluster.label", "observed.cont.table",
                                               "crit.value", "random.gamma",       
                                               "decision", "est.cluster.no"))
        # check p values in [0,1]
        testthat::expect_lte(res.new.perm$p.value, 1)
        testthat::expect_gte(res.new.perm$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.new.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.new.perm, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.new.perm$statistic, res.old.perm$ObservedRI,
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.new.perm$p.value, res.old.perm$estPvalue,
                               check.attributes = FALSE)
      })
    }
  }
}

testARI <- function(n.iter, new.fun, old.fun, new.opts = NULL, old.opts = NULL) {
  if(requireNamespace("HDLSSkST", quietly = TRUE) & requireNamespace("rmvnorm", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), mean = runif(10, -2, 2))
      X2 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      X3 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), mean = runif(10, -2, 2))
      X4 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      X5 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), mean = runif(10, -2, 2))
      X6 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      set.seed(i)
      res.old.perm <- do.call(old.fun, c(list(M = rbind(X1, X2, X3, X4, X5, X6),  
                                              sizes = rep(100, 6), 
                                              n_sts = 20), old.opts))
      res.new.perm <- do.call(new.fun, c(list(X1, X2, X3, X4, X5, X6, n.perm = 20, 
                                              seed = i), new.opts))
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.new.perm, 10)
        testthat::expect_named(res.new.perm, c('statistic', 'p.value', 'estimate', 
                                               'alternative', 'method', 'data.name', 
                                               'crit.value', 'random.gamma', 
                                               'decision', 'est.cluster.no'))
        # statistic is not NA
        testthat::expect_false(is.na(res.new.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.new.perm, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.new.perm$statistic, res.old.perm$ARIStat,
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.new.perm$est.cluster.no, res.old.perm$multipleTest,
                               check.attributes = FALSE)
      })
    }
  }
}

# original
set.seed(0305)
testRI(1, DataSimilarity::RItest, HDLSSkST::RItest, old.opts = list(n_clust = 6))

# modified
set.seed(0305)
testRI(1, DataSimilarity::RItest, HDLSSkST::RItest, new.opts = list(version = "modified"), 
       old.opts = list(clust_alg = "estClustNo", n_clust = 6))

# multiscale

# aggregated
set.seed(0305)
testARI(1, DataSimilarity::RItest, HDLSSkST::ARItest, new.opts = list(version = "aggregated-knw"))


set.seed(0305)
testARI(1, DataSimilarity::RItest, HDLSSkST::ARItest, new.opts = list(version = "aggregated-est"), 
        old.opts = list(clust_alg = "estClustNo"))
