testEnergyDiscoB <- function(n.iter, new.fun, old.method) {
  if(requireNamespace("energy", quietly = TRUE) & requireNamespace("rmvnorm", quietly = TRUE)) {
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
      res.edist <- energy::eqdist.etest(rbind(X1, X2, X3, X4, X5, X6), 
                                        c(nrow(X1), nrow(X2), nrow(X3), nrow(X4), 
                                          nrow(X5), nrow(X6)), 
                                        R = 0, method = old.method)
      res.Energy <- new.fun(X1, X2, as.data.frame(X3), X4, X5, X6, n.perm = 0, seed = i)
      
      set.seed(i)
      res.edist.perm <- energy::eqdist.etest(rbind(X1, X2, X3, X4, X5, X6), 
                                             c(nrow(X1), nrow(X2), nrow(X3), nrow(X4), 
                                               nrow(X5), nrow(X6)), 
                                             R = 10, method = old.method)
      res.Energy.perm <- new.fun(X1, X2, as.data.frame(X3), X4, X5, X6, n.perm = 10,
                                 seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.Energy, 6)
        testthat::expect_named(res.Energy, c("call", "method", "statistic", "p.value", 
                                             "data.name",  "alternative"))
        testthat::expect_length(res.Energy.perm, 6)
        testthat::expect_named(res.Energy.perm, c("call", "method", "statistic", 
                                                  "p.value", "data.name", "alternative"))
        
        # check p values in [0,1]
        if(old.method == "original") testthat::expect_lte(res.Energy$p.value, 1)
        if(old.method == "original") testthat::expect_gte(res.Energy$p.value, 0)
        testthat::expect_lte(res.Energy.perm$p.value, 1)
        testthat::expect_gte(res.Energy.perm$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.Energy$statistic))
        testthat::expect_false(is.na(res.Energy.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.Energy, "htest")
        testthat::expect_s3_class(res.Energy.perm, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.Energy$statistic, 
                               if(old.method == "original") res.edist$statistic 
                               else res.edist, check.attributes = FALSE)
        testthat::expect_equal(res.Energy.perm$statistic, res.edist.perm$statistic, 
                               check.attributes = FALSE)
        # check test p values
        if(old.method == "original")  testthat::expect_equal(res.Energy$p.value, 
                                                             res.edist$p.value, 
                                                             check.attributes = FALSE)
        testthat::expect_equal(res.Energy.perm$p.value, res.edist.perm$p.value, 
                               check.attributes = FALSE)
      })
      
      res.Energy.1 <- new.fun(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                              as.data.frame(X3)[, 1, drop = FALSE], X4[, 1, drop = FALSE], 
                              X5[, 1, drop = FALSE], X6[, 1, drop = FALSE], n.perm = 0, seed = i)
      res.Energy.perm.1 <- new.fun(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                   as.data.frame(X3)[, 1, drop = FALSE], 
                                   X4[, 1, drop = FALSE], X5[, 1, drop = FALSE], 
                                   X6[, 1, drop = FALSE], n.perm = 10, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.Energy.1, 6)
        testthat::expect_named(res.Energy.1, c("call", "method", "statistic", "p.value", 
                                               "data.name",  "alternative"))
        testthat::expect_length(res.Energy.perm.1, 6)
        testthat::expect_named(res.Energy.perm.1, c("call", "method", "statistic", 
                                                    "p.value", "data.name", "alternative"))
        
        # check p values in [0,1]
        if(old.method == "original") testthat::expect_lte(res.Energy.1$p.value, 1)
        if(old.method == "original") testthat::expect_gte(res.Energy.1$p.value, 0)
        testthat::expect_lte(res.Energy.perm.1$p.value, 1)
        testthat::expect_gte(res.Energy.perm.1$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.Energy.1$statistic))
        testthat::expect_false(is.na(res.Energy.perm.1$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.Energy.1, "htest")
        testthat::expect_s3_class(res.Energy.perm.1, "htest")
      })
    }
  }
}

set.seed(0305)
testEnergyDiscoB(2, new.fun = DataSimilarity::Energy, old.method = "original")

set.seed(0305)
testEnergyDiscoB(2, new.fun = DataSimilarity::DISCOB, old.method = "discoB")


testDISCOF <- function(n.iter) {
  if(requireNamespace("energy", quietly = TRUE) & requireNamespace("rmvnorm", quietly = TRUE)) {
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
      res.edist <- energy::eqdist.etest(rbind(X1, X2, X3, X4, X5, X6), 
                                        c(nrow(X1), nrow(X2), nrow(X3), nrow(X4), 
                                          nrow(X5), nrow(X6)), 
                                        R = 0, method = "discoF")
      res.Energy <- DataSimilarity::DISCOF(X1, X2, as.data.frame(X3), X4, X5, X6,
                                           n.perm = 0, seed = i)
      
      set.seed(i)
      res.edist.perm <- energy::eqdist.etest(rbind(X1, X2, X3, X4, X5, X6), 
                                             c(nrow(X1), nrow(X2), nrow(X3), nrow(X4), 
                                               nrow(X5), nrow(X6)), 
                                             R = 10, method = "discoF")
      res.Energy.perm <- DataSimilarity::DISCOF(X1, X2, as.data.frame(X3), X4, X5, 
                                                X6, n.perm = 10, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.Energy, 19)
        testthat::expect_named(res.Energy, c("call", "method", "statistic", "p.value", "k", 
                                             "N", "between", "withins", "within", "total", 
                                             "Df.trt", "Df.e", "index", "factor.names", 
                                             "factor.levels", "sample.sizes", "stats", 
                                             "data.name", "alternative"))
        testthat::expect_length(res.Energy.perm, 19)
        testthat::expect_named(res.Energy.perm, c("call", "method", "statistic", "p.value", "k", 
                                                  "N", "between", "withins", "within", "total", 
                                                  "Df.trt", "Df.e", "index", "factor.names", 
                                                  "factor.levels", "sample.sizes", "stats", 
                                                  "data.name", "alternative"))
        # check p values in [0,1]
        testthat::expect_lte(res.Energy.perm$p.value, 1)
        testthat::expect_gte(res.Energy.perm$p.value, 0)
        # check approx. p value is NA
        testthat::expect_true(is.na(res.Energy$p.value))
        # statistic is not NA
        testthat::expect_false(is.na(res.Energy$statistic))
        testthat::expect_false(is.na(res.Energy.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.Energy, "disco")
        testthat::expect_s3_class(res.Energy.perm, "disco")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.Energy$statistic, res.edist$statistic, check.attributes = FALSE)
        testthat::expect_equal(res.Energy.perm$statistic, res.edist.perm$statistic, check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.Energy$p.value, res.edist$p.value, check.attributes = FALSE)
        testthat::expect_equal(res.Energy.perm$p.value, res.edist.perm$p.value, check.attributes = FALSE)
      })
      
      res.Energy.1 <- DataSimilarity::DISCOF(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE],
                                             as.data.frame(X3)[, 1, drop = FALSE], 
                                             X4[, 1, drop = FALSE], X5[, 1, drop = FALSE], 
                                             X6[, 1, drop = FALSE], n.perm = 0, seed = i)
      res.Energy.perm.1 <- DataSimilarity::DISCOF(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE],
                                                  as.data.frame(X3)[, 1, drop = FALSE], 
                                                  X4[, 1, drop = FALSE], X5[, 1, drop = FALSE], 
                                                  X6[, 1, drop = FALSE], n.perm = 10, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.Energy.1, 19)
        testthat::expect_named(res.Energy.1, c("call", "method", "statistic", "p.value", "k", 
                                               "N", "between", "withins", "within", "total", 
                                               "Df.trt", "Df.e", "index", "factor.names", 
                                               "factor.levels", "sample.sizes", "stats", 
                                               "data.name", "alternative"))
        testthat::expect_length(res.Energy.perm.1, 19)
        testthat::expect_named(res.Energy.perm.1, c("call", "method", "statistic", "p.value", "k", 
                                                    "N", "between", "withins", "within", "total", 
                                                    "Df.trt", "Df.e", "index", "factor.names", 
                                                    "factor.levels", "sample.sizes", "stats", 
                                                    "data.name", "alternative"))
        # check p values in [0,1]
        testthat::expect_lte(res.Energy.perm.1$p.value, 1)
        testthat::expect_gte(res.Energy.perm.1$p.value, 0)
        # check approx. p value is NA
        testthat::expect_true(is.na(res.Energy.1$p.value))
        # statistic is not NA
        testthat::expect_false(is.na(res.Energy.1$statistic))
        testthat::expect_false(is.na(res.Energy.perm.1$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.Energy.1, "disco")
        testthat::expect_s3_class(res.Energy.perm.1, "disco")
      })
    }
  }
}

set.seed(0305)
testDISCOF(2)
