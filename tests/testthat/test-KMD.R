testKMD <- function(n.iter) {
  if(requireNamespace("KMD", quietly = TRUE) & requireNamespace("rmvnorm", quietly = TRUE)) {
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
      res.KMD_test.perm <- KMD::KMD_test(rbind(X1, X2, X3, X4, X5, X6),  
                                         c(rep(1, nrow(X1)), rep(2, nrow(X2)), 
                                           rep(3, nrow(X3)), rep(4, nrow(X4)), 
                                           rep(5, nrow(X5)), rep(6, nrow(X6))),
                                         Permutation = TRUE, B = 10)
      set.seed(i)
      res.KMD_test <- KMD::KMD_test(rbind(X1, X2, X3, X4, X5, X6),  
                                    c(rep(1, nrow(X1)), rep(2, nrow(X2)),
                                      rep(3, nrow(X3)), rep(4, nrow(X4)),
                                      rep(5, nrow(X5)), rep(6, nrow(X6))), 
                                    Permutation = FALSE)
      set.seed(i)
      res.KMD.test.stat <- KMD::KMD(rbind(X1, X2, X3, X4, X5, X6),  
                                    c(rep(1, nrow(X1)), rep(2, nrow(X2)), 
                                      rep(3, nrow(X3)), rep(4, nrow(X4)), 
                                      rep(5, nrow(X5)), rep(6, nrow(X6))))
      
      res.KMD <- DataSimilarity::KMD(X1, X2, as.data.frame(X3), X4, X5, X6,
                                     n.perm = 0, k = 1, seed = i)
      res.KMD.perm <- DataSimilarity::KMD(X1, X2, as.data.frame(X3), X4, X5, X6, 
                                          n.perm = 10, k = 1, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.KMD, 9)
        testthat::expect_named(res.KMD, c("statistic", "p.value", "estimate",
                                          "alternative", "method", "data.name", 
                                          "graph", "k", "kernel"))
        testthat::expect_length(res.KMD.perm, 9)
        testthat::expect_named(res.KMD.perm, c("statistic", "p.value", "estimate", 
                                               "alternative", "method", "data.name", 
                                               "graph", "k", "kernel"))
        # check p values in [0,1]
        testthat::expect_lte(res.KMD$p.value, 1)
        testthat::expect_gte(res.KMD$p.value, 0)
        testthat::expect_lte(res.KMD.perm$p.value, 1)
        testthat::expect_gte(res.KMD.perm$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.KMD$statistic))
        testthat::expect_false(is.na(res.KMD.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.KMD, "htest")
        testthat::expect_s3_class(res.KMD.perm, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.KMD$estimate, res.KMD.test.stat,
                               check.attributes = FALSE)
        testthat::expect_equal(res.KMD.perm$statistic, res.KMD.test.stat, 
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.KMD$p.value, res.KMD_test[, "p value"], 
                               check.attributes = FALSE)
        testthat::expect_equal(res.KMD.perm$p.value, res.KMD_test.perm, 
                               check.attributes = FALSE)
        
        testthat::expect_equal(res.KMD$estimate, res.KMD.perm$statistic, 
                               check.attributes = FALSE)
      })
      
      
      res.KMD.1 <- DataSimilarity::KMD(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                       as.data.frame(X3)[, 1, drop = FALSE], 
                                       X4[, 1, drop = FALSE], X5[, 1, drop = FALSE], 
                                       X6[, 1, drop = FALSE], n.perm = 0, 
                                       k = 1, seed = i)
      res.KMD.perm.1 <- DataSimilarity::KMD(X1[, 1, drop = FALSE], 
                                            X2[, 1, drop = FALSE], 
                                            as.data.frame(X3)[, 1, drop = FALSE], 
                                            X4[, 1, drop = FALSE], 
                                            X5[, 1, drop = FALSE], 
                                            X6[, 1, drop = FALSE], 
                                            n.perm = 10, k = 1, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.KMD.1, 9)
        testthat::expect_named(res.KMD.1, c("statistic", "p.value", "estimate",
                                            "alternative", "method", "data.name", 
                                            "graph", "k", "kernel"))
        testthat::expect_length(res.KMD.perm.1, 9)
        testthat::expect_named(res.KMD.perm.1, c("statistic", "p.value", "estimate", 
                                                 "alternative", "method", "data.name", 
                                                 "graph", "k", "kernel"))
        # check p values in [0,1]
        testthat::expect_lte(res.KMD.1$p.value, 1)
        testthat::expect_gte(res.KMD.1$p.value, 0)
        testthat::expect_lte(res.KMD.perm.1$p.value, 1)
        testthat::expect_gte(res.KMD.perm.1$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.KMD.1$statistic))
        testthat::expect_false(is.na(res.KMD.perm.1$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.KMD.1, "htest")
        testthat::expect_s3_class(res.KMD.perm.1, "htest")
      })
    }
  }
}

set.seed(0305)
testKMD(10)
