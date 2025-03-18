testYMRZL <- function(n.iter) {
  if(requireNamespace("Ecume", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10),
                             mean = runif(10, -2, 2))
      X2 <- as.data.frame(mvtnorm::rmvnorm(100, mean = runif(10, -2, 2),
                                           sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10)))
      
      res.YMRZL <- DataSimilarity::YMRZL(X1, X2, n.perm = 0, seed = i)
      res.YMRZL.perm <- DataSimilarity::YMRZL(X1, X2, n.perm = 5, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.YMRZL, 8)
        testthat::expect_named(res.YMRZL, c("statistic", "parameter", "p.value", 
                                            "estimate", "alternative", "method", 
                                            "data.name", "classifier"))
        testthat::expect_length(res.YMRZL.perm, 8)
        testthat::expect_named(res.YMRZL.perm, c("statistic", "parameter", "p.value", 
                                                 "estimate", "alternative", "method", 
                                                 "data.name", "classifier"))
        # check p values in [0,1]
        testthat::expect_lte(res.YMRZL$p.value, 1)
        testthat::expect_gte(res.YMRZL$p.value, 0)
        testthat::expect_lte(res.YMRZL.perm$p.value, 1)
        testthat::expect_gte(res.YMRZL.perm$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.YMRZL$statistic))
        testthat::expect_false(is.na(res.YMRZL.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.YMRZL, "htest")
        testthat::expect_s3_class(res.YMRZL.perm, "htest")
      })
      
      res.YMRZL.1 <- DataSimilarity::YMRZL(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                           n.perm = 0, seed = i)
      res.YMRZL.perm.1 <- DataSimilarity::YMRZL(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                                n.perm = 10, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.YMRZL.1, 8)
        testthat::expect_named(res.YMRZL.1, c("statistic", "parameter", "p.value", 
                                              "estimate", "alternative", "method", 
                                              "data.name", "classifier"))
        testthat::expect_length(res.YMRZL.perm.1, 8)
        testthat::expect_named(res.YMRZL.perm.1, c("statistic", "parameter", "p.value", 
                                                   "estimate", "alternative", "method", 
                                                   "data.name", "classifier"))
        # check p values in [0,1]
        testthat::expect_lte(res.YMRZL.1$p.value, 1)
        testthat::expect_gte(res.YMRZL.1$p.value, 0)
        testthat::expect_lte(res.YMRZL.perm.1$p.value, 1)
        testthat::expect_gte(res.YMRZL.perm.1$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.YMRZL.1$statistic))
        testthat::expect_false(is.na(res.YMRZL.perm.1$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.YMRZL.1, "htest")
        testthat::expect_s3_class(res.YMRZL.perm.1, "htest")
      })
      # cannot test results against any function 
    }
  }
}

set.seed(0305)
testYMRZL(1)
