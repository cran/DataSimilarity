testBQS <- function(n.iter) {
  for(i in 1:n.iter) {
    set.seed(i)
    X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                           mean = runif(10, -2, 2))
    X2 <- as.data.frame(mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                                         sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10)))
    
    res.BQS <- DataSimilarity::BQS(X1, X2, n.perm = 0, seed = i)
    res.BQS.perm <- DataSimilarity::BQS(X1, X2, n.perm = 10, seed = i)

    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.BQS, 6)
      testthat::expect_named(res.BQS, c("statistic", "p.value", "estimate",
                                       "alternative", "method", "data.name"))
      testthat::expect_length(res.BQS.perm, 6)
      testthat::expect_named(res.BQS.perm, c("statistic", "p.value", "estimate",
                                            "alternative", "method", "data.name"))
      # check p values in [0,1]
      testthat::expect_lte(res.BQS.perm$p.value, 1)
      testthat::expect_gte(res.BQS.perm$p.value, 0)
      testthat::expect_null(res.BQS$p.value)
      # statistic is not NA
      testthat::expect_false(is.na(res.BQS$statistic))
      testthat::expect_false(is.na(res.BQS.perm$statistic))
      # output should be numeric
      testthat::expect_s3_class(res.BQS, "htest")
      testthat::expect_s3_class(res.BQS.perm, "htest")
    })
    
    res.BQS.1 <- DataSimilarity::BQS(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                     n.perm = 0, seed = i)
    res.BQS.perm.1 <- DataSimilarity::BQS(X1[, 1, drop = FALSE], 
                                          X2[, 1, drop = FALSE],
                                          n.perm = 10, seed = i)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.BQS.1, 6)
      testthat::expect_named(res.BQS.1, c("statistic", "p.value", "estimate",
                                        "alternative", "method", "data.name"))
      testthat::expect_length(res.BQS.perm.1, 6)
      testthat::expect_named(res.BQS.perm.1, c("statistic", "p.value", "estimate",
                                             "alternative", "method", "data.name"))
      # check p values in [0,1]
      testthat::expect_lte(res.BQS.perm.1$p.value, 1)
      testthat::expect_gte(res.BQS.perm.1$p.value, 0)
      testthat::expect_null(res.BQS.1$p.value)
      # statistic is not NA
      testthat::expect_false(is.na(res.BQS.1$statistic))
      testthat::expect_false(is.na(res.BQS.perm.1$statistic))
      # output should be numeric
      testthat::expect_s3_class(res.BQS.1, "htest")
      testthat::expect_s3_class(res.BQS.perm.1, "htest")
    })
    # cannot test results against any function 
  }
}

set.seed(0305)
testBQS(10)
