testBG2 <- function(n.iter) {
  for(i in 1:n.iter) {
    set.seed(i)
    X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                           mean = runif(10, -2, 2))
    X2 <- as.data.frame(mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                                         sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10)))
    
    res.BG2 <- DataSimilarity::BG2(X1, X2, n.perm = 0, seed = i)
    res.BG2.perm <- DataSimilarity::BG2(X1, X2, n.perm = 10, seed = i)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.BG2, 7)
      testthat::expect_named(res.BG2, c("statistic", "parameter", "p.value", "estimate",
                                       "alternative", "method", "data.name"))
      testthat::expect_length(res.BG2.perm, 7)
      testthat::expect_named(res.BG2.perm, c("statistic",  "parameter", "p.value", "estimate",
                                            "alternative", "method", "data.name"))
      # check p values in [0,1]
      testthat::expect_lte(res.BG2.perm$p.value, 1)
      testthat::expect_gte(res.BG2.perm$p.value, 0)
      testthat::expect_lte(res.BG2$p.value, 1)
      testthat::expect_gte(res.BG2$p.value, 0)
      # statistic is not NA
      testthat::expect_false(is.na(res.BG2$statistic))
      testthat::expect_false(is.na(res.BG2.perm$statistic))
      # output should be numeric
      testthat::expect_s3_class(res.BG2, "htest")
      testthat::expect_s3_class(res.BG2.perm, "htest")
    })
    
    res.BG2.1 <- DataSimilarity::BG2(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                     n.perm = 0, seed = i)
    res.BG2.perm.1 <- DataSimilarity::BG2(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE],
                                          n.perm = 10, seed = i)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.BG2.1, 7)
      testthat::expect_named(res.BG2.1, c("statistic", "parameter", "p.value", "estimate",
                                           "alternative", "method", "data.name"))
      testthat::expect_length(res.BG2.perm.1, 7)
      testthat::expect_named(res.BG2.perm.1, c("statistic",  "parameter", "p.value", "estimate",
                                                "alternative", "method", "data.name"))
      # check p values in [0,1]
      testthat::expect_lte(res.BG2.perm.1$p.value, 1)
      testthat::expect_gte(res.BG2.perm.1$p.value, 0)
      testthat::expect_lte(res.BG2.1$p.value, 1)
      testthat::expect_gte(res.BG2.1$p.value, 0)
      # statistic is not NA
      testthat::expect_false(is.na(res.BG2.1$statistic))
      testthat::expect_false(is.na(res.BG2.perm.1$statistic))
      # output should be numeric
      testthat::expect_s3_class(res.BG2.1, "htest")
      testthat::expect_s3_class(res.BG2.perm.1, "htest")
    })
    # cannot test results against any function 
  }
}

set.seed(0305)
testBG2(2)
