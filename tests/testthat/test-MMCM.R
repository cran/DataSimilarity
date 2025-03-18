testMMCM <- function(n.iter) {
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
    res.MMCM <- DataSimilarity::MMCM(X1, X2, as.data.frame(X3), X4, X5, X6, seed = i)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.MMCM, 7)
      testthat::expect_named(res.MMCM, c("statistic", "parameter", "p.value",
                                         "estimate", "alternative", "method", 
                                         "data.name"))
      # check p values in [0,1]
      testthat::expect_lte(res.MMCM$p.value, 1)
      testthat::expect_gte(res.MMCM$p.value, 0)
      # statistic and p values are not NA
      testthat::expect_false(is.na(res.MMCM$statistic))
      testthat::expect_false(is.na(res.MMCM$p.value))
      # output should be numeric
      testthat::expect_s3_class(res.MMCM, "htest")
    })
    
    
    res.MMCM.1 <- DataSimilarity::MMCM(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                       as.data.frame(X3)[, 1, drop = FALSE], 
                                       X4[, 1, drop = FALSE], X5[, 1, drop = FALSE], 
                                       X6[, 1, drop = FALSE], seed = i)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.MMCM.1, 7)
      testthat::expect_named(res.MMCM.1, c("statistic", "parameter", "p.value",
                                         "estimate", "alternative", "method", 
                                         "data.name"))
      # check p values in [0,1]
      testthat::expect_lte(res.MMCM.1$p.value, 1)
      testthat::expect_gte(res.MMCM.1$p.value, 0)
      # statistic and p values are not NA
      testthat::expect_false(is.na(res.MMCM.1$statistic))
      testthat::expect_false(is.na(res.MMCM.1$p.value))
      # output should be htest
      testthat::expect_s3_class(res.MMCM.1, "htest")
    })
  }
}

set.seed(0305)
testMMCM(10)
