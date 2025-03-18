testEngineerMetric <- function(n.iter) {
  for(i in 1:n.iter) {
    set.seed(i)
    X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                           mean = runif(10, -2, 2))
    X2 <- as.data.frame(mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                           sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10)))
    
    res.engineerMetric <- DataSimilarity::engineerMetric(X1, X2, seed = i)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.engineerMetric, 5)
      testthat::expect_named(res.engineerMetric, c("statistic", "p.value", 
                                                   "alternative", "method", 
                                                   "data.name"))
      # check approx. p value is NULL
      testthat::expect_null(res.engineerMetric$p.value)
      # statistic is not NA
      testthat::expect_false(is.na(res.engineerMetric$statistic))
      # output should be numeric
      testthat::expect_s3_class(res.engineerMetric, "htest")
    })
    # cannot test results against any function 
    
    res.engineerMetric.1 <- DataSimilarity::engineerMetric(X1[, 1, drop = FALSE], 
                                                           X2[, 1, drop = FALSE],
                                                           seed = i)
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.engineerMetric.1, 5)
      testthat::expect_named(res.engineerMetric.1, c("statistic", "p.value", 
                                                   "alternative", "method", 
                                                   "data.name"))
      # check approx. p value is NULL
      testthat::expect_null(res.engineerMetric.1$p.value)
      # statistic is not NA
      testthat::expect_false(is.na(res.engineerMetric.1$statistic))
      # output should be numeric
      testthat::expect_s3_class(res.engineerMetric.1, "htest")
    })
  }
}

set.seed(0305)
testEngineerMetric(10)
