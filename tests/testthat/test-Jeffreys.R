test_Jeffreys <- function(n.iter) {
  for(i in 1:n.iter) {
    set.seed(i)
    X1 <- mvtnorm::rmvnorm(100, sigma = diag(0.1 * i + 0.5, 5), mean = rep(1, 5))
    X2 <- as.data.frame(mvtnorm::rmvnorm(100, mean = rep(0, 5), sigma = diag(1, 5)))
    set.seed(i)
    res.Jeffreys <- DataSimilarity::Jeffreys(X1, X2, seed = i)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.Jeffreys, 7)
      testthat::expect_named(res.Jeffreys, c("statistic", "p.value", "estimate",
                                        "alternative", "method", "data.name", 
                                        "parameters"))
      # statistic and p values are not NA
      testthat::expect_false(is.na(res.Jeffreys$statistic))
      # output should be numeric
      testthat::expect_s3_class(res.Jeffreys, "htest")
    })
    
    res.Jeffreys.1 <- DataSimilarity::Jeffreys(X1[, 1, drop = FALSE], 
                                               X2[, 1, drop = FALSE], 
                                               seed = i)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.Jeffreys.1, 7)
      testthat::expect_named(res.Jeffreys.1, c("statistic", "p.value", "estimate",
                                             "alternative", "method", "data.name", 
                                             "parameters"))
      # statistic and p values are not NA
      testthat::expect_false(is.na(res.Jeffreys.1$statistic))
      # output should be numeric
      testthat::expect_s3_class(res.Jeffreys.1, "htest")
    })
  }
}

set.seed(0305)
test_Jeffreys(1)
