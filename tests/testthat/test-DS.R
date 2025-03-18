testDS <- function(n.iter) {
  for(i in 1:n.iter) {
    set.seed(i)
    X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                           mean = runif(10, -2, 2))
    X2 <- as.data.frame(mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                                         sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10)))
    
    res.DS <- DataSimilarity::DS(X1, X2, n.perm = 0, seed = i)
    res.DS.perm <- DataSimilarity::DS(X1, X2, n.perm = 10, seed = i)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.DS, 5)
      testthat::expect_named(res.DS, c("statistic", "p.value", 
                                       "alternative", "method", "data.name"))
      testthat::expect_length(res.DS.perm, 5)
      testthat::expect_named(res.DS.perm, c("statistic", "p.value", 
                                            "alternative", "method", "data.name"))
      # check p values in [0,1]
      testthat::expect_lte(res.DS.perm$p.value, 1)
      testthat::expect_gte(res.DS.perm$p.value, 0)
      # check approx. p value is NULL
      testthat::expect_null(res.DS$p.value)
      # statistic is not NA
      testthat::expect_false(is.na(res.DS$statistic))
      testthat::expect_false(is.na(res.DS.perm$statistic))
      # output should be numeric
      testthat::expect_s3_class(res.DS, "htest")
      testthat::expect_s3_class(res.DS.perm, "htest")
    })
    # cannot test results against any function 
    
    res.DS.1 <- DataSimilarity::DS(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                   n.perm = 0, seed = i)
    res.DS.perm.1 <- DataSimilarity::DS(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                        n.perm = 10, seed = i)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.DS.1, 5)
      testthat::expect_named(res.DS.1, c("statistic", "p.value", 
                                         "alternative", "method", "data.name"))
      testthat::expect_length(res.DS.perm.1, 5)
      testthat::expect_named(res.DS.perm.1, c("statistic", "p.value", 
                                              "alternative", "method", "data.name"))
      # check p values in [0,1]
      testthat::expect_lte(res.DS.perm.1$p.value, 1)
      testthat::expect_gte(res.DS.perm.1$p.value, 0)
      # check approx. p value is NULL
      testthat::expect_null(res.DS.1$p.value)
      # statistic is not NA
      testthat::expect_false(is.na(res.DS.1$statistic))
      testthat::expect_false(is.na(res.DS.perm.1$statistic))
      # output should be numeric
      testthat::expect_s3_class(res.DS.1, "htest")
      testthat::expect_s3_class(res.DS.perm.1, "htest")
    })
  }
}

set.seed(0305)
testDS(1)
