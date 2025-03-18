testC2ST <- function(n.iter, method = "knn") {
  if(requireNamespace("Ecume", quietly = TRUE)) {
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
      res.classifier_test <- Ecume::classifier_test(list(X1, X2, X3, X4, X5, X6),
                                                    method = method)
      res.C2ST <- DataSimilarity::C2ST(X1, as.data.frame(X2), X3, X4, X5, X6, 
                                       method = method, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.C2ST, 8)
        testthat::expect_named(res.C2ST, c("statistic", "parameter", "p.value", 
                                           "estimate", "alternative", "method", 
                                           "data.name", "classifier"))
        # check p values in [0,1]
        testthat::expect_lte(res.C2ST$p.value, 1)
        testthat::expect_gte(res.C2ST$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.C2ST$statistic))
        # output should be res.C2ST
        testthat::expect_s3_class(res.C2ST, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.C2ST$statistic, res.classifier_test$statistic, 
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.C2ST$p.value, res.classifier_test$p.value, 
                               check.attributes = FALSE)
      })
      
      set.seed(i)
      res.C2ST.1 <- DataSimilarity::C2ST(X1[, 1, drop = FALSE], 
                                         as.data.frame(X2)[, 1, drop = FALSE],
                                         X3[, 1, drop = FALSE], X4[, 1, drop = FALSE], 
                                         X5[, 1, drop = FALSE], X6[, 1, drop = FALSE], 
                                         method = method, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.C2ST, 8)
        testthat::expect_named(res.C2ST, c("statistic", "parameter", "p.value", 
                                           "estimate", "alternative", "method", 
                                           "data.name", "classifier"))
        # check p values in [0,1]
        testthat::expect_lte(res.C2ST$p.value, 1)
        testthat::expect_gte(res.C2ST$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.C2ST$statistic))
        # output should be res.C2ST
        testthat::expect_s3_class(res.C2ST, "htest")
      })
    }
  }
}

set.seed(0305)
testC2ST(3)

