testWasserstein <- function(n.iter) {
  if(requireNamespace("Ecume", quietly = TRUE)) {
    for(i in 1:n.iter) {
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10),
                             mean = runif(10, -2, 2))
      X2 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      
      set.seed(i)
      res.wasserstein_permut <- Ecume::wasserstein_permut(X1, X2, iterations = 10)
      res.Wasserstein <- DataSimilarity::Wasserstein(X1, as.data.frame(X2), 
                                                     n.perm = 0, seed = i)
      res.Wasserstein.perm <- DataSimilarity::Wasserstein(X1, as.data.frame(X2), 
                                                          n.perm = 10, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.Wasserstein, 5)
        testthat::expect_named(res.Wasserstein, c("statistic", "p.value", 
                                                  "alternative", "method", "data.name"))
        testthat::expect_length(res.Wasserstein.perm, 5)
        testthat::expect_named(res.Wasserstein.perm, c("statistic", "p.value", 
                                                       "alternative", "method", "data.name"))
        # check p values in [0,1]
        testthat::expect_lte(res.Wasserstein.perm$p.value, 1)
        testthat::expect_gte(res.Wasserstein.perm$p.value, 0)
        
        # statistic is not NA
        testthat::expect_false(is.na(res.Wasserstein$statistic))
        testthat::expect_false(is.na(res.Wasserstein.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.Wasserstein, "htest")
        testthat::expect_s3_class(res.Wasserstein.perm, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.Wasserstein$statistic, res.wasserstein_permut$statistic,
                               check.attributes = FALSE)
        testthat::expect_equal(res.Wasserstein.perm$statistic, res.wasserstein_permut$statistic, 
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.Wasserstein.perm$p.value, res.wasserstein_permut$p.value, 
                               check.attributes = FALSE)
      })
      
      
      res.Wasserstein.1 <- DataSimilarity::Wasserstein(X1[, 1, drop = FALSE],
                                                       as.data.frame(X2)[, 1, drop = FALSE], 
                                                       n.perm = 0, seed = i)
      res.Wasserstein.perm.1 <- DataSimilarity::Wasserstein(X1[, 1, drop = FALSE],
                                                            as.data.frame(X2)[, 1, drop = FALSE], 
                                                            n.perm = 10, seed = i)
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.Wasserstein.1, 5)
        testthat::expect_named(res.Wasserstein.1, c("statistic", "p.value", 
                                                    "alternative", "method", 
                                                    "data.name"))
        testthat::expect_length(res.Wasserstein.perm.1, 5)
        testthat::expect_named(res.Wasserstein.perm.1, c("statistic", "p.value", 
                                                         "alternative", "method", 
                                                         "data.name"))
        # check p values in [0,1]
        testthat::expect_lte(res.Wasserstein.perm.1$p.value, 1)
        testthat::expect_gte(res.Wasserstein.perm.1$p.value, 0)
        
        # statistic is not NA
        testthat::expect_false(is.na(res.Wasserstein.1$statistic))
        testthat::expect_false(is.na(res.Wasserstein.perm.1$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.Wasserstein.1, "htest")
        testthat::expect_s3_class(res.Wasserstein.perm.1, "htest")
      })
    }
  }
}

set.seed(0305)
testWasserstein(10)
