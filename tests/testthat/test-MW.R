testMW <- function(n.iter) {
  if(requireNamespace("LPKsample", quietly = TRUE)) {
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
      res.GLP <- LPKsample::GLP(rbind(X1, X2, X3, X4, X5, X6),  
                                c(rep(1, nrow(X1)), rep(2, nrow(X2)), 
                                  rep(3, nrow(X3)), rep(4, nrow(X4)), 
                                  rep(5, nrow(X5)), rep(6, nrow(X6))), 
                                combine.criterion = "kernel")
      set.seed(i)
      res.GLP.perm <- LPKsample::GLP(rbind(X1, X2, X3, X4, X5, X6),  
                                     c(rep(1, nrow(X1)), rep(2, nrow(X2)), 
                                       rep(3, nrow(X3)), rep(4, nrow(X4)), 
                                       rep(5, nrow(X5)), rep(6, nrow(X6))), 
                                     combine.criterion = "kernel", perm = 3)
      res.MW <- DataSimilarity::MW(X1, X2, as.data.frame(X3), X4, X5, X6, seed = i)
      res.MW.perm <- DataSimilarity::MW(X1, X2, as.data.frame(X3), X4, X5, X6, 
                                        n.perm = 3, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.MW, 8)
        testthat::expect_named(res.MW, c("statistic", "parameter", "p.value",
                                         "estimate", "alternative", "method", 
                                         "data.name", "components"))
        testthat::expect_length(res.MW.perm, 8)
        testthat::expect_named(res.MW.perm, c("statistic", "parameter", "p.value", 
                                              "estimate", "alternative", "method", 
                                              "data.name", "components"))
        # check p values in [0,1]
        testthat::expect_lte(res.MW$p.value, 1)
        testthat::expect_gte(res.MW$p.value, 0)
        testthat::expect_lte(res.MW.perm$p.value, 1)
        testthat::expect_gte(res.MW.perm$p.value, 0)
        # statistic and p values are not NA
        testthat::expect_false(is.na(res.MW$statistic))
        testthat::expect_false(is.na(res.MW$p.value))
        testthat::expect_false(is.na(res.MW.perm$statistic))
        testthat::expect_false(is.na(res.MW.perm$p.value))
        # output should be numeric
        testthat::expect_s3_class(res.MW, "htest")
        testthat::expect_s3_class(res.MW.perm, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.MW$statistic, res.GLP$GLP, check.attributes = FALSE)
        testthat::expect_equal(res.MW.perm$statistic, res.GLP.perm$GLP, check.attributes = FALSE)
        
        # check test p values
        testthat::expect_equal(res.MW$p.value, res.GLP$pval, check.attributes = FALSE)
        testthat::expect_equal(res.MW.perm$p.value, res.GLP.perm$pval, check.attributes = FALSE)
      })
      # LPKsample implementation does not work for univariate data --> cannot fix here
    }
  }
}

set.seed(0305)
testMW(1)
