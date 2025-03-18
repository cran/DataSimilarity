testRosenbaum <- function(n.iter) {
  if(requireNamespace("crossmatch", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -2, 2))
      X2 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2),
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      dists <- dist(rbind(X1, X2))
      set.seed(i)
      res.crossmatch <- crossmatch::crossmatchtest(c(rep(0, nrow(X1)), rep(1, nrow(X2))), 
                                                   as.matrix(dists))
      res.Rosenbaum <- DataSimilarity::Rosenbaum(X1, as.data.frame(X2), seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.Rosenbaum, 8)
        testthat::expect_named(res.Rosenbaum, c("statistic", "p.value", "estimate",  
                                                "alternative", "method", "data.name", 
                                                "stderr", "mu0"))
        # check p values in [0,1]
        testthat::expect_lte(res.Rosenbaum$p.value, 1)
        testthat::expect_gte(res.Rosenbaum$p.value, 0)
        testthat::expect_lte(res.Rosenbaum$p.value, 1)
        testthat::expect_gte(res.Rosenbaum$p.value, 0)
        # statistic and p values are not NA
        testthat::expect_false(is.na(res.Rosenbaum$statistic))
        testthat::expect_false(is.na(res.Rosenbaum$p.value))
        # output should be numeric
        testthat::expect_s3_class(res.Rosenbaum, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.Rosenbaum$statistic, res.crossmatch$dev, 
                               check.attributes = FALSE)
        
        # check test p values
        testthat::expect_equal(res.Rosenbaum$p.value, res.crossmatch$approxpval, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.Rosenbaum$p.value, res.crossmatch$pval, 
                               check.attributes = FALSE)
      })
      
      
      res.Rosenbaum.1 <- DataSimilarity::Rosenbaum(X1, as.data.frame(X2), seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.Rosenbaum.1, 8)
        testthat::expect_named(res.Rosenbaum.1, c("statistic", "p.value", "estimate",  
                                                "alternative", "method", "data.name", 
                                                "stderr", "mu0"))
        # check p values in [0,1]
        testthat::expect_lte(res.Rosenbaum.1$p.value, 1)
        testthat::expect_gte(res.Rosenbaum.1$p.value, 0)
        testthat::expect_lte(res.Rosenbaum.1$p.value, 1)
        testthat::expect_gte(res.Rosenbaum.1$p.value, 0)
        # statistic and p values are not NA
        testthat::expect_false(is.na(res.Rosenbaum.1$statistic))
        testthat::expect_false(is.na(res.Rosenbaum.1$p.value))
        # output should be numeric
        testthat::expect_s3_class(res.Rosenbaum.1, "htest")
      })
    }
  }
}

set.seed(0305)
testRosenbaum(10)
