testBallDivergence <- function(n.iter) {
  if(requireNamespace("Ball", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -2, 2))
      X2 <- as.data.frame(mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                                           sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10)))
      
      set.seed(1234)
      res.bd.test <- Ball::bd.test(X1, X2, num.permutations = 1, method = "limit")
      set.seed(1234)
      res.bd.test.perm <- Ball::bd.test(X1, X2, num.permutations = 10, method = "permutation")
      set.seed(1234)
      res.BallDivergence <- DataSimilarity::BallDivergence(X1, X2, n.perm = 0, seed = i)
      set.seed(1234)
      res.BallDivergence.perm <- DataSimilarity::BallDivergence(X1, X2, n.perm = 10, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.BallDivergence, 7)
        testthat::expect_named(res.BallDivergence, c("statistic", "p.value", "n.perm", 
                                                     "size", "alternative", 
                                                     "method", "data.name"))
        testthat::expect_length(res.BallDivergence.perm, 7)
        testthat::expect_named(res.BallDivergence.perm, c("statistic", "p.value", "n.perm", 
                                                          "size", "alternative", 
                                                          "method", "data.name"))
        # check p values in [0,1]
        testthat::expect_lte(res.BallDivergence$p.value, 1)
        testthat::expect_gte(res.BallDivergence$p.value, 0)
        testthat::expect_lte(res.BallDivergence.perm$p.value, 1)
        testthat::expect_gte(res.BallDivergence.perm$p.value, 0)
        
        # statistic is not NA
        testthat::expect_false(is.na(res.BallDivergence$statistic))
        testthat::expect_false(is.na(res.BallDivergence.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.BallDivergence, "htest")
        testthat::expect_s3_class(res.BallDivergence.perm, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.BallDivergence$statistic, res.bd.test$statistic, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.BallDivergence.perm$statistic,
                               res.bd.test.perm$statistic,
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.BallDivergence$p.value, res.bd.test$p.value, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.BallDivergence.perm$p.value,
                               res.bd.test.perm$p.value, 
                               check.attributes = FALSE)
        
        testthat::expect_equal(res.BallDivergence$statistic, 
                               res.BallDivergence.perm$statistic,
                               check.attributes = FALSE)
      })
    }
  }
}

set.seed(0305)
testBallDivergence(10)
