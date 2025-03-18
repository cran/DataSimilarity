testBG <- function(n.iter) {
  for(i in 1:n.iter) {
    set.seed(i)
    X1 <- mvtnorm::rmvnorm(100, sigma = diag(1, 5), mean = rep(i, 5))
    X2 <- mvtnorm::rmvnorm(100, mean = rep(0, 5), sigma = diag(1, 5))
    set.seed(i)
    res.BG <- DataSimilarity::BG(X1, as.data.frame(X2), seed = i)
    res.rectPart <- DataSimilarity::rectPartition(X1, X2, n = nrow(X1), p = ncol(X1))
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.BG, 7)
      testthat::expect_named(res.BG, c("statistic", "p.value", "estimate",
                                       "alternative", "method", "data.name", 
                                       "parameters"))
      testthat::expect_length(res.rectPart, 3)
      # check p values in [0,1]
      testthat::expect_lte(res.BG$p.value, 1)
      testthat::expect_gte(res.BG$p.value, 0)
      # statistic and p values are not NA
      testthat::expect_false(is.na(res.BG$statistic))
      testthat::expect_false(is.na(res.BG$p.value))
      # output should be numeric
      testthat::expect_s3_class(res.BG, "htest")
    })
    
    set.seed(i)
    res.BG.1 <- DataSimilarity::BG(X1[, 1, drop = FALSE], as.data.frame(X2)[, 1, drop = FALSE], 
                                   seed = i)
    res.rectPart.1 <- DataSimilarity::rectPartition(X1[, 1, drop = FALSE], 
                                                    X2[, 1, drop = FALSE],
                                                    n = nrow(X1), p = 1)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.BG.1, 7)
      testthat::expect_named(res.BG.1, c("statistic", "p.value", "estimate",
                                       "alternative", "method", "data.name", 
                                       "parameters"))
      testthat::expect_length(res.rectPart.1, 3)
      # check p values in [0,1]
      testthat::expect_lte(res.BG.1$p.value, 1)
      testthat::expect_gte(res.BG.1$p.value, 0)
      # statistic and p values are not NA
      testthat::expect_false(is.na(res.BG.1$statistic))
      testthat::expect_false(is.na(res.BG.1$p.value))
      # output should be numeric
      testthat::expect_s3_class(res.BG.1, "htest")
    })
    
    
  }
  X <- matrix(c(1:100), ncol = 1)
  Y <- matrix(c(100:1), ncol = 1)
  eps <- 0.01
  res.rectPart1Dim <- DataSimilarity::rectPartition(X, Y, 100, 1, eps = 0.01)
  res.UB <- res.rectPart1Dim[[3]] 
  testthat::test_that("output values", {
    # check partition
    testthat::expect_equal(res.rectPart1Dim[[1]][[1]][1], 1 - eps)
    testthat::expect_equal(res.rectPart1Dim[[1]][[1]][res.UB + 1], 100)
  })
}

set.seed(0305)
testBG(10)
