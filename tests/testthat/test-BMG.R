testBMG <- function(n.iter) {
  for(i in 1:n.iter) {
    set.seed(i)
    X1 <- mvtnorm::rmvnorm(100, sigma = diag(1, 5), mean = rep(i, 5))
    X2 <- as.data.frame(mvtnorm::rmvnorm(100, mean = rep(0, 5), sigma = diag(1, 5)))
    n.comb <- nrow(X1) + nrow(X2)
    set.seed(i)
    res.BMG.asymp <- DataSimilarity::BMG(X1, X2, seed = i, asymptotic = TRUE)
    res.BMG <- DataSimilarity::BMG(X1, X2, seed = i, asymptotic = FALSE)
    res.HP <- DataSimilarity::HamiltonPath(X1, X2, seed = i)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.BMG, 7)
      testthat::expect_length(res.BMG.asymp, 7)
      testthat::expect_named(res.BMG, c("statistic", "p.value", "estimate",
                                       "alternative", "method", "data.name", 
                                       "parameters"))
      testthat::expect_named(res.BMG.asymp, c("statistic", "p.value", "estimate",
                                        "alternative", "method", "data.name", 
                                        "parameters"))
      testthat::expect_length(res.HP, 2 * (n.comb - 1))
      # check p values in [0,1]
      testthat::expect_lte(res.BMG$p.value, 1)
      testthat::expect_gte(res.BMG$p.value, 0)
      testthat::expect_lte(res.BMG.asymp$p.value, 1)
      testthat::expect_gte(res.BMG.asymp$p.value, 0)
      # statistic and p values are not NA
      testthat::expect_false(is.na(res.BMG$statistic))
      testthat::expect_false(is.na(res.BMG$p.value))
      testthat::expect_false(is.na(res.BMG.asymp$statistic))
      testthat::expect_false(is.na(res.BMG.asymp$p.value))
      # output should be numeric
      testthat::expect_s3_class(res.BMG, "htest")
      testthat::expect_s3_class(res.BMG.asymp, "htest")
    })
    
    set.seed(i)
    res.BMG.asymp.1 <- DataSimilarity::BMG(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                           seed = i, asymptotic = TRUE)
    res.BMG.1 <- DataSimilarity::BMG(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                     seed = i, asymptotic = FALSE)
    res.HP.1 <- DataSimilarity::HamiltonPath(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                             seed = i)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.BMG.1, 7)
      testthat::expect_length(res.BMG.asymp.1, 7)
      testthat::expect_named(res.BMG.1, c("statistic", "p.value", "estimate",
                                        "alternative", "method", "data.name", 
                                        "parameters"))
      testthat::expect_named(res.BMG.asymp.1, c("statistic", "p.value", "estimate",
                                              "alternative", "method", "data.name", 
                                              "parameters"))
      testthat::expect_length(res.HP.1, 2 * (n.comb - 1))
      # check p values in [0,1]
      testthat::expect_lte(res.BMG.1$p.value, 1)
      testthat::expect_gte(res.BMG.1$p.value, 0)
      testthat::expect_lte(res.BMG.asymp.1$p.value, 1)
      testthat::expect_gte(res.BMG.asymp.1$p.value, 0)
      # statistic and p values are not NA
      testthat::expect_false(is.na(res.BMG.1$statistic))
      testthat::expect_false(is.na(res.BMG.1$p.value))
      testthat::expect_false(is.na(res.BMG.asymp.1$statistic))
      testthat::expect_false(is.na(res.BMG.asymp.1$p.value))
      # output should be numeric
      testthat::expect_s3_class(res.BMG.1, "htest")
      testthat::expect_s3_class(res.BMG.asymp.1, "htest")
    })
   
    testthat::test_that("output values", {
      # check output value of test statistic
      testthat::expect_equal(res.BMG$statistic, res.BMG.asymp$statistic)
    }) 
  }
}

set.seed(0305)
testBMG(1)
