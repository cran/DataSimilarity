test_LHZ <- function(n.iter) {
  for(i in 1:n.iter) {
    set.seed(i)
    X1 <- mvtnorm::rmvnorm(100, sigma = diag(1, 5), mean = rep(i, 5))
    X2 <- mvtnorm::rmvnorm(100, mean = rep(0, 5), sigma = diag(1, 5))
    set.seed(i)
    res.LHZ <- DataSimilarity::LHZ(X1, as.data.frame(X2), seed = i)
    res.LHZ.perm <- DataSimilarity::LHZ(X1, as.data.frame(X2), seed = i, n.perm = i)
    res.LHZ.stat <- DataSimilarity::LHZStatistic(X1, X2)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.LHZ, 7)
      testthat::expect_length(res.LHZ.perm, 7)
      testthat::expect_length(res.LHZ.stat, 1)
      testthat::expect_named(res.LHZ, c("statistic", "p.value", "estimate",
                                             "alternative", "method", "data.name", 
                                             "parameters"))
      testthat::expect_named(res.LHZ.perm, c("statistic", "p.value", "estimate",
                                        "alternative", "method", "data.name", 
                                        "parameters"))
      # statistic and p values are not NA
      testthat::expect_true(is.null(res.LHZ$p.value))
      testthat::expect_false(is.na(res.LHZ.perm$p.value))
      testthat::expect_false(is.na(res.LHZ$statistic))
      testthat::expect_false(is.na(res.LHZ.perm$statistic))
      # check p values in [0,1]
      testthat::expect_lte(res.LHZ.perm$p.value, 1)
      testthat::expect_gte(res.LHZ.perm$p.value, 0)
      # output should be htest
      testthat::expect_s3_class(res.LHZ, "htest")
      testthat::expect_s3_class(res.LHZ.perm, "htest")
    })
    
    testthat::test_that("output values", {
      # check output value of test statistic
      testthat::expect_equal(res.LHZ$statistic, res.LHZ.stat, 
                             check.attributes = FALSE)
      testthat::expect_equal(res.LHZ.perm$statistic, res.LHZ.stat,
                             check.attributes = FALSE)
    }) 
    
    res.LHZ.1 <- DataSimilarity::LHZ(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                     seed = i)
    res.LHZ.perm.1 <- DataSimilarity::LHZ(X1[, 1, drop = FALSE], X2[, 1, drop = FALSE], 
                                          seed = i, n.perm = i)
    res.LHZ.stat.1 <- DataSimilarity::LHZStatistic(X1[, 1, drop = FALSE],
                                                   X2[, 1, drop = FALSE])
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.LHZ.1, 7)
      testthat::expect_length(res.LHZ.perm.1, 7)
      testthat::expect_length(res.LHZ.stat.1, 1)
      testthat::expect_named(res.LHZ.1, c("statistic", "p.value", "estimate",
                                        "alternative", "method", "data.name", 
                                        "parameters"))
      testthat::expect_named(res.LHZ.perm.1, c("statistic", "p.value", "estimate",
                                             "alternative", "method", "data.name", 
                                             "parameters"))
      # statistic and p values are not NA
      testthat::expect_true(is.null(res.LHZ.1$p.value))
      testthat::expect_false(is.na(res.LHZ.perm.1$p.value))
      testthat::expect_false(is.na(res.LHZ$statistic))
      testthat::expect_false(is.na(res.LHZ.perm$statistic))
      # check p values in [0,1]
      testthat::expect_lte(res.LHZ.perm.1$p.value, 1)
      testthat::expect_gte(res.LHZ.perm.1$p.value, 0)
      # output should be htest
      testthat::expect_s3_class(res.LHZ.1, "htest")
      testthat::expect_s3_class(res.LHZ.perm, "htest")
    })
    
    testthat::test_that("output values", {
      # check output value of test statistic
      testthat::expect_equal(res.LHZ.1$statistic, res.LHZ.stat.1, 
                             check.attributes = FALSE)
      testthat::expect_equal(res.LHZ.perm.1$statistic, res.LHZ.stat.1,
                             check.attributes = FALSE)
    }) 
  }
}

set.seed(0305)
test_LHZ(1)
