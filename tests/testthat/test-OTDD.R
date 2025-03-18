testOTDD <- function(n.iter) {
  for(i in 1:n.iter) {
    set.seed(i)
    X1 <- mvtnorm::rmvnorm(100, sigma = diag(10))
    X2 <- mvtnorm::rmvnorm(100, mean = rep(c(0.1, -0.1), 5), sigma = diag(10))
    
    y1 <- rbinom(100, 1, 1 / (1 + exp(1 - X1 %*% rep(0.5, 10))))
    y2 <- rbinom(100, 1, 1 / (1 + exp(1 - X2 %*% rep(0.7, 10))))
    
    X1 <- data.frame(y = factor(y1, levels = 0:1), X1)
    X2 <- data.frame(y = factor(y2, levels = 0:1), X2)
    
    res.OTDD <- DataSimilarity::OTDD(X1, X2, seed = i)
    res.OTDD.aug <- DataSimilarity::OTDD(X1, X2, method = "augmentation", seed = i)
    res.OTDD.ga <- DataSimilarity::OTDD(X1, X2, seed = i, inner.ot.method = "gaussian.approx")
    res.OTDD.nu <- DataSimilarity::OTDD(X1, X2, seed = i, inner.ot.method = "naive.upperbound")
    res.OTDD.mo <- DataSimilarity::OTDD(X1, X2, seed = i, inner.ot.method = "means.only")
    res.OTDD.si <- DataSimilarity::OTDD(X1, X2, seed = i, sinkhorn = TRUE, debias = FALSE)
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(res.OTDD, 5)
      testthat::expect_named(res.OTDD, c("statistic", "p.value", 
                                         "alternative", "method", "data.name"))
      testthat::expect_length(res.OTDD.aug, 5)
      testthat::expect_named(res.OTDD.aug, c("statistic", "p.value", 
                                             "alternative", "method", "data.name"))
      testthat::expect_length(res.OTDD.ga, 5)
      testthat::expect_named(res.OTDD.ga, c("statistic", "p.value", 
                                            "alternative", "method", "data.name"))
      testthat::expect_length(res.OTDD.nu, 5)
      testthat::expect_named(res.OTDD.nu, c("statistic", "p.value", 
                                            "alternative", "method", "data.name"))
      testthat::expect_length(res.OTDD.mo, 5)
      testthat::expect_named(res.OTDD.mo, c("statistic", "p.value", 
                                            "alternative", "method", "data.name"))
      testthat::expect_length(res.OTDD.si, 5)
      testthat::expect_named(res.OTDD.si, c("statistic", "p.value", 
                                            "alternative", "method", "data.name"))
      # check p values in [0,1]
      testthat::expect_true(is.null(res.OTDD$p.value))
      testthat::expect_true(is.null(res.OTDD.aug$p.value))
      testthat::expect_true(is.null(res.OTDD.ga$p.value))
      testthat::expect_true(is.null(res.OTDD.nu$p.value))
      testthat::expect_true(is.null(res.OTDD.mo$p.value))
      testthat::expect_true(is.null(res.OTDD.si$p.value))
      # statistic is not NA and >= 0
      testthat::expect_false(is.na(res.OTDD$statistic))
      testthat::expect_gte(res.OTDD$statistic, 0)
      testthat::expect_false(is.na(res.OTDD.aug$statistic))
      testthat::expect_gte(res.OTDD.aug$statistic, 0)
      testthat::expect_false(is.na(res.OTDD.ga$statistic))
      testthat::expect_gte(res.OTDD.ga$statistic, 0)
      testthat::expect_false(is.na(res.OTDD.nu$statistic))
      testthat::expect_gte(res.OTDD.nu$statistic, 0)
      testthat::expect_false(is.na(res.OTDD.mo$statistic))
      testthat::expect_gte(res.OTDD.mo$statistic, 0)
      testthat::expect_false(is.na(res.OTDD.si$statistic))
      testthat::expect_gte(res.OTDD.si$statistic, 0)
      # output should be numeric
      testthat::expect_s3_class(res.OTDD, "htest")
      testthat::expect_s3_class(res.OTDD.aug, "htest")
      testthat::expect_s3_class(res.OTDD.ga, "htest")
      testthat::expect_s3_class(res.OTDD.nu, "htest")
      testthat::expect_s3_class(res.OTDD.mo, "htest")
      testthat::expect_s3_class(res.OTDD.si, "htest")
    })
    # cannot test results against any function 
  }
}

set.seed(0305)
testOTDD(1)
