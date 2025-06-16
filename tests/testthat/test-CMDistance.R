testCMDistance <- function(n.iter) {
  # Test example 2 in Tatti (2007)
  ex.2 <- DataSimilarity::CMDistance(X1 = data.frame(c("C", "C", "C", "A")), 
                                     X2 = data.frame(c("C", "A", "B", "A")),
                                     binary = FALSE, 
                                     S.fun = function(x) as.numeric(x == "C"),
                                     Omega = data.frame(c("A", "B", "C")))
  
  # Calculation of covariance matrix for binary case
  S.fun <- function(x) x
  Omega <- expand.grid(0:1, 0:1, 0:1, 0:1)
  cov.S <- DataSimilarity:::calc.cov.S(Omega, S.fun)
  
  testthat::test_that("output type", {
    # check length and names of output
    testthat::expect_length(ex.2, 11)
    testthat::expect_named(ex.2, c("statistic", "p.value", "estimate", 
                                   "alternative", "method", "data.name", "binary", 
                                   "cov", "S.fun", "cov.S", "Omega"))
    # check approx.and perm p value is NA
    testthat::expect_null(ex.2$p.value)
    testthat::expect_null(ex.2$p.value)
    # statistic is not NA
    testthat::expect_false(is.na(ex.2$statistic))
    # output should be numeric
    testthat::expect_s3_class(ex.2, "htest")
  })
  
  testthat::test_that("output values", {
    # check test statistic values
    testthat::expect_equal(ex.2$statistic, 3 / sqrt(8), check.attributes = FALSE)
    testthat::expect_equal(sum(abs(cov.S - diag(0.25, 4))), 0)
  })
  
  # Test if different calculations lead to same results
  Omega <- expand.grid(0:1, 0:1, 0:1)
  
  for(i in 1:n.iter){
    set.seed(i)
    X1bin <- matrix(sample(0:1, 100 * 3, replace = TRUE), ncol = 3)
    X2bin <- matrix(sample(0:1, 100 * 3, replace = TRUE, prob = 1:2), ncol = 3)
    
    binary <- DataSimilarity::CMDistance(X1bin, X2bin, binary = TRUE, cov = FALSE)
    res.calc.everything <- DataSimilarity::CMDistance(X1bin, X2bin, binary = FALSE, 
                                                      S.fun = S.fun, 
                                                      Omega = Omega)
    res.pre.calc.cov <- DataSimilarity::CMDistance(X1bin, X2bin, binary = FALSE, 
                                                   S.fun = S.fun, 
                                                   cov.S = 0.5 * diag(3))
    res.pre.calc.right.cov <- DataSimilarity::CMDistance(X1bin, X2bin, binary = FALSE, 
                                                         S.fun = S.fun, 
                                                         cov.S = 0.25 * diag(3))
    
    testthat::test_that("output type", {
      # check length and names of output
      testthat::expect_length(binary, 11)
      testthat::expect_named(binary, c("statistic", "p.value", "estimate", 
                                       "alternative", "method", "data.name", "binary", "cov", 
                                       "S.fun", "cov.S", "Omega"))
      testthat::expect_length(res.calc.everything, 11)
      testthat::expect_named(res.calc.everything, c("statistic", "p.value", "estimate", 
                                                    "alternative", "method", "data.name", 
                                                    "binary", "cov", 
                                                    "S.fun", "cov.S", "Omega"))
      testthat::expect_length(res.pre.calc.cov, 11)
      testthat::expect_named(res.pre.calc.cov, c("statistic", "p.value", "estimate", 
                                                 "alternative", "method", "data.name",
                                                 "binary", "cov", 
                                                 "S.fun", "cov.S", "Omega"))
      testthat::expect_length(res.pre.calc.right.cov, 11)
      testthat::expect_named(res.pre.calc.right.cov, c("statistic", "p.value", 
                                                       "estimate", "alternative", 
                                                       "method", "data.name", 
                                                       "binary", "cov",  "S.fun", 
                                                       "cov.S", "Omega"))
      # check approx.and perm p value is NULL
      testthat::expect_null(binary$p.value)
      testthat::expect_null(res.calc.everything$p.value)
      testthat::expect_null(res.pre.calc.cov$p.value)
      testthat::expect_null(res.pre.calc.right.cov$p.value)
      # statistic is not NA
      testthat::expect_false(is.na(binary$statistic))
      testthat::expect_false(is.na(res.calc.everything$statistic))
      testthat::expect_false(is.na(res.pre.calc.cov$statistic))
      testthat::expect_false(is.na(res.pre.calc.right.cov$statistic))
      # output should be numeric
      testthat::expect_s3_class(binary, "htest")
      testthat::expect_s3_class(res.calc.everything, "htest")
      testthat::expect_s3_class(res.pre.calc.cov, "htest")
      testthat::expect_s3_class(res.pre.calc.right.cov, "htest")
    })
    
    testthat::test_that("output values", {
      # check test statistic values
      testthat::expect_equal(binary$statistic, res.calc.everything$statistic,
                             check.attributes = FALSE)
      testthat::expect_equal(binary$statistic, res.pre.calc.cov$statistic * sqrt(2),
                             check.attributes = FALSE)
      testthat::expect_equal(binary$statistic, res.pre.calc.right.cov$statistic,
                             check.attributes = FALSE)
      testthat::expect_equal(binary$cov.S, res.calc.everything$cov.S,
                             check.attributes = FALSE)
      testthat::expect_equal(binary$cov.S, res.pre.calc.cov$cov.S / 2, 
                             check.attributes = FALSE)
      testthat::expect_equal(binary$cov.S, res.pre.calc.right.cov$cov.S, 
                             check.attributes = FALSE)
      testthat::expect_equal(sum(abs(binary$cov.S - diag(0.25, 3))), 0)
    })
  }
}

set.seed(0305)
testCMDistance(10)
