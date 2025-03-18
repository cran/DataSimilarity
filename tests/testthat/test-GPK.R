testGPK <- function(n.iter) {
  if(requireNamespace("kerTests", quietly = TRUE) & requireNamespace("rmvnorm", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -2, 2))
      X2 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      
      set.seed(i)
      sigma <- kerTests::med_sigma(X1, X2)
      res.kertest.perm <- kerTests::kertests(X1, X2, sigma, perm = 10)
      res.GPK <- DataSimilarity::GPK(X1, as.data.frame(X2), seed = i)
      res.GPK.perm <- DataSimilarity::GPK(X1, as.data.frame(X2), n.perm = 10, 
                                          seed = i)
      res.GPK.perm.fast <- DataSimilarity::GPK(X1, as.data.frame(X2), n.perm = 10, 
                                               fast = TRUE, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.GPK, 6)
        testthat::expect_length(res.GPK$statistic, 3)
        testthat::expect_named(res.GPK$statistic, c("ZW1", "ZW2", "ZD"))
        testthat::expect_named(res.GPK, c("statistic", "p.value", "estimate", 
                                          "alternative", "method", "data.name"))
        testthat::expect_length(res.GPK.perm, 6)
        testthat::expect_named(res.GPK.perm, c("statistic", "p.value", "estimate", 
                                               "alternative", "method", "data.name"))
        testthat::expect_length(res.GPK.perm.fast, 6)
        testthat::expect_length(res.GPK.perm.fast$statistic, 3)
        testthat::expect_named(res.GPK.perm.fast$statistic, c("ZW1", "ZW2", "ZD"))
        testthat::expect_named(res.GPK.perm.fast, c("statistic", "p.value", "estimate",
                                                    "alternative", "method", "data.name"))
        # check p values in [0,1]
        testthat::expect_lte(res.GPK$p.value, 1)
        testthat::expect_gte(res.GPK$p.value, 0)
        testthat::expect_lte(res.GPK.perm$p.value, 1)
        testthat::expect_gte(res.GPK.perm$p.value, 0)
        testthat::expect_lte(res.GPK.perm.fast$p.value, 1)
        testthat::expect_gte(res.GPK.perm.fast$p.value, 0)
        # statistic is not NA
        testthat::expect_false(any(is.na(res.GPK$statistic)))
        testthat::expect_false(any(is.na(res.GPK.perm.fast$statistic)))
        testthat::expect_false(is.na(res.GPK.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.GPK, "htest")
        testthat::expect_s3_class(res.GPK.perm, "htest")
        testthat::expect_s3_class(res.GPK.perm.fast, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.GPK$statistic["ZW1"], res.kertest.perm$teststat$ZW1, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.GPK$statistic["ZW2"], res.kertest.perm$teststat$ZW2, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.GPK$statistic["ZD"], res.kertest.perm$teststat$ZD, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.GPK.perm$statistic, res.kertest.perm$teststat$GPK, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.GPK.perm.fast$statistic["ZW1"], res.kertest.perm$teststat$ZW1,
                               check.attributes = FALSE)
        testthat::expect_equal(res.GPK.perm.fast$statistic["ZW2"], res.kertest.perm$teststat$ZW2,
                               check.attributes = FALSE)
        testthat::expect_equal(res.GPK.perm.fast$statistic["ZD"], res.kertest.perm$teststat$ZD, 
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.GPK$p.value, res.kertest.perm$pval$fGPK_appr,
                               check.attributes = FALSE)
        testthat::expect_equal(res.GPK.perm.fast$p.value, res.kertest.perm$pval$fGPK_perm,
                               check.attributes = FALSE)
        testthat::expect_equal(res.GPK.perm$p.value, res.kertest.perm$pval$GPK_perm,
                               check.attributes = FALSE)
      })
      
      res.GPK.1 <- DataSimilarity::GPK(X1[, 1, drop = FALSE], 
                                       as.data.frame(X2)[, 1, drop = FALSE], 
                                       seed = i)
      res.GPK.perm.1 <- DataSimilarity::GPK(X1[, 1, drop = FALSE], 
                                            as.data.frame(X2)[, 1, drop = FALSE], 
                                            n.perm = 10, seed = i)
      res.GPK.perm.fast.1 <- DataSimilarity::GPK(X1, as.data.frame(X2), n.perm = 10, 
                                                 fast = TRUE, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.GPK.1, 6)
        testthat::expect_length(res.GPK.1$statistic, 3)
        testthat::expect_named(res.GPK.1$statistic, c("ZW1", "ZW2", "ZD"))
        testthat::expect_named(res.GPK.1, c("statistic", "p.value", "estimate", 
                                            "alternative", "method", "data.name"))
        testthat::expect_length(res.GPK.perm.1, 6)
        testthat::expect_named(res.GPK.perm.1, c("statistic", "p.value", "estimate", 
                                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.GPK.perm.fast.1, 6)
        testthat::expect_length(res.GPK.perm.fast.1$statistic, 3)
        testthat::expect_named(res.GPK.perm.fast.1$statistic, c("ZW1", "ZW2", "ZD"))
        testthat::expect_named(res.GPK.perm.fast.1, c("statistic", "p.value", "estimate",
                                                      "alternative", "method", "data.name"))
        # check p values in [0,1]
        testthat::expect_lte(res.GPK.1$p.value, 1)
        testthat::expect_gte(res.GPK.1$p.value, 0)
        testthat::expect_lte(res.GPK.perm.1$p.value, 1)
        testthat::expect_gte(res.GPK.perm.1$p.value, 0)
        testthat::expect_lte(res.GPK.perm.fast.1$p.value, 1)
        testthat::expect_gte(res.GPK.perm.fast.1$p.value, 0)
        # statistic is not NA
        testthat::expect_false(any(is.na(res.GPK.1$statistic)))
        testthat::expect_false(any(is.na(res.GPK.perm.fast.1$statistic)))
        testthat::expect_false(is.na(res.GPK.perm.1$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.GPK.1, "htest")
        testthat::expect_s3_class(res.GPK.perm.1, "htest")
        testthat::expect_s3_class(res.GPK.perm.fast.1, "htest")
      })
    }
  }
}

set.seed(0305)
testGPK(10)
