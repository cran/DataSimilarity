testCramer <- function(n.iter, new.fun, old.kernel) {
  if(requireNamespace("cramer", quietly = TRUE) & requireNamespace("rmvnorm", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), mean = runif(10, -2, 2))
      X2 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      set.seed(i)
      res.cramer <- cramer::cramer.test(X1, X2, kernel = old.kernel, just.statistic = TRUE, 
                                        replicates = 0)
      res.Cramer <- new.fun(X1, X2, n.perm = 0, seed = i)
      
      set.seed(i)
      res.cramer.perm <- cramer::cramer.test(X1, X2, replicates = 3, kernel = old.kernel)
      res.Cramer.perm <- new.fun(X1, as.data.frame(X2), n.perm = 3, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.Cramer, 8)
        testthat::expect_named(res.Cramer, c("method", "d", "m", "n", "statistic", 
                                             "n.perm", "data.name",
                                             "alternative"))
        testthat::expect_length(res.Cramer.perm, 14)
        testthat::expect_named(res.Cramer.perm, c("method", "d", "m", "n", "statistic", 
                                                  "p.value", "sim", "n.perm", 
                                                  "hypdist", "ev", "hypdist.x",
                                                  "hypdist.Fx","data.name",
                                                  "alternative"))
        # check p values in [0,1]
        testthat::expect_lte(res.Cramer.perm$p.value, 1)
        testthat::expect_gte(res.Cramer.perm$p.value, 0)
        # check approx. p value is NULL
        testthat::expect_null(res.Cramer$p.value)
        # statistic is not NA
        testthat::expect_false(is.na(res.Cramer$statistic))
        testthat::expect_false(is.na(res.Cramer.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.Cramer, "htest")
        testthat::expect_s3_class(res.Cramer.perm, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.Cramer$statistic, res.cramer$statistic, check.attributes = FALSE)
        testthat::expect_equal(res.Cramer.perm$statistic, res.cramer.perm$statistic, check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.Cramer.perm$p.value, res.cramer.perm$p.value, check.attributes = FALSE)
      })
    }
  }
}

set.seed(0305)
testCramer(1, DataSimilarity::Cramer, "phiCramer")

set.seed(0305)
testCramer(1, function(X1, X2, n.perm, seed) DataSimilarity::BF(X1, X2, kernel = "phiLog", 
                                                                n.perm = n.perm, seed = seed), 
           "phiLog")

set.seed(0305)
testCramer(1, function(X1, X2, n.perm, seed) DataSimilarity::BF(X1, X2, kernel = "phiFracA", 
                                                                n.perm = n.perm, seed = seed),
           "phiFracA")

set.seed(0305)
testCramer(1, function(X1, X2, n.perm, seed) DataSimilarity::BF(X1, X2, kernel = "phiFracB", 
                                                                n.perm = n.perm, seed = seed),
           "phiFracB")

set.seed(0305)
testCramer(1, DataSimilarity::Bahr, "phiBahr")
