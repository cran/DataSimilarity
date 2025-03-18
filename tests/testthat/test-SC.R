testSC <- function(n.iter) {
  if(requireNamespace("gTestsMulti", quietly = TRUE) & requireNamespace("rmvnorm", quietly = TRUE)) {
    for(i in 1:n.iter) {
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
      dists.multi <- stats::dist(rbind(X1, X2, X3, X4, X5, X6))
      E.multi <- ade4::mstree(dists.multi)
      set.seed(i)
      res.gTest <- gTestsMulti::gtestsmulti(E.multi, list(X1, X2, X3, X4, X5, X6), 
                                            perm = 10)
      res.SC.S <- DataSimilarity::SC(X1, as.data.frame(X2), X3, X4, X5, X6, seed = i)
      res.SC.SA <- DataSimilarity::SC(X1, as.data.frame(X2), X3, X4, X5, X6, 
                                      type = "SA", seed = i)
      res.SC.S.perm <- DataSimilarity::SC(X1, as.data.frame(X2), X3, X4, X5, X6, 
                                          type = "S", n.perm = 10, seed = i)
      res.SC.SA.perm <- DataSimilarity::SC(X1, as.data.frame(X2), X3, X4, X5, X6, 
                                           type = "SA", n.perm = 10, seed = i)
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.SC.S, 6)
        testthat::expect_named(res.SC.S, c("statistic", "p.value", "estimate", 
                                           "alternative", "method", "data.name"))
        testthat::expect_length(res.SC.SA, 6)
        testthat::expect_named(res.SC.SA, c("statistic", "p.value", "estimate", 
                                            "alternative", "method", "data.name"))
        testthat::expect_length(res.SC.S.perm, 6)
        testthat::expect_named(res.SC.S.perm, c("statistic", "p.value", "estimate", 
                                                "alternative", "method", "data.name"))
        testthat::expect_length(res.SC.SA.perm, 6)
        testthat::expect_named(res.SC.SA.perm, c("statistic", "p.value", "estimate", 
                                                 "alternative", "method", "data.name"))
        # check p values in [0,1]
        testthat::expect_lte(res.SC.S$p.value, 1)
        testthat::expect_gte(res.SC.S$p.value, 0)
        testthat::expect_lte(res.SC.S.perm$p.value, 1)
        testthat::expect_gte(res.SC.S.perm$p.value, 0)
        testthat::expect_lte(res.SC.SA$p.value, 1)
        testthat::expect_gte(res.SC.SA$p.value, 0)
        testthat::expect_lte(res.SC.SA.perm$p.value, 1)
        testthat::expect_gte(res.SC.SA.perm$p.value, 0)
        # check perm. p value is NA
        # statistic is not NA
        testthat::expect_false(is.na(res.SC.S$statistic))
        testthat::expect_false(is.na(res.SC.SA$statistic))
        testthat::expect_false(is.na(res.SC.S.perm$statistic))
        testthat::expect_false(is.na(res.SC.SA.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.SC.S, "htest")
        testthat::expect_s3_class(res.SC.SA, "htest")
        testthat::expect_s3_class(res.SC.S.perm, "htest")
        testthat::expect_s3_class(res.SC.SA.perm, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.SC.S$statistic, res.gTest$teststat$S, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.SC.SA$statistic, res.gTest$teststat$S_A, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.SC.S.perm$statistic, res.gTest$teststat$S, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.SC.SA.perm$statistic, res.gTest$teststat$S_A, 
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.SC.S$p.value, res.gTest$pval$S_appr, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.SC.SA$p.value, res.gTest$pval$S_A_appr, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.SC.S.perm$p.value, res.gTest$pval$S_perm, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.SC.SA.perm$p.value, res.gTest$pval$S_A_perm, 
                               check.attributes = FALSE)
      })
      
      
      res.SC.S.1 <- DataSimilarity::SC(X1[, 1, drop = FALSE], 
                                       as.data.frame(X2)[, 1, drop = FALSE], 
                                       X3[, 1, drop = FALSE], X4[, 1, drop = FALSE], 
                                       X5[, 1, drop = FALSE], X6[, 1, drop = FALSE], 
                                       seed = i)
      res.SC.SA.1 <- DataSimilarity::SC(X1[, 1, drop = FALSE], 
                                        as.data.frame(X2)[, 1, drop = FALSE], 
                                        X3[, 1, drop = FALSE], X4[, 1, drop = FALSE], 
                                        X5[, 1, drop = FALSE], X6[, 1, drop = FALSE], 
                                        type = "SA", seed = i)
      res.SC.S.perm.1 <- DataSimilarity::SC(X1[, 1, drop = FALSE], 
                                            as.data.frame(X2)[, 1, drop = FALSE], 
                                            X3[, 1, drop = FALSE], X4[, 1, drop = FALSE], 
                                            X5[, 1, drop = FALSE], X6[, 1, drop = FALSE], 
                                            type = "S", n.perm = 10, seed = i)
      res.SC.SA.perm.1 <- DataSimilarity::SC(X1[, 1, drop = FALSE], 
                                             as.data.frame(X2)[, 1, drop = FALSE],
                                             X3[, 1, drop = FALSE], X4[, 1, drop = FALSE], 
                                             X5[, 1, drop = FALSE], X6[, 1, drop = FALSE], 
                                             type = "SA", n.perm = 10, seed = i)
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.SC.S.1, 6)
        testthat::expect_named(res.SC.S.1, c("statistic", "p.value", "estimate", 
                                             "alternative", "method", "data.name"))
        testthat::expect_length(res.SC.SA.1, 6)
        testthat::expect_named(res.SC.SA.1, c("statistic", "p.value", "estimate", 
                                              "alternative", "method", "data.name"))
        testthat::expect_length(res.SC.S.perm.1, 6)
        testthat::expect_named(res.SC.S.perm.1, c("statistic", "p.value", "estimate", 
                                                  "alternative", "method", "data.name"))
        testthat::expect_length(res.SC.SA.perm.1, 6)
        testthat::expect_named(res.SC.SA.perm.1, c("statistic", "p.value", "estimate", 
                                                   "alternative", "method", "data.name"))
        # check p values in [0,1]
        testthat::expect_lte(res.SC.S.1$p.value, 1)
        testthat::expect_gte(res.SC.S.1$p.value, 0)
        testthat::expect_lte(res.SC.S.perm.1$p.value, 1)
        testthat::expect_gte(res.SC.S.perm.1$p.value, 0)
        testthat::expect_lte(res.SC.SA.1$p.value, 1)
        testthat::expect_gte(res.SC.SA.1$p.value, 0)
        testthat::expect_lte(res.SC.SA.perm.1$p.value, 1)
        testthat::expect_gte(res.SC.SA.perm.1$p.value, 0)
        # check perm. p value is NA
        # statistic is not NA
        testthat::expect_false(is.na(res.SC.S.1$statistic))
        testthat::expect_false(is.na(res.SC.SA.1$statistic))
        testthat::expect_false(is.na(res.SC.S.perm.1$statistic))
        testthat::expect_false(is.na(res.SC.SA.perm.1$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.SC.S.1, "htest")
        testthat::expect_s3_class(res.SC.SA.1, "htest")
        testthat::expect_s3_class(res.SC.S.perm.1, "htest")
        testthat::expect_s3_class(res.SC.SA.perm.1, "htest")
      })
    }
  }
}

set.seed(0305)
testSC(1)
