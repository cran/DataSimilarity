testHMN <- function(n.iter) {
  if(requireNamespace("hypoRF", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -1, 1))
      X2 <- mvtnorm::rmvnorm(100, mean = runif(10, -1, 1), 
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      
      res.hypoRF <- hypoRF::hypoRF(as.data.frame(X1), as.data.frame(X2), K = 1, 
                                   seed = i)
      res.hypoRF.perm <- hypoRF::hypoRF(as.data.frame(X1), as.data.frame(X2),
                                        K = 10, seed = i)
      res.HMN <- DataSimilarity::HMN(X1, as.data.frame(X2), n.perm = 0, seed = i)
      res.HMN.perm <- DataSimilarity::HMN(X1, as.data.frame(X2), n.perm = 10, seed = i)
      
      res.hypoRF.OverallOOB <- hypoRF::hypoRF(as.data.frame(X1), as.data.frame(X2), 
                                              K = 1, statistic = "OverallOOB", seed = i)
      res.hypoRF.perm.OverallOOB <- hypoRF::hypoRF(as.data.frame(X1), as.data.frame(X2), 
                                                   K = 10, statistic = "OverallOOB", 
                                                   seed = i)
      res.HMN.OverallOOB <- DataSimilarity::HMN(X1, as.data.frame(X2), n.perm = 0, 
                                                statistic = "OverallOOB", seed = i)
      res.HMN.perm.OverallOOB <- DataSimilarity::HMN(X1, as.data.frame(X2), n.perm = 10, 
                                                     statistic = "OverallOOB", seed = i)
      
      res.hypoRF.n <- hypoRF::hypoRF(as.data.frame(X1), as.data.frame(X2), K = 1, 
                                     normalapprox = TRUE, seed = i)
      res.hypoRF.perm.n <- hypoRF::hypoRF(as.data.frame(X1), as.data.frame(X2), 
                                          K = 10, normalapprox = TRUE, seed = i)
      res.HMN.n <- DataSimilarity::HMN(X1, as.data.frame(X2), n.perm = 0,
                                       normal.approx = TRUE, seed = i)
      res.HMN.perm.n <- DataSimilarity::HMN(X1, as.data.frame(X2), n.perm = 10,
                                            normal.approx = TRUE, seed = i)
      
      res.hypoRF.OverallOOB.n <- hypoRF::hypoRF(as.data.frame(X1), as.data.frame(X2), K = 1, 
                                                statistic = "OverallOOB", normalapprox = TRUE,
                                                seed = i)
      res.hypoRF.perm.OverallOOB.n <- hypoRF::hypoRF(as.data.frame(X1), as.data.frame(X2),
                                                     K = 10, statistic = "OverallOOB", 
                                                     normalapprox = TRUE, seed = i)
      res.HMN.OverallOOB.n <- DataSimilarity::HMN(X1, as.data.frame(X2), n.perm = 0, 
                                                  statistic = "OverallOOB", 
                                                  normal.approx = TRUE, seed = i)
      res.HMN.perm.OverallOOB.n <- DataSimilarity::HMN(X1, as.data.frame(X2), 
                                                       n.perm = 10, statistic = "OverallOOB", 
                                                       normal.approx = TRUE, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.HMN, 12)
        testthat::expect_named(res.HMN, 
                               c("statistic", "parameter", "p.value", "estimate", 
                                 "alternative", "method","data.name",  "val", "varest", 
                                 "importance.ranking", "importance.distribution",
                                 "cut.off"))
        testthat::expect_length(res.HMN.perm, 12)
        testthat::expect_named(res.HMN.perm,
                               c("statistic", "parameter", "p.value",  "estimate", 
                                 "alternative", "method",  "data.name", "val", 
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        testthat::expect_length(res.HMN.OverallOOB, 12)
        testthat::expect_named(res.HMN.OverallOOB, 
                               c("statistic", "parameter", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "val", 
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        testthat::expect_length(res.HMN.perm.OverallOOB, 12)
        testthat::expect_named(res.HMN.perm.OverallOOB, 
                               c("statistic", "parameter", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "val", 
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        
        testthat::expect_length(res.HMN.n, 12)
        testthat::expect_named(res.HMN.n,
                               c("statistic", "parameter", "p.value", "estimate",  
                                 "alternative",  "method", "data.name", "val",
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        testthat::expect_length(res.HMN.perm.n, 12)
        testthat::expect_named(res.HMN.perm.n, 
                               c("statistic", "parameter", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "val",
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        testthat::expect_length(res.HMN.OverallOOB.n, 12)
        testthat::expect_named(res.HMN.OverallOOB.n, 
                               c("statistic", "parameter", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "val", 
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        testthat::expect_length(res.HMN.perm.OverallOOB.n, 12)
        testthat::expect_named(res.HMN.perm.OverallOOB.n,
                               c("statistic", "parameter", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "val",
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        
        # check p values in [0,1]
        testthat::expect_lte(res.HMN$p.value, 1)
        testthat::expect_gte(res.HMN$p.value, 0)
        testthat::expect_lte(res.HMN.perm$p.value, 1)
        testthat::expect_gte(res.HMN.perm$p.value, 0)
        testthat::expect_lte(res.HMN.OverallOOB$p.value, 1)
        testthat::expect_gte(res.HMN.OverallOOB$p.value, 0)
        testthat::expect_lte(res.HMN.perm.OverallOOB$p.value, 1)
        testthat::expect_gte(res.HMN.perm.OverallOOB$p.value, 0)
        testthat::expect_lte(res.HMN.n$p.value, 1)
        testthat::expect_gte(res.HMN.n$p.value, 0)
        testthat::expect_lte(res.HMN.perm.n$p.value, 1)
        testthat::expect_gte(res.HMN.perm.n$p.value, 0)
        testthat::expect_lte(res.HMN.OverallOOB.n$p.value, 1)
        testthat::expect_gte(res.HMN.OverallOOB.n$p.value, 0)
        testthat::expect_lte(res.HMN.perm.OverallOOB.n$p.value, 1)
        testthat::expect_gte(res.HMN.perm.OverallOOB.n$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.HMN$statistic))
        testthat::expect_false(is.na(res.HMN.perm$statistic))
        testthat::expect_false(is.na(res.HMN.OverallOOB$statistic))
        testthat::expect_false(is.na(res.HMN.perm.OverallOOB$statistic))
        testthat::expect_false(is.na(res.HMN.n$statistic))
        testthat::expect_false(is.na(res.HMN.perm.n$statistic))
        testthat::expect_false(is.na(res.HMN.OverallOOB.n$statistic))
        testthat::expect_false(is.na(res.HMN.perm.OverallOOB.n$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.HMN, "htest")
        testthat::expect_s3_class(res.HMN.perm, "htest")
        testthat::expect_s3_class(res.HMN.OverallOOB, "htest")
        testthat::expect_s3_class(res.HMN.perm.OverallOOB, "htest")
        testthat::expect_s3_class(res.HMN.n, "htest")
        testthat::expect_s3_class(res.HMN.perm.n, "htest")
        testthat::expect_s3_class(res.HMN.OverallOOB.n, "htest")
        testthat::expect_s3_class(res.HMN.perm.OverallOOB.n, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.HMN$statistic, res.hypoRF$obs,
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.perm$statistic, res.hypoRF.perm$obs, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.OverallOOB$statistic,
                               res.hypoRF.OverallOOB$obs,
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.perm.OverallOOB$statistic, 
                               res.hypoRF.perm.OverallOOB$obs,
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.n$statistic, res.hypoRF.n$obs, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.perm.n$statistic, res.hypoRF.perm.n$obs, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.OverallOOB.n$statistic, 
                               res.hypoRF.OverallOOB.n$obs,
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.perm.OverallOOB.n$statistic, 
                               res.hypoRF.perm.OverallOOB.n$obs,
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.HMN$p.value, res.hypoRF$pvalue,
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.perm$p.value, res.hypoRF.perm$pvalue, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.OverallOOB$p.value,
                               res.hypoRF.OverallOOB$pvalue, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.perm.OverallOOB$p.value, 
                               res.hypoRF.perm.OverallOOB$pvalue, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.n$p.value, res.hypoRF.n$pvalue,
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.perm.n$p.value, res.hypoRF.perm.n$pvalue,
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.OverallOOB.n$p.value,
                               res.hypoRF.OverallOOB.n$pvalue, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.HMN.perm.OverallOOB.n$p.value, 
                               res.hypoRF.perm.OverallOOB.n$pvalue,
                               check.attributes = FALSE)
      })
      
      
      res.HMN.1 <- DataSimilarity::HMN(X1[, 1, drop = FALSE], 
                                       as.data.frame(X2)[, 1, drop = FALSE],
                                       n.perm = 0, seed = i)
      res.HMN.perm.1 <- DataSimilarity::HMN(X1[, 1, drop = FALSE], 
                                            as.data.frame(X2)[, 1, drop = FALSE], 
                                            n.perm = 10, seed = i)
      res.HMN.OverallOOB.1 <- DataSimilarity::HMN(X1[, 1, drop = FALSE], 
                                                  as.data.frame(X2)[, 1, drop = FALSE],
                                                  n.perm = 0, statistic = "OverallOOB",
                                                  seed = i)
      res.HMN.perm.OverallOOB.1 <- DataSimilarity::HMN(X1[, 1, drop = FALSE], 
                                                       as.data.frame(X2)[, 1, drop = FALSE], 
                                                       n.perm = 10, 
                                                       statistic = "OverallOOB", 
                                                       seed = i)
      res.HMN.n.1 <- DataSimilarity::HMN(X1[, 1, drop = FALSE], 
                                         as.data.frame(X2)[, 1, drop = FALSE], 
                                         n.perm = 0, normal.approx = TRUE, seed = i)
      res.HMN.perm.n.1 <- DataSimilarity::HMN(X1[, 1, drop = FALSE], 
                                              as.data.frame(X2)[, 1, drop = FALSE], 
                                              n.perm = 10, normal.approx = TRUE, 
                                              seed = i)
      res.HMN.OverallOOB.n.1 <- DataSimilarity::HMN(X1[, 1, drop = FALSE], 
                                                    as.data.frame(X2)[, 1, drop = FALSE], 
                                                    n.perm = 0, 
                                                    statistic = "OverallOOB", 
                                                    normal.approx = TRUE, 
                                                    seed = i)
      res.HMN.perm.OverallOOB.n.1 <- DataSimilarity::HMN(X1[, 1, drop = FALSE], 
                                                         as.data.frame(X2)[, 1, drop = FALSE], 
                                                         n.perm = 10, 
                                                         statistic = "OverallOOB", 
                                                         normal.approx = TRUE, 
                                                         seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.HMN.1, 12)
        testthat::expect_named(res.HMN.1, 
                               c("statistic", "parameter", "p.value", "estimate", 
                                 "alternative", "method","data.name",  "val", "varest", 
                                 "importance.ranking", "importance.distribution",
                                 "cut.off"))
        testthat::expect_length(res.HMN.perm.1, 12)
        testthat::expect_named(res.HMN.perm.1,
                               c("statistic", "parameter", "p.value",  "estimate", 
                                 "alternative", "method",  "data.name", "val", 
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        testthat::expect_length(res.HMN.OverallOOB.1, 12)
        testthat::expect_named(res.HMN.OverallOOB.1, 
                               c("statistic", "parameter", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "val", 
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        testthat::expect_length(res.HMN.perm.OverallOOB.1, 12)
        testthat::expect_named(res.HMN.perm.OverallOOB.1, 
                               c("statistic", "parameter", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "val", 
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        
        testthat::expect_length(res.HMN.n.1, 12)
        testthat::expect_named(res.HMN.n.1,
                               c("statistic", "parameter", "p.value", "estimate",  
                                 "alternative",  "method", "data.name", "val",
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        testthat::expect_length(res.HMN.perm.n.1, 12)
        testthat::expect_named(res.HMN.perm.n.1, 
                               c("statistic", "parameter", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "val",
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        testthat::expect_length(res.HMN.OverallOOB.n.1, 12)
        testthat::expect_named(res.HMN.OverallOOB.n.1, 
                               c("statistic", "parameter", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "val", 
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        testthat::expect_length(res.HMN.perm.OverallOOB.n.1, 12)
        testthat::expect_named(res.HMN.perm.OverallOOB.n.1,
                               c("statistic", "parameter", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "val",
                                 "varest", "importance.ranking", 
                                 "importance.distribution", "cut.off"))
        
        # check p values in [0,1]
        testthat::expect_lte(res.HMN.1$p.value, 1)
        testthat::expect_gte(res.HMN.1$p.value, 0)
        testthat::expect_lte(res.HMN.perm.1$p.value, 1)
        testthat::expect_gte(res.HMN.perm.1$p.value, 0)
        testthat::expect_lte(res.HMN.OverallOOB.1$p.value, 1)
        testthat::expect_gte(res.HMN.OverallOOB.1$p.value, 0)
        testthat::expect_lte(res.HMN.perm.OverallOOB.1$p.value, 1)
        testthat::expect_gte(res.HMN.perm.OverallOOB.1$p.value, 0)
        testthat::expect_lte(res.HMN.n.1$p.value, 1)
        testthat::expect_gte(res.HMN.n.1$p.value, 0)
        testthat::expect_lte(res.HMN.perm.n.1$p.value, 1)
        testthat::expect_gte(res.HMN.perm.n.1$p.value, 0)
        testthat::expect_lte(res.HMN.OverallOOB.n.1$p.value, 1)
        testthat::expect_gte(res.HMN.OverallOOB.n.1$p.value, 0)
        testthat::expect_lte(res.HMN.perm.OverallOOB.n.1$p.value, 1)
        testthat::expect_gte(res.HMN.perm.OverallOOB.n.1$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.HMN.1$statistic))
        testthat::expect_false(is.na(res.HMN.perm.1$statistic))
        testthat::expect_false(is.na(res.HMN.OverallOOB.1$statistic))
        testthat::expect_false(is.na(res.HMN.perm.OverallOOB.1$statistic))
        testthat::expect_false(is.na(res.HMN.n.1$statistic))
        testthat::expect_false(is.na(res.HMN.perm.n.1$statistic))
        testthat::expect_false(is.na(res.HMN.OverallOOB.n.1$statistic))
        testthat::expect_false(is.na(res.HMN.perm.OverallOOB.n.1$statistic))
        # output should be htest object
        testthat::expect_s3_class(res.HMN.1, "htest")
        testthat::expect_s3_class(res.HMN.perm.1, "htest")
        testthat::expect_s3_class(res.HMN.OverallOOB.1, "htest")
        testthat::expect_s3_class(res.HMN.perm.OverallOOB.1, "htest")
        testthat::expect_s3_class(res.HMN.n.1, "htest")
        testthat::expect_s3_class(res.HMN.perm.n.1, "htest")
        testthat::expect_s3_class(res.HMN.OverallOOB.n.1, "htest")
        testthat::expect_s3_class(res.HMN.perm.OverallOOB.n.1, "htest")
      })
    }
  }
}

set.seed(0305)
testHMN(1)
