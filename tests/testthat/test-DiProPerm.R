testDiProPerm <- function(n.iter) {
  if(requireNamespace("DWDLargeR", quietly = TRUE) & 
     requireNamespace("e1071", quietly = TRUE) & 
     requireNamespace("pROC", quietly = TRUE) & 
     requireNamespace("Matrix", quietly = TRUE) &
     requireNamespace("rmvnorm", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10),
                             mean = runif(10, -2, 2))
      X2 <- as.data.frame(mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                                           sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10)))
      
      res.DiProPerm.dwd.md <- DataSimilarity::DiProPerm(X1, X2, n.perm = 0, seed = i)
      res.DiProPerm.dwd.t <- DataSimilarity::DiProPerm(X1, X2, n.perm = 0, seed = i,
                                                       stat.fun = DataSimilarity::tStat)
      res.DiProPerm.dwd.auc <- DataSimilarity::DiProPerm(X1, X2, n.perm = 0, seed = i,
                                                         stat.fun = DataSimilarity::AUC)
      res.DiProPerm.svm.md <- DataSimilarity::DiProPerm(X1, X2, n.perm = 0, seed = i, 
                                                        dipro.fun = DataSimilarity::svmProj)
      res.DiProPerm.svm.t <- DataSimilarity::DiProPerm(X1, X2, n.perm = 0, seed = i,
                                                       stat.fun = DataSimilarity::tStat, 
                                                       dipro.fun = DataSimilarity::svmProj)
      res.DiProPerm.svm.auc <- DataSimilarity::DiProPerm(X1, X2, n.perm = 0, seed = i,
                                                         stat.fun = DataSimilarity::AUC, 
                                                         dipro.fun = DataSimilarity::svmProj)
      
      res.DiProPerm.dwd.md.perm <- DataSimilarity::DiProPerm(X1, X2, n.perm = 10, 
                                                             seed = i)
      res.DiProPerm.dwd.t.perm <- DataSimilarity::DiProPerm(X1, X2, n.perm = 10, 
                                                            seed = i,
                                                            stat.fun = DataSimilarity::tStat)
      res.DiProPerm.dwd.auc.perm <- DataSimilarity::DiProPerm(X1, X2, n.perm = 10, 
                                                              seed = i,
                                                              stat.fun = DataSimilarity::AUC)
      res.DiProPerm.svm.md.perm <- DataSimilarity::DiProPerm(X1, X2, n.perm = 10, 
                                                             seed = i, 
                                                             dipro.fun = DataSimilarity::svmProj)
      res.DiProPerm.svm.t.perm <- DataSimilarity::DiProPerm(X1, X2, n.perm = 10, 
                                                            seed = i,
                                                            stat.fun = DataSimilarity::tStat, 
                                                            dipro.fun = DataSimilarity::svmProj)
      res.DiProPerm.svm.auc.perm <- DataSimilarity::DiProPerm(X1, X2, n.perm = 10, 
                                                              seed = i,
                                                              stat.fun = DataSimilarity::AUC, 
                                                              dipro.fun = DataSimilarity::svmProj)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.DiProPerm.dwd.md, 6)
        testthat::expect_named(res.DiProPerm.dwd.md, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.dwd.t, 6)
        testthat::expect_named(res.DiProPerm.dwd.t,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.dwd.auc, 6)
        testthat::expect_named(res.DiProPerm.dwd.auc,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.svm.md, 6)
        testthat::expect_named(res.DiProPerm.svm.md,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.svm.t, 6)
        testthat::expect_named(res.DiProPerm.svm.t, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.svm.auc, 6)
        testthat::expect_named(res.DiProPerm.svm.auc,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.dwd.md.perm, 6)
        testthat::expect_named(res.DiProPerm.dwd.md.perm,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.dwd.t.perm, 6)
        testthat::expect_named(res.DiProPerm.dwd.t.perm,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.dwd.auc.perm, 6)
        testthat::expect_named(res.DiProPerm.dwd.auc.perm,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.svm.md.perm, 6)
        testthat::expect_named(res.DiProPerm.svm.md.perm,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.svm.t.perm, 6)
        testthat::expect_named(res.DiProPerm.svm.t.perm,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.svm.auc.perm, 6)
        testthat::expect_named(res.DiProPerm.svm.auc.perm,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        
        # check p values in [0,1]
        testthat::expect_lte(res.DiProPerm.dwd.md.perm$p.value, 1)
        testthat::expect_gte(res.DiProPerm.dwd.md.perm$p.value, 0)
        testthat::expect_lte(res.DiProPerm.dwd.t.perm$p.value, 1)
        testthat::expect_gte(res.DiProPerm.dwd.t.perm$p.value, 0)
        testthat::expect_lte(res.DiProPerm.dwd.auc.perm$p.value, 1)
        testthat::expect_gte(res.DiProPerm.dwd.auc.perm$p.value, 0)
        testthat::expect_lte(res.DiProPerm.svm.md.perm$p.value, 1)
        testthat::expect_gte(res.DiProPerm.svm.md.perm$p.value, 0)
        testthat::expect_lte(res.DiProPerm.svm.t.perm$p.value, 1)
        testthat::expect_gte(res.DiProPerm.svm.t.perm$p.value, 0)
        testthat::expect_lte(res.DiProPerm.svm.auc.perm$p.value, 1)
        testthat::expect_gte(res.DiProPerm.svm.auc.perm$p.value, 0)
        # check approx. p value is NULL
        testthat::expect_null(res.DiProPerm.dwd.md$p.value)
        testthat::expect_null(res.DiProPerm.dwd.t$p.value)
        testthat::expect_null(res.DiProPerm.dwd.auc$p.value)
        testthat::expect_null(res.DiProPerm.svm.md$p.value)
        testthat::expect_null(res.DiProPerm.svm.t$p.value)
        testthat::expect_null(res.DiProPerm.svm.auc$p.value)
        # statistic is not NA
        testthat::expect_false(is.na(res.DiProPerm.dwd.md$statistic))
        testthat::expect_false(is.na(res.DiProPerm.dwd.t$statistic))
        testthat::expect_false(is.na(res.DiProPerm.dwd.auc$statistic))
        testthat::expect_false(is.na(res.DiProPerm.svm.md$statistic))
        testthat::expect_false(is.na(res.DiProPerm.svm.t$statistic))
        testthat::expect_false(is.na(res.DiProPerm.svm.auc$statistic))
        
        testthat::expect_false(is.na(res.DiProPerm.dwd.md.perm$statistic))
        testthat::expect_false(is.na(res.DiProPerm.dwd.t.perm$statistic))
        testthat::expect_false(is.na(res.DiProPerm.dwd.auc.perm$statistic))
        testthat::expect_false(is.na(res.DiProPerm.svm.md.perm$statistic))
        testthat::expect_false(is.na(res.DiProPerm.svm.t.perm$statistic))
        testthat::expect_false(is.na(res.DiProPerm.svm.auc.perm$statistic))
        # output should be htest
        testthat::expect_s3_class(res.DiProPerm.dwd.md, "htest")
        testthat::expect_s3_class(res.DiProPerm.dwd.t, "htest")
        testthat::expect_s3_class(res.DiProPerm.dwd.auc, "htest")
        testthat::expect_s3_class(res.DiProPerm.svm.md, "htest")
        testthat::expect_s3_class(res.DiProPerm.svm.t, "htest")
        testthat::expect_s3_class(res.DiProPerm.svm.auc, "htest")
        testthat::expect_s3_class(res.DiProPerm.dwd.md.perm, "htest")
        testthat::expect_s3_class(res.DiProPerm.dwd.t.perm, "htest")
        testthat::expect_s3_class(res.DiProPerm.dwd.auc.perm, "htest")
        testthat::expect_s3_class(res.DiProPerm.svm.md.perm, "htest")
        testthat::expect_s3_class(res.DiProPerm.svm.t.perm, "htest")
        testthat::expect_s3_class(res.DiProPerm.svm.auc.perm, "htest")
      })
      # cannot test results against DiProPerm function from diproperm package as 
      # these are not reproducable
      res.DiProPerm.dwd.md.1 <- DataSimilarity::DiProPerm(X1[, 1, drop = FALSE], 
                                                          X2[, 1, drop = FALSE],
                                                          n.perm = 0, seed = i)
      res.DiProPerm.dwd.t.1 <- DataSimilarity::DiProPerm(X1[, 1, drop = FALSE], 
                                                         X2[, 1, drop = FALSE], 
                                                         n.perm = 0, seed = i,
                                                         stat.fun = DataSimilarity::tStat)
      res.DiProPerm.dwd.auc.1 <- DataSimilarity::DiProPerm(X1[, 1, drop = FALSE], 
                                                           X2[, 1, drop = FALSE], 
                                                           n.perm = 0, seed = i,
                                                           stat.fun = DataSimilarity::AUC)
      res.DiProPerm.svm.md.1 <- DataSimilarity::DiProPerm(X1[, 1, drop = FALSE], 
                                                          X2[, 1, drop = FALSE], 
                                                          n.perm = 0, seed = i, 
                                                          dipro.fun = DataSimilarity::svmProj)
      res.DiProPerm.svm.t.1 <- DataSimilarity::DiProPerm(X1[, 1, drop = FALSE], 
                                                         X2[, 1, drop = FALSE], 
                                                         n.perm = 0, seed = i,
                                                         stat.fun = DataSimilarity::tStat, 
                                                         dipro.fun = DataSimilarity::svmProj)
      res.DiProPerm.svm.auc.1 <- DataSimilarity::DiProPerm(X1[, 1, drop = FALSE], 
                                                           X2[, 1, drop = FALSE], 
                                                           n.perm = 0, seed = i,
                                                           stat.fun = DataSimilarity::AUC, 
                                                           dipro.fun = DataSimilarity::svmProj)
      
      res.DiProPerm.dwd.md.perm.1 <- DataSimilarity::DiProPerm(X1[, 1, drop = FALSE], 
                                                               X2[, 1, drop = FALSE], 
                                                               n.perm = 3, 
                                                               seed = i)
      res.DiProPerm.dwd.t.perm.1 <- DataSimilarity::DiProPerm(X1[, 1, drop = FALSE], 
                                                              X2[, 1, drop = FALSE], 
                                                              n.perm = 3, 
                                                              seed = i,
                                                              stat.fun = DataSimilarity::tStat)
      res.DiProPerm.dwd.auc.perm.1 <- DataSimilarity::DiProPerm(X1[, 1, drop = FALSE], 
                                                                X2[, 1, drop = FALSE], 
                                                                n.perm = 3, 
                                                                seed = i,
                                                                stat.fun = DataSimilarity::AUC)
      res.DiProPerm.svm.md.perm.1 <- DataSimilarity::DiProPerm(X1[, 1, drop = FALSE], 
                                                               X2[, 1, drop = FALSE], 
                                                               n.perm = 3, 
                                                               seed = i, 
                                                               dipro.fun = DataSimilarity::svmProj)
      res.DiProPerm.svm.t.perm.1 <- DataSimilarity::DiProPerm(X1[, 1, drop = FALSE], 
                                                              X2[, 1, drop = FALSE], 
                                                              n.perm = 3, 
                                                              seed = i,
                                                              stat.fun = DataSimilarity::tStat, 
                                                              dipro.fun = DataSimilarity::svmProj)
      res.DiProPerm.svm.auc.perm.1 <- DataSimilarity::DiProPerm(X1[, 1, drop = FALSE], 
                                                                X2[, 1, drop = FALSE], 
                                                                n.perm = 3, 
                                                                seed = i,
                                                                stat.fun = DataSimilarity::AUC, 
                                                                dipro.fun = DataSimilarity::svmProj)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.DiProPerm.dwd.md.1, 6)
        testthat::expect_named(res.DiProPerm.dwd.md.1, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.dwd.t.1, 6)
        testthat::expect_named(res.DiProPerm.dwd.t.1,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.dwd.auc.1, 6)
        testthat::expect_named(res.DiProPerm.dwd.auc.1,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.svm.md.1, 6)
        testthat::expect_named(res.DiProPerm.svm.md.1,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.svm.t.1, 6)
        testthat::expect_named(res.DiProPerm.svm.t.1, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.svm.auc.1, 6)
        testthat::expect_named(res.DiProPerm.svm.auc.1,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.dwd.md.perm.1, 6)
        testthat::expect_named(res.DiProPerm.dwd.md.perm.1,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.dwd.t.perm.1, 6)
        testthat::expect_named(res.DiProPerm.dwd.t.perm.1,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.dwd.auc.perm.1, 6)
        testthat::expect_named(res.DiProPerm.dwd.auc.perm.1,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.svm.md.perm.1, 6)
        testthat::expect_named(res.DiProPerm.svm.md.perm.1,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.svm.t.perm.1, 6)
        testthat::expect_named(res.DiProPerm.svm.t.perm.1,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        testthat::expect_length(res.DiProPerm.svm.auc.perm.1, 6)
        testthat::expect_named(res.DiProPerm.svm.auc.perm.1,  
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name"))
        
        # check p values in [0,1]
        testthat::expect_lte(res.DiProPerm.dwd.md.perm.1$p.value, 1)
        testthat::expect_gte(res.DiProPerm.dwd.md.perm.1$p.value, 0)
        testthat::expect_lte(res.DiProPerm.dwd.t.perm.1$p.value, 1)
        testthat::expect_gte(res.DiProPerm.dwd.t.perm.1$p.value, 0)
        testthat::expect_lte(res.DiProPerm.dwd.auc.perm.1$p.value, 1)
        testthat::expect_gte(res.DiProPerm.dwd.auc.perm.1$p.value, 0)
        testthat::expect_lte(res.DiProPerm.svm.md.perm.1$p.value, 1)
        testthat::expect_gte(res.DiProPerm.svm.md.perm.1$p.value, 0)
        testthat::expect_lte(res.DiProPerm.svm.t.perm.1$p.value, 1)
        testthat::expect_gte(res.DiProPerm.svm.t.perm.1$p.value, 0)
        testthat::expect_lte(res.DiProPerm.svm.auc.perm.1$p.value, 1)
        testthat::expect_gte(res.DiProPerm.svm.auc.perm.1$p.value, 0)
        # check approx. p value is NULL
        testthat::expect_null(res.DiProPerm.dwd.md.1$p.value)
        testthat::expect_null(res.DiProPerm.dwd.t.1$p.value)
        testthat::expect_null(res.DiProPerm.dwd.auc.1$p.value)
        testthat::expect_null(res.DiProPerm.svm.md.1$p.value)
        testthat::expect_null(res.DiProPerm.svm.t.1$p.value)
        testthat::expect_null(res.DiProPerm.svm.auc.1$p.value)
        # statistic is not NA
        testthat::expect_false(is.na(res.DiProPerm.dwd.md.1$statistic))
        testthat::expect_false(is.na(res.DiProPerm.dwd.t.1$statistic))
        testthat::expect_false(is.na(res.DiProPerm.dwd.auc.1$statistic))
        testthat::expect_false(is.na(res.DiProPerm.svm.md.1$statistic))
        testthat::expect_false(is.na(res.DiProPerm.svm.t.1$statistic))
        testthat::expect_false(is.na(res.DiProPerm.svm.auc.1$statistic))
        
        testthat::expect_false(is.na(res.DiProPerm.dwd.md.perm.1$statistic))
        testthat::expect_false(is.na(res.DiProPerm.dwd.t.perm.1$statistic))
        testthat::expect_false(is.na(res.DiProPerm.dwd.auc.perm.1$statistic))
        testthat::expect_false(is.na(res.DiProPerm.svm.md.perm.1$statistic))
        testthat::expect_false(is.na(res.DiProPerm.svm.t.perm.1$statistic))
        testthat::expect_false(is.na(res.DiProPerm.svm.auc.perm.1$statistic))
        # output should be htest
        testthat::expect_s3_class(res.DiProPerm.dwd.md.1, "htest")
        testthat::expect_s3_class(res.DiProPerm.dwd.t.1, "htest")
        testthat::expect_s3_class(res.DiProPerm.dwd.auc.1, "htest")
        testthat::expect_s3_class(res.DiProPerm.svm.md.1, "htest")
        testthat::expect_s3_class(res.DiProPerm.svm.t.1, "htest")
        testthat::expect_s3_class(res.DiProPerm.svm.auc.1, "htest")
        testthat::expect_s3_class(res.DiProPerm.dwd.md.perm.1, "htest")
        testthat::expect_s3_class(res.DiProPerm.dwd.t.perm.1, "htest")
        testthat::expect_s3_class(res.DiProPerm.dwd.auc.perm.1, "htest")
        testthat::expect_s3_class(res.DiProPerm.svm.md.perm.1, "htest")
        testthat::expect_s3_class(res.DiProPerm.svm.t.perm.1, "htest")
        testthat::expect_s3_class(res.DiProPerm.svm.auc.perm.1, "htest")
      })
    }
  }
}

set.seed(0305)
testDiProPerm(1)
