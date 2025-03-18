testNKTandGGRL <- function(n.iter) {
  if(requireNamespace("rpart", quietly = TRUE) & requireNamespace("e1071", quietly = TRUE) &
     requireNamespace("rmvnorm", quietly = TRUE)) {
    for(i in 1:n.iter){
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = diag(10))
      X2 <- mvtnorm::rmvnorm(100, mean = rep(c(0.1, -0.1), 5), sigma = diag(10))
      
      y1 <- rbinom(100, 1, 1 / (1 + exp(1 - X1 %*% rep(0.5, 10))))
      y2 <- rbinom(100, 1, 1 / (1 + exp(1 - X2 %*% rep(0.7, 10))))
      
      X1 <- data.frame(y = factor(y1, levels = 0:1), X1)
      X2 <- data.frame(y = factor(y2, levels = 0:1), X2)
      
      tree1 <- rpart::rpart(y ~ ., data = X1, method = "class")
      tree2 <- rpart::rpart(y ~ ., data = X2, method = "class")
      
      tree1.tune <- e1071::best.rpart(y ~ ., data = X1, 
                                      tunecontrol = e1071::tune.control(sampling = "cross", 
                                                                        cross = 5, random = 100), 
                                      minsplit = 2^(1:7), minbucket = 2^(0:6), 
                                      cp = 10^seq(-4, -1, by = 0.001))
      tree2.tune <- e1071::best.rpart(y ~ ., data = X2, 
                                      tunecontrol = e1071::tune.control(sampling = "cross", 
                                                                        cross = 5, random = 100), 
                                      minsplit = 2^(1:7), minbucket = 2^(0:6), 
                                      cp = 10^seq(-4, -1, by = 0.001))
      
      parti1 <- DataSimilarity:::findPartition(tree1, X1, X2)
      parti2 <- DataSimilarity:::findPartition(tree2, X1, X2)
      
      parti1.tune <- DataSimilarity:::findPartition(tree1.tune, X1, X2)
      parti2.tune <- DataSimilarity:::findPartition(tree2.tune, X1, X2)
      
      sec.parti <- DataSimilarity:::intersectPartitions(parti1, parti2)
      sec.parti.tune <- DataSimilarity:::intersectPartitions(parti1.tune, parti2.tune)
      
      GCR.tune <- DataSimilarity:::calculateGCR(X1, X2)
      GCR.no.tune <- DataSimilarity:::calculateGCR(X1, X2, tune = FALSE)
      
      P1.joint <- colSums(DataSimilarity:::calcP(sec.parti$parti, X1, n = "joint"))
      P2.joint <- colSums(DataSimilarity:::calcP(sec.parti$parti, X2, n = "joint"))
      
      P1.joint.tune <- colSums(DataSimilarity:::calcP(GCR.tune$res.intersect$parti, X1, n = "joint"))
      P2.joint.tune <- colSums(DataSimilarity:::calcP(GCR.tune$res.intersect$parti, X2, n = "joint"))
      
      P1.joint.no.tune <- colSums(DataSimilarity:::calcP(GCR.no.tune$res.intersect$parti, X1, n = "joint"))
      P2.joint.no.tune <- colSums(DataSimilarity:::calcP(GCR.no.tune$res.intersect$parti, X2, n = "joint"))
      
      P1.cond <- DataSimilarity:::calcP(parti1, X1, "conditional")
      P2.cond <- DataSimilarity:::calcP(parti2, X2, "conditional")
      
      P1.cond.tune <- DataSimilarity:::calcP(GCR.tune$parti1, X1, "conditional")
      P2.cond.tune <- DataSimilarity:::calcP(GCR.tune$parti2, X2, "conditional")
      
      P1.cond.no.tune <- DataSimilarity:::calcP(GCR.no.tune$parti1, X1, "conditional")
      P2.cond.no.tune <- DataSimilarity:::calcP(GCR.no.tune$parti2, X2, "conditional")
      
      res.NKT.1.tune <- DataSimilarity::NKT(X1, X2, target1 = "y", target2 = "y", method = 1, n.eval = 3, k = 2, seed = i)
      res.NKT.2.tune <- DataSimilarity::NKT(X1, X2, target1 = "y", target2 = "y", method = 2, n.eval = 3, k = 2, seed = i)
      res.NKT.3.tune <- DataSimilarity::NKT(X1, X2, target1 = "y", target2 = "y", method = 3, n.eval = 3, k = 2, seed = i)
      
      res.NKT.1.no.tune <- DataSimilarity::NKT(X1, X2, target1 = "y", target2 = "y", method = 1, 
                                               tune = FALSE, seed = i)
      res.NKT.2.no.tune <- DataSimilarity::NKT(X1, X2, target1 = "y", target2 = "y", method = 2, 
                                               tune = FALSE, seed = i)
      res.NKT.3.no.tune <- DataSimilarity::NKT(X1, X2, target1 = "y", target2 = "y", method = 3, 
                                               tune = FALSE, seed = i)
      
      
      res.GGRL.tune.sum.f.a <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", n.eval = 3, k = 2, seed = i)
      
      res.GGRL.no.tune.sum.f.a <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", tune = FALSE, seed = i)
      
      res.GGRL.tune.max.f.a <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", agg.fun = max, n.eval = 3, k = 2, seed = i)
      
      res.GGRL.no.tune.max.f.a <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", 
                                                       tune = FALSE, agg.fun = max, seed = i)
      
      res.GGRL.tune.sum.f.s <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", diff.fun = DataSimilarity:::f.s, n.eval = 3, k = 2, seed = i)
      
      res.GGRL.no.tune.sum.f.s <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y",  
                                                       tune = FALSE, diff.fun = DataSimilarity:::f.s, seed = i)
      
      res.GGRL.tune.max.f.s <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", n.eval = 3, k = 2, 
                                                    diff.fun = DataSimilarity:::f.s, agg.fun = max, seed = i)
      
      res.GGRL.no.tune.max.f.s <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", tune = FALSE, 
                                                       diff.fun = DataSimilarity:::f.s, agg.fun = max, seed = i)
      
      
      res.GGRL.tune.sum.f.a.perm <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", n.perm = 10, n.eval = 3, k = 2, seed = i)
      
      res.GGRL.no.tune.sum.f.a.perm <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", tune = FALSE, n.perm = 10, seed = i)
      
      res.GGRL.tune.max.f.a.perm <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", agg.fun = max, n.perm = 10, n.eval = 3, k = 2, seed = i)
      
      res.GGRL.no.tune.max.f.a.perm <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", 
                                                            tune = FALSE, agg.fun = max, n.perm = 10, seed = i)
      
      res.GGRL.tune.sum.f.s.perm <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", diff.fun = DataSimilarity:::f.s, n.perm = 10, n.eval = 3, k = 2, seed = i)
      
      res.GGRL.no.tune.sum.f.s.perm <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", n.eval = 3, k = 2,  
                                                            tune = FALSE, diff.fun = DataSimilarity:::f.s, n.perm = 10, seed = i)
      
      res.GGRL.tune.max.f.s.perm <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", n.eval = 3, k = 2, 
                                                         diff.fun = DataSimilarity:::f.s, agg.fun = max, n.perm = 10, seed = i)
      
      res.GGRL.no.tune.max.f.s.perm <- DataSimilarity::GGRL(X1, X2, target1 = "y", target2 = "y", tune = FALSE, 
                                                            diff.fun = DataSimilarity:::f.s, agg.fun = max, n.perm = 10, seed = i)
      
      
      checkMin <- function(varname, part, splits, mins) {
        round(part[part$var == varname, "min"], 5) %in% 
          round(c(splits[, "index"][names(splits[, "index"]) == varname], 
                  mins[names(mins) == varname]), 5)
      }
      
      checkMax <- function(varname, part, splits, maxs) {
        round(part[part$var == varname, "max"], 5) %in% 
          round(c(splits[, "index"][names(splits[, "index"]) == varname], 
                  maxs[names(maxs) == varname]), 5)
      }
      
      mins <- sapply(rbind(X1, X2)[, -1], min)
      maxs <- sapply(rbind(X1, X2)[, -1], max)
      
      checkLimits <- function(parti, splits) {
        testthat::expect_true(all(sapply(parti, function(p) checkMin("X1", p, splits, mins))))
        testthat::expect_true(all(sapply(parti, function(p) checkMin("X2", p, splits, mins))))
        testthat::expect_true(all(sapply(parti, function(p) checkMin("X3", p, splits, mins))))
        testthat::expect_true(all(sapply(parti, function(p) checkMin("X4", p, splits, mins))))
        testthat::expect_true(all(sapply(parti, function(p) checkMin("X5", p, splits, mins))))
        testthat::expect_true(all(sapply(parti, function(p) checkMin("X6", p, splits, mins))))
        testthat::expect_true(all(sapply(parti, function(p) checkMin("X7", p, splits, mins))))
        testthat::expect_true(all(sapply(parti, function(p) checkMin("X8", p, splits, mins))))
        testthat::expect_true(all(sapply(parti, function(p) checkMin("X8", p, splits, mins))))
        testthat::expect_true(all(sapply(parti, function(p) checkMin("X10", p, splits, mins))))
        
        testthat::expect_true(all(sapply(parti, function(p) checkMax("X1", p, splits, maxs))))
        testthat::expect_true(all(sapply(parti, function(p) checkMax("X2", p, splits, maxs))))
        testthat::expect_true(all(sapply(parti, function(p) checkMax("X3", p, splits, maxs))))
        testthat::expect_true(all(sapply(parti, function(p) checkMax("X4", p, splits, maxs))))
        testthat::expect_true(all(sapply(parti, function(p) checkMax("X5", p, splits, maxs))))
        testthat::expect_true(all(sapply(parti, function(p) checkMax("X6", p, splits, maxs))))
        testthat::expect_true(all(sapply(parti, function(p) checkMax("X7", p, splits, maxs))))
        testthat::expect_true(all(sapply(parti, function(p) checkMax("X8", p, splits, maxs))))
        testthat::expect_true(all(sapply(parti, function(p) checkMax("X8", p, splits, maxs))))
        testthat::expect_true(all(sapply(parti, function(p) checkMax("X10", p, splits, maxs))))
      }
      
      testthat::test_that("helper functions", {
        # check if resulting partition has length equal to number of leaves 
        # and if each list element has number of rows equal to number of variables 
        # and if minima and maxima are matching minima and maxima of data or any of 
        # the split points
        testthat::expect_length(parti1, sum(tree1$frame$var == "<leaf>"))
        testthat::expect_length(parti2, sum(tree2$frame$var == "<leaf>"))
        testthat::expect_true(all(sapply(parti1, nrow) == ncol(X1) - 1))
        testthat::expect_true(all(sapply(parti2, nrow) == ncol(X2) - 1))
        checkLimits(parti1, tree1$splits)
        checkLimits(parti2, tree2$splits)
        
        testthat::expect_length(sec.parti$parti, nrow(sec.parti$combis))
        testthat::expect_true(all(sapply(sec.parti$parti, nrow) == ncol(X1) - 1))
        checkLimits(parti1, rbind(tree1$splits, tree2$splits))
        
        testthat::expect_true(all(sapply(GCR.tune$parti1, nrow) == ncol(X1) - 1))
        testthat::expect_true(all(sapply(GCR.tune$parti2, nrow) == ncol(X2) - 1))
        
        testthat::expect_length(sec.parti$parti, nrow(sec.parti$combis))
        testthat::expect_true(all(sapply(sec.parti$parti, nrow) == ncol(X1) - 1))
        checkLimits(sec.parti$parti, rbind(tree1$splits, tree2$splits))
        
        testthat::expect_length(parti1.tune, sum(tree1.tune$frame$var == "<leaf>"))
        testthat::expect_length(parti2.tune, sum(tree2.tune$frame$var == "<leaf>"))
        testthat::expect_true(all(sapply(parti1.tune, nrow) == ncol(X1) - 1))
        testthat::expect_true(all(sapply(parti2.tune, nrow) == ncol(X2) - 1))
        checkLimits(parti1.tune, tree1.tune$splits)
        checkLimits(parti2.tune, tree2.tune$splits)
        
        testthat::expect_length(sec.parti.tune$parti, nrow(sec.parti.tune$combis))
        testthat::expect_true(all(sapply(sec.parti.tune$parti, nrow) == ncol(X1) - 1))
        checkLimits(sec.parti.tune$parti, rbind(tree1.tune$splits, tree2.tune$splits))
        
        # check if sums over calcP are equal to one
        testthat::expect_equal(sum(P1.joint), 1)
        testthat::expect_equal(sum(P2.joint), 1)
        
        testthat::expect_equal(sum(P1.joint.tune), 1)
        testthat::expect_equal(sum(P2.joint.tune), 1)
        
        testthat::expect_equal(sum(P1.joint.no.tune), 1)
        testthat::expect_equal(sum(P2.joint.no.tune), 1)
        
        testthat::expect_true(all(colSums(P1.cond) == 1))
        testthat::expect_true(all(colSums(P2.cond) == 1))
        
        testthat::expect_true(all(colSums(P1.cond.tune) == 1))
        testthat::expect_true(all(colSums(P2.cond.tune) == 1))
        
        testthat::expect_true(all(colSums(P1.cond.no.tune) == 1))
        testthat::expect_true(all(colSums(P2.cond.no.tune) == 1))
      })
      
      testthat::test_that("output NKT", {
        # check output types for NKT + statistic in [0, 1]
        # check length and names of output
        testthat::expect_length(res.NKT.1.tune, 7)
        testthat::expect_named(res.NKT.1.tune, c("statistic", "p.value", "estimate", 
                                                 "alternative", "method", "data.name", "method"))
        testthat::expect_length(res.NKT.2.tune, 7)
        testthat::expect_named(res.NKT.2.tune, c("statistic", "p.value", "estimate", 
                                                 "alternative", "method", "data.name", "method"))
        testthat::expect_length(res.NKT.3.tune, 7)
        testthat::expect_named(res.NKT.3.tune, c("statistic", "p.value", "estimate", 
                                                 "alternative", "method", "data.name", "method"))
        testthat::expect_length(res.NKT.1.no.tune, 7)
        testthat::expect_named(res.NKT.1.no.tune, c("statistic", "p.value", "estimate", 
                                                    "alternative", "method", "data.name", "method"))
        testthat::expect_length(res.NKT.2.no.tune, 7)
        testthat::expect_named(res.NKT.2.no.tune, c("statistic", "p.value", "estimate", 
                                                    "alternative", "method", "data.name", "method"))
        testthat::expect_length(res.NKT.3.no.tune, 7)
        testthat::expect_named(res.NKT.3.no.tune, c("statistic", "p.value", "estimate", 
                                                    "alternative", "method", "data.name", "method"))
        # check approx.and perm p value is NA
        testthat::expect_true(is.null(res.NKT.1.tune$p.value))
        testthat::expect_true(is.null(res.NKT.1.tune$p.value))
        testthat::expect_true(is.null(res.NKT.2.tune$p.value))
        testthat::expect_true(is.null(res.NKT.2.tune$p.value))
        testthat::expect_true(is.null(res.NKT.3.tune$p.value))
        testthat::expect_true(is.null(res.NKT.3.tune$p.value))
        testthat::expect_true(is.null(res.NKT.1.no.tune$p.value))
        testthat::expect_true(is.null(res.NKT.1.no.tune$p.value))
        testthat::expect_true(is.null(res.NKT.2.no.tune$p.value))
        testthat::expect_true(is.null(res.NKT.2.no.tune$p.value))
        testthat::expect_true(is.null(res.NKT.3.no.tune$p.value))
        testthat::expect_true(is.null(res.NKT.3.no.tune$p.value))
        # statistic is not NA
        testthat::expect_false(is.na(res.NKT.1.tune$statistic))
        testthat::expect_false(is.na(res.NKT.2.tune$statistic))
        testthat::expect_false(is.na(res.NKT.3.tune$statistic))
        testthat::expect_false(is.na(res.NKT.1.no.tune$statistic))
        testthat::expect_false(is.na(res.NKT.2.no.tune$statistic))
        testthat::expect_false(is.na(res.NKT.3.no.tune$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.NKT.1.tune, "htest")
        testthat::expect_s3_class(res.NKT.2.tune, "htest")
        testthat::expect_s3_class(res.NKT.3.tune, "htest")
        testthat::expect_s3_class(res.NKT.1.no.tune, "htest")
        testthat::expect_s3_class(res.NKT.2.no.tune, "htest")
        testthat::expect_s3_class(res.NKT.3.no.tune, "htest")
        # statistic should lie in [0,1]
        testthat::expect_gte(res.NKT.1.tune$statistic, 0)
        testthat::expect_lte(res.NKT.1.tune$statistic, 1)
        testthat::expect_gte(res.NKT.2.tune$statistic, 0)
        testthat::expect_lte(res.NKT.2.tune$statistic, 1)
        testthat::expect_gte(res.NKT.3.tune$statistic, 0)
        testthat::expect_lte(res.NKT.3.tune$statistic, 1)
        
        testthat::expect_gte(res.NKT.1.no.tune$statistic, 0)
        testthat::expect_lte(res.NKT.1.no.tune$statistic, 1)
        testthat::expect_gte(res.NKT.2.no.tune$statistic, 0)
        testthat::expect_lte(res.NKT.2.no.tune$statistic, 1)
        testthat::expect_gte(res.NKT.3.no.tune$statistic, 0)
        testthat::expect_lte(res.NKT.3.no.tune$statistic, 1)
      })
      
      testthat::test_that("output GGRL", {
        # check output types for NKT + statistic in [0, 1]
        # check length and names of output
        testthat::expect_length(res.GGRL.tune.sum.f.a, 8)
        testthat::expect_named(res.GGRL.tune.sum.f.a, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.sum.f.a, 8)
        testthat::expect_named(res.GGRL.no.tune.sum.f.a, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.tune.max.f.a, 8)
        testthat::expect_named(res.GGRL.tune.max.f.a, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.max.f.a, 8)
        testthat::expect_named(res.GGRL.no.tune.max.f.a, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.tune.sum.f.s, 8)
        testthat::expect_named(res.GGRL.tune.sum.f.s, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.sum.f.s, 8)
        testthat::expect_named(res.GGRL.no.tune.sum.f.s, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.tune.max.f.s, 8)
        testthat::expect_named(res.GGRL.tune.max.f.s, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.max.f.s, 8)
        testthat::expect_named(res.GGRL.no.tune.max.f.s, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        
        testthat::expect_length(res.GGRL.tune.sum.f.a.perm, 8)
        testthat::expect_named(res.GGRL.tune.sum.f.a.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.sum.f.a.perm, 8)
        testthat::expect_named(res.GGRL.no.tune.sum.f.a.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.tune.max.f.a.perm, 8)
        testthat::expect_named(res.GGRL.tune.max.f.a.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.max.f.a.perm, 8)
        testthat::expect_named(res.GGRL.no.tune.max.f.a.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.tune.sum.f.s.perm, 8)
        testthat::expect_named(res.GGRL.tune.sum.f.s.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.sum.f.s.perm, 8)
        testthat::expect_named(res.GGRL.no.tune.sum.f.s.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.tune.max.f.s.perm, 8)
        testthat::expect_named(res.GGRL.tune.max.f.s.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.max.f.s.perm, 8)
        testthat::expect_named(res.GGRL.no.tune.max.f.s.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        
        # check approx.and perm p value is NA
        testthat::expect_true(is.na(res.GGRL.tune.sum.f.a$p.value))
        testthat::expect_true(is.na(res.GGRL.no.tune.sum.f.a$p.value))
        testthat::expect_true(is.na(res.GGRL.tune.max.f.a$p.value))
        testthat::expect_true(is.na(res.GGRL.no.tune.max.f.a$p.value))
        testthat::expect_true(is.na(res.GGRL.tune.sum.f.s$p.value))
        testthat::expect_true(is.na(res.GGRL.no.tune.sum.f.s$p.value))
        testthat::expect_true(is.na(res.GGRL.tune.max.f.s$p.value))
        testthat::expect_true(is.na(res.GGRL.no.tune.max.f.s$p.value))
        
        testthat::expect_gte(res.GGRL.tune.sum.f.a.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.no.tune.sum.f.a.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.tune.max.f.a.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.no.tune.max.f.a.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.tune.sum.f.s.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.no.tune.sum.f.s.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.tune.max.f.s.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.no.tune.max.f.s.perm$p.value, 0)
        
        testthat::expect_lte(res.GGRL.tune.sum.f.a.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.no.tune.sum.f.a.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.tune.max.f.a.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.no.tune.max.f.a.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.tune.sum.f.s.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.no.tune.sum.f.s.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.tune.max.f.s.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.no.tune.max.f.s.perm$p.value, 1)
        
        # statistic is not NA
        testthat::expect_false(is.na(res.GGRL.tune.sum.f.a$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.sum.f.a$statistic))
        testthat::expect_false(is.na(res.GGRL.tune.max.f.a$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.max.f.a$statistic))
        testthat::expect_false(is.na(res.GGRL.tune.sum.f.s$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.sum.f.s$statistic))
        testthat::expect_false(is.na(res.GGRL.tune.max.f.s$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.max.f.s$statistic))
        
        testthat::expect_false(is.na(res.GGRL.tune.sum.f.a.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.sum.f.a.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.tune.max.f.a.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.max.f.a.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.tune.sum.f.s.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.sum.f.s.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.tune.max.f.s.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.max.f.s.perm$statistic))
        
        # output should be numeric
        testthat::expect_s3_class(res.GGRL.tune.sum.f.a, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.sum.f.a, "htest")
        testthat::expect_s3_class(res.GGRL.tune.max.f.a, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.max.f.a, "htest")
        testthat::expect_s3_class(res.GGRL.tune.sum.f.s, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.sum.f.s, "htest")
        testthat::expect_s3_class(res.GGRL.tune.max.f.s, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.max.f.s, "htest")
        
        testthat::expect_s3_class(res.GGRL.tune.sum.f.a.perm, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.sum.f.a.perm, "htest")
        testthat::expect_s3_class(res.GGRL.tune.max.f.a.perm, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.max.f.a.perm, "htest")
        testthat::expect_s3_class(res.GGRL.tune.sum.f.s.perm, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.sum.f.s.perm, "htest")
        testthat::expect_s3_class(res.GGRL.tune.max.f.s.perm, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.max.f.s.perm, "htest")
      })
    }
  }
}

set.seed(0305)
testNKTandGGRL(10)

testGGRLCat <- function(n.iter) {
  if(requireNamespace("rpart", quietly = TRUE) & requireNamespace("e1071", quietly = TRUE)) {
    for(i in 1:n.iter){
      set.seed(i)
      max1 <- sample(2:5, 1)
      max2 <- sample(2:5, 1)
      max3 <- sample(2:5, 1)
      X1 <- data.frame(X1 = factor(sample(letters[1:max1], 1000, TRUE)), 
                       X2 = factor(sample(letters[1:max2], 1000, TRUE)), 
                       X3 = factor(sample(letters[1:max3], 1000, TRUE)), 
                       y = sample(0:1, 100, TRUE))
      X2 <- data.frame(X1 = factor(sample(letters[1:max1], 1000, TRUE, 1:max1)), 
                       X2 = factor(sample(letters[1:max2], 1000, TRUE, 1:max2)), 
                       X3 = factor(sample(letters[1:max3], 1000, TRUE, 1:max3)), 
                       y = sample(0:1, 100, TRUE))
      
      tree1 <- rpart::rpart(y ~ ., data = X1, method = "class")
      tree2 <- rpart::rpart(y ~ ., data = X2, method = "class")
      
      tree1.tune <- e1071::best.rpart(y ~ ., data = X1, 
                                      tunecontrol = e1071::tune.control(sampling = "cross", 
                                                                        cross = 5, random = 3), 
                                      minsplit = 2^(1:7), minbucket = 2^(0:6), 
                                      cp = 10^seq(-4, -1, by = 0.001))
      tree2.tune <- e1071::best.rpart(y ~ ., data = X2, 
                                      tunecontrol = e1071::tune.control(sampling = "cross", 
                                                                        cross = 5, random = 3), 
                                      minsplit = 2^(1:7), minbucket = 2^(0:6), 
                                      cp = 10^seq(-4, -1, by = 0.001))
      
      parti1 <- DataSimilarity:::findPartitionCat(tree1, X1, X2)
      parti2 <- DataSimilarity:::findPartitionCat(tree2, X1, X2)
      
      parti1.tune <- DataSimilarity:::findPartitionCat(tree1.tune, X1, X2)
      parti2.tune <- DataSimilarity:::findPartitionCat(tree2.tune, X1, X2)
      
      sec.parti <- DataSimilarity:::intersectPartitionsCat(parti1, parti2)
      sec.parti.tune <- DataSimilarity:::intersectPartitionsCat(parti1.tune, parti2.tune)
      
      GCR.tune <- DataSimilarity:::calculateGCRCat(X1, X2)
      GCR.no.tune <- DataSimilarity:::calculateGCRCat(X1, X2, tune = FALSE)
      
      P1.joint <- colSums(DataSimilarity:::calcPCat(sec.parti$parti, X1, n = "joint"))
      P2.joint <- colSums(DataSimilarity:::calcPCat(sec.parti$parti, X2, n = "joint"))
      
      P1.joint.tune <- colSums(DataSimilarity:::calcPCat(GCR.tune$res.intersect$parti, X1, n = "joint"))
      P2.joint.tune <- colSums(DataSimilarity:::calcPCat(GCR.tune$res.intersect$parti, X2, n = "joint"))
      
      P1.joint.no.tune <- colSums(DataSimilarity:::calcPCat(GCR.no.tune$res.intersect$parti, X1, n = "joint"))
      P2.joint.no.tune <- colSums(DataSimilarity:::calcPCat(GCR.no.tune$res.intersect$parti, X2, n = "joint"))
      
      P1.cond <- DataSimilarity:::calcPCat(parti1, X1, "conditional")
      P2.cond <- DataSimilarity:::calcPCat(parti2, X2, "conditional")
      
      P1.cond.tune <- DataSimilarity:::calcPCat(GCR.tune$parti1, X1, "conditional")
      P2.cond.tune <- DataSimilarity:::calcPCat(GCR.tune$parti2, X2, "conditional")
      
      P1.cond.no.tune <- DataSimilarity:::calcPCat(GCR.no.tune$parti1, X1, "conditional")
      P2.cond.no.tune <- DataSimilarity:::calcPCat(GCR.no.tune$parti2, X2, "conditional")
      
      res.GGRL.tune.sum.f.a <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", n.eval = 3, k = 2, seed = i)
      
      res.GGRL.no.tune.sum.f.a <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", tune = FALSE, seed = i)
      
      res.GGRL.tune.max.f.a <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", 
                                                       agg.fun = max, n.eval = 3, k = 2, seed = i)
      
      res.GGRL.no.tune.max.f.a <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", 
                                                          tune = FALSE, agg.fun = max, seed = i)
      
      res.GGRL.tune.sum.f.s <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", 
                                                       diff.fun = DataSimilarity:::f.sCat, n.eval = 3, k = 2, seed = i)
      
      res.GGRL.no.tune.sum.f.s <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y",  
                                                          tune = FALSE, diff.fun = DataSimilarity:::f.sCat, seed = i)
      
      res.GGRL.tune.max.f.s <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", n.eval = 3, k = 2, 
                                                       diff.fun = DataSimilarity:::f.sCat, agg.fun = max, seed = i)
      
      res.GGRL.no.tune.max.f.s <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", tune = FALSE, 
                                                          diff.fun = DataSimilarity:::f.sCat, agg.fun = max, seed = i)
      
      
      res.GGRL.tune.sum.f.a.perm <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", 
                                                            n.perm = 2, n.eval = 3, k = 2, seed = i)
      
      res.GGRL.no.tune.sum.f.a.perm <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", 
                                                               tune = FALSE, n.perm = 2, seed = i)
      
      res.GGRL.tune.max.f.a.perm <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", 
                                                            agg.fun = max, n.perm = 2, n.eval = 3, k = 2, seed = i)
      
      res.GGRL.no.tune.max.f.a.perm <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", 
                                                               tune = FALSE, agg.fun = max, n.perm = 2, seed = i)
      
      res.GGRL.tune.sum.f.s.perm <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", 
                                                            diff.fun = DataSimilarity:::f.sCat, n.perm = 2, n.eval = 3, k = 2, seed = i)
      
      res.GGRL.no.tune.sum.f.s.perm <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", n.eval = 2,  
                                                               tune = FALSE, diff.fun = DataSimilarity:::f.sCat, n.perm = 2, seed = i)
      
      res.GGRL.tune.max.f.s.perm <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", n.eval = 2, 
                                                            diff.fun = DataSimilarity:::f.sCat, agg.fun = max, n.perm = 2, seed = i)
      
      res.GGRL.no.tune.max.f.s.perm <- DataSimilarity::GGRLCat(X1, X2, target1 = "y", target2 = "y", tune = FALSE, 
                                                               diff.fun = DataSimilarity:::f.sCat, agg.fun = max, n.perm = 2, seed = i)
      
      
      testthat::test_that("helper functions", {
        # check if resulting partition has length equal to number of leaves 
        # and if each list element has number of rows equal to number of variables 
        # and if minima and maxima are matching minima and maxima of data or any of 
        # the split points
        testthat::expect_length(parti1, sum(tree1$frame$var == "<leaf>"))
        testthat::expect_length(parti2, sum(tree2$frame$var == "<leaf>"))
        testthat::expect_true(all(sapply(parti1, length) == ncol(X1) - 1))
        testthat::expect_true(all(sapply(parti2, length) == ncol(X2) - 1))
        
        testthat::expect_length(sec.parti$parti, nrow(sec.parti$combis))
        testthat::expect_true(all(sapply(sec.parti$parti, length) == ncol(X1) - 1))
        
        testthat::expect_true(all(sapply(GCR.tune$parti1, length) == ncol(X1) - 1))
        testthat::expect_true(all(sapply(GCR.tune$parti2, length) == ncol(X2) - 1))
        
        testthat::expect_length(sec.parti$parti, nrow(sec.parti$combis))
        testthat::expect_true(all(sapply(sec.parti$parti, length) == ncol(X1) - 1))
        
        testthat::expect_length(parti1.tune, sum(tree1.tune$frame$var == "<leaf>"))
        testthat::expect_length(parti2.tune, sum(tree2.tune$frame$var == "<leaf>"))
        testthat::expect_true(all(sapply(parti1.tune, length) == ncol(X1) - 1))
        testthat::expect_true(all(sapply(parti2.tune, length) == ncol(X2) - 1))
        
        testthat::expect_length(sec.parti.tune$parti, nrow(sec.parti.tune$combis))
        testthat::expect_true(all(sapply(sec.parti.tune$parti, length) == ncol(X1) - 1))
        
        # check if sums over calcP are equal to one
        testthat::expect_equal(sum(P1.joint), 1)
        testthat::expect_equal(sum(P2.joint), 1)
        
        testthat::expect_equal(sum(P1.joint.tune), 1)
        testthat::expect_equal(sum(P2.joint.tune), 1)
        
        testthat::expect_equal(sum(P1.joint.no.tune), 1)
        testthat::expect_equal(sum(P2.joint.no.tune), 1)
        
        testthat::expect_true(all(colSums(P1.cond) == 1))
        testthat::expect_true(all(colSums(P2.cond) == 1))
        
        testthat::expect_true(all(colSums(P1.cond.tune) == 1))
        testthat::expect_true(all(colSums(P2.cond.tune) == 1))
        
        testthat::expect_true(all(colSums(P1.cond.no.tune) == 1))
        testthat::expect_true(all(colSums(P2.cond.no.tune) == 1))
      })
      
      testthat::test_that("output GGRL", {
        # check output types for NKT + statistic in [0, 1]
        # check length and names of output
        testthat::expect_length(res.GGRL.tune.sum.f.a, 8)
        testthat::expect_named(res.GGRL.tune.sum.f.a, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.sum.f.a, 8)
        testthat::expect_named(res.GGRL.no.tune.sum.f.a, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.tune.max.f.a, 8)
        testthat::expect_named(res.GGRL.tune.max.f.a, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.max.f.a, 8)
        testthat::expect_named(res.GGRL.no.tune.max.f.a, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.tune.sum.f.s, 8)
        testthat::expect_named(res.GGRL.tune.sum.f.s, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.sum.f.s, 8)
        testthat::expect_named(res.GGRL.no.tune.sum.f.s, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.tune.max.f.s, 8)
        testthat::expect_named(res.GGRL.tune.max.f.s, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.max.f.s, 8)
        testthat::expect_named(res.GGRL.no.tune.max.f.s, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        
        testthat::expect_length(res.GGRL.tune.sum.f.a.perm, 8)
        testthat::expect_named(res.GGRL.tune.sum.f.a.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.sum.f.a.perm, 8)
        testthat::expect_named(res.GGRL.no.tune.sum.f.a.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.tune.max.f.a.perm, 8)
        testthat::expect_named(res.GGRL.tune.max.f.a.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.max.f.a.perm, 8)
        testthat::expect_named(res.GGRL.no.tune.max.f.a.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.tune.sum.f.s.perm, 8)
        testthat::expect_named(res.GGRL.tune.sum.f.s.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.sum.f.s.perm, 8)
        testthat::expect_named(res.GGRL.no.tune.sum.f.s.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.tune.max.f.s.perm, 8)
        testthat::expect_named(res.GGRL.tune.max.f.s.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        testthat::expect_length(res.GGRL.no.tune.max.f.s.perm, 8)
        testthat::expect_named(res.GGRL.no.tune.max.f.s.perm, 
                               c("statistic", "p.value", "estimate", 
                                 "alternative", "method", "data.name", "diff.fun", "agg.fun"))
        
        # check approx.and perm p value is NA
        testthat::expect_true(is.na(res.GGRL.tune.sum.f.a$p.value))
        testthat::expect_true(is.na(res.GGRL.no.tune.sum.f.a$p.value))
        testthat::expect_true(is.na(res.GGRL.tune.max.f.a$p.value))
        testthat::expect_true(is.na(res.GGRL.no.tune.max.f.a$p.value))
        testthat::expect_true(is.na(res.GGRL.tune.sum.f.s$p.value))
        testthat::expect_true(is.na(res.GGRL.no.tune.sum.f.s$p.value))
        testthat::expect_true(is.na(res.GGRL.tune.max.f.s$p.value))
        testthat::expect_true(is.na(res.GGRL.no.tune.max.f.s$p.value))
        
        testthat::expect_gte(res.GGRL.tune.sum.f.a.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.no.tune.sum.f.a.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.tune.max.f.a.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.no.tune.max.f.a.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.tune.sum.f.s.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.no.tune.sum.f.s.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.tune.max.f.s.perm$p.value, 0)
        testthat::expect_gte(res.GGRL.no.tune.max.f.s.perm$p.value, 0)
        
        testthat::expect_lte(res.GGRL.tune.sum.f.a.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.no.tune.sum.f.a.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.tune.max.f.a.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.no.tune.max.f.a.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.tune.sum.f.s.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.no.tune.sum.f.s.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.tune.max.f.s.perm$p.value, 1)
        testthat::expect_lte(res.GGRL.no.tune.max.f.s.perm$p.value, 1)
        
        # statistic is not NA
        testthat::expect_false(is.na(res.GGRL.tune.sum.f.a$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.sum.f.a$statistic))
        testthat::expect_false(is.na(res.GGRL.tune.max.f.a$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.max.f.a$statistic))
        testthat::expect_false(is.na(res.GGRL.tune.sum.f.s$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.sum.f.s$statistic))
        testthat::expect_false(is.na(res.GGRL.tune.max.f.s$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.max.f.s$statistic))
        
        testthat::expect_false(is.na(res.GGRL.tune.sum.f.a.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.sum.f.a.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.tune.max.f.a.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.max.f.a.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.tune.sum.f.s.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.sum.f.s.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.tune.max.f.s.perm$statistic))
        testthat::expect_false(is.na(res.GGRL.no.tune.max.f.s.perm$statistic))
        
        # output should be numeric
        testthat::expect_s3_class(res.GGRL.tune.sum.f.a, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.sum.f.a, "htest")
        testthat::expect_s3_class(res.GGRL.tune.max.f.a, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.max.f.a, "htest")
        testthat::expect_s3_class(res.GGRL.tune.sum.f.s, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.sum.f.s, "htest")
        testthat::expect_s3_class(res.GGRL.tune.max.f.s, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.max.f.s, "htest")
        
        testthat::expect_s3_class(res.GGRL.tune.sum.f.a.perm, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.sum.f.a.perm, "htest")
        testthat::expect_s3_class(res.GGRL.tune.max.f.a.perm, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.max.f.a.perm, "htest")
        testthat::expect_s3_class(res.GGRL.tune.sum.f.s.perm, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.sum.f.s.perm, "htest")
        testthat::expect_s3_class(res.GGRL.tune.max.f.s.perm, "htest")
        testthat::expect_s3_class(res.GGRL.no.tune.max.f.s.perm, "htest")
      })
    }
  }
}

set.seed(0305)
testGGRLCat(1)
