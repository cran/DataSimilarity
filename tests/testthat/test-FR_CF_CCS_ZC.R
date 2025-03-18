testgTestBased <- function(n.iter, new.fun, old.type) {
  if(requireNamespace("gTests", quietly = TRUE) & requireNamespace("rmvnorm", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -2, 2))
      X2 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2), 
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      dists <- stats::dist(rbind(X1, X2))
      E <- ade4::mstree(dists, ngmax = 1)
      set.seed(i)
      res.gTest <- gTests::g.tests(E, sample1ID = 1:nrow(X1), 
                                   sample2ID = (nrow(X1)+1):(nrow(X1)+nrow(X2)), 
                                   test.type = old.type, perm = 10)
      res.FR.appr <- new.fun(X1, as.data.frame(X2), seed = i)
      res.FR.perm <- new.fun(X1, as.data.frame(X2), n.perm = 10, seed = i)
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.FR.appr, 6)
        testthat::expect_named(res.FR.appr, c("statistic", "p.value", "estimate", 
                                              "alternative", "method", "data.name"))
        testthat::expect_length(res.FR.perm, 6)
        testthat::expect_named(res.FR.perm, c("statistic", "p.value", "estimate", 
                                              "alternative", "method", "data.name"))
        # check p values in [0,1]
        testthat::expect_lte(res.FR.appr$p.value, 1)
        testthat::expect_gte(res.FR.appr$p.value, 0)
        testthat::expect_lte(res.FR.perm$p.value, 1)
        testthat::expect_gte(res.FR.perm$p.value, 0)
        
        # statistic is not NA
        testthat::expect_false(is.na(res.FR.appr$statistic))
        testthat::expect_false(is.na(res.FR.perm$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.FR.appr, "htest")
        testthat::expect_s3_class(res.FR.perm, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.FR.appr$statistic, res.gTest[[old.type]]$test.statistic, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.FR.perm$statistic, res.gTest[[old.type]]$test.statistic, 
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.FR.appr$p.value, res.gTest[[old.type]]$pval.approx, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.FR.perm$p.value, res.gTest[[old.type]]$pval.perm, 
                               check.attributes = FALSE)
      })
      
      res.FR.appr.1 <- new.fun(X1[, 1, drop = FALSE], as.data.frame(X2)[, 1, drop = FALSE],
                               seed = i)
      res.FR.perm.1 <- new.fun(X1[, 1, drop = FALSE], as.data.frame(X2)[, 1, drop = FALSE], 
                               n.perm = 10, seed = i)
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.FR.appr.1, 6)
        testthat::expect_named(res.FR.appr.1, c("statistic", "p.value", "estimate", 
                                                "alternative", "method", "data.name"))
        testthat::expect_length(res.FR.perm.1, 6)
        testthat::expect_named(res.FR.perm.1, c("statistic", "p.value", "estimate", 
                                                "alternative", "method", "data.name"))
        # check p values in [0,1]
        testthat::expect_lte(res.FR.appr.1$p.value, 1)
        testthat::expect_gte(res.FR.appr.1$p.value, 0)
        testthat::expect_lte(res.FR.perm.1$p.value, 1)
        testthat::expect_gte(res.FR.perm.1$p.value, 0)
        
        # statistic is not NA
        testthat::expect_false(is.na(res.FR.appr.1$statistic))
        testthat::expect_false(is.na(res.FR.perm.1$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.FR.appr.1, "htest")
        testthat::expect_s3_class(res.FR.perm.1, "htest")
      })
    }
  }
}

testgTestCatBased <- function(n.iter, new.fun, old.type) {
  if(requireNamespace("KMD", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1cat <- matrix(sample(1:4, 300, replace = TRUE), ncol = 3)
      X2cat <- matrix(sample(1:4, 300, replace = TRUE, prob = 1:4), ncol = 3)
      dist.fun <- function(x, y) sum(x != y)
      new.pooled.data <- apply(rbind(X1cat, X2cat), 1, paste0, collapse = "_")
      counts <- table(new.pooled.data, c(rep(1, nrow(X1cat)), rep(2, nrow(X2cat))))
      dists.cat <- outer(rownames(counts), rownames(counts),
                         function(x, y) apply(cbind(x, y), 1, 
                                              function(z) dist.fun(strsplit(as.character(z[1]), "_")[[1]], 
                                                                   strsplit(as.character(z[2]), "_")[[1]])))
      rownames(dists.cat) <- colnames(dists.cat) <- rownames(counts)
      E.cat <- gTests::getGraph(counts, dists.cat, K = 1, graph.type = "mstree")
      set.seed(i)
      res.gTest <- gTests::g.tests_discrete(E.cat, counts, test.type = old.type, perm = 10)
      res.FR.appr.u <- new.fun(X1cat, X2cat, agg.type = "u", dist.fun = function(x, y) sum(x != y), seed = i)
      res.FR.appr.a <- new.fun(X1cat, X2cat, agg.type = "a", dist.fun = function(x, y) sum(x != y), seed = i)
      res.FR.perm.u <- new.fun(X1cat, X2cat, agg.type = "u", dist.fun = function(x, y) sum(x != y), n.perm = 10, seed = i)
      res.FR.perm.a <- new.fun(X1cat, X2cat, agg.type = "a", dist.fun = function(x, y) sum(x != y), n.perm = 10, seed = i)
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.FR.appr.u, 7)
        testthat::expect_named(res.FR.appr.u, c("statistic", "parameter", "p.value", "estimate", 
                                                "alternative", "method", "data.name"))
        testthat::expect_length(res.FR.appr.a, 7)
        testthat::expect_named(res.FR.appr.a, c("statistic", "parameter", "p.value", "estimate",  
                                                "alternative", "method", "data.name"))
        testthat::expect_length(res.FR.perm.u, 7)
        testthat::expect_named(res.FR.perm.u, c("statistic", "parameter", "p.value", "estimate", 
                                                "alternative", "method", "data.name"))
        testthat::expect_length(res.FR.perm.a, 7)
        testthat::expect_named(res.FR.perm.a, c("statistic", "parameter", "p.value", "estimate", 
                                                "alternative", "method", "data.name"))
        # check p values in [0,1]
        testthat::expect_lte(res.FR.appr.u$p.value, 1)
        testthat::expect_gte(res.FR.appr.u$p.value, 0)
        testthat::expect_lte(res.FR.appr.a$p.value, 1)
        testthat::expect_gte(res.FR.appr.a$p.value, 0)
        testthat::expect_lte(res.FR.perm.u$p.value, 1)
        testthat::expect_gte(res.FR.perm.u$p.value, 0)
        testthat::expect_lte(res.FR.perm.a$p.value, 1)
        testthat::expect_gte(res.FR.perm.a$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.FR.appr.u$statistic))
        testthat::expect_false(is.na(res.FR.appr.a$statistic))
        testthat::expect_false(is.na(res.FR.perm.u$statistic))
        testthat::expect_false(is.na(res.FR.perm.a$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.FR.appr.u, "htest")
        testthat::expect_s3_class(res.FR.appr.a, "htest")
        testthat::expect_s3_class(res.FR.perm.u, "htest")
        testthat::expect_s3_class(res.FR.perm.a, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.FR.appr.u$statistic, res.gTest[[old.type]]$test.statistic_u, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.FR.appr.a$statistic, res.gTest[[old.type]]$test.statistic_a, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.FR.perm.u$statistic, res.gTest[[old.type]]$test.statistic_u, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.FR.perm.a$statistic, res.gTest[[old.type]]$test.statistic_a, 
                               check.attributes = FALSE)
        # check test p values
        testthat::expect_equal(res.FR.appr.u$p.value, res.gTest[[old.type]]$pval.approx_u, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.FR.appr.a$p.value, res.gTest[[old.type]]$pval.approx_a, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.FR.perm.u$p.value, res.gTest[[old.type]]$pval.perm_u, 
                               check.attributes = FALSE)
        testthat::expect_equal(res.FR.perm.a$p.value, res.gTest[[old.type]]$pval.perm_a, 
                               check.attributes = FALSE)
      })
      
      res.FR.appr.u.1 <- new.fun(X1cat, X2cat, agg.type = "u", dist.fun = function(x, y) sum(x != y), seed = i)
      res.FR.appr.a.1 <- new.fun(X1cat, X2cat, agg.type = "a", dist.fun = function(x, y) sum(x != y), seed = i)
      res.FR.perm.u.1 <- new.fun(X1cat, X2cat, agg.type = "u", dist.fun = function(x, y) sum(x != y), n.perm = 10, seed = i)
      res.FR.perm.a.1 <- new.fun(X1cat, X2cat, agg.type = "a", dist.fun = function(x, y) sum(x != y), n.perm = 10, seed = i)
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.FR.appr.u.1, 7)
        testthat::expect_named(res.FR.appr.u.1, c("statistic", "parameter", "p.value", "estimate", 
                                                  "alternative", "method", "data.name"))
        testthat::expect_length(res.FR.appr.a.1, 7)
        testthat::expect_named(res.FR.appr.a.1, c("statistic", "parameter", "p.value", "estimate",  
                                                  "alternative", "method", "data.name"))
        testthat::expect_length(res.FR.perm.u.1, 7)
        testthat::expect_named(res.FR.perm.u.1, c("statistic", "parameter", "p.value", "estimate", 
                                                  "alternative", "method", "data.name"))
        testthat::expect_length(res.FR.perm.a.1, 7)
        testthat::expect_named(res.FR.perm.a.1, c("statistic", "parameter", "p.value", "estimate", 
                                                  "alternative", "method", "data.name"))
        # check p values in [0,1]
        testthat::expect_lte(res.FR.appr.u.1$p.value, 1)
        testthat::expect_gte(res.FR.appr.u.1$p.value, 0)
        testthat::expect_lte(res.FR.appr.a.1$p.value, 1)
        testthat::expect_gte(res.FR.appr.a.1$p.value, 0)
        testthat::expect_lte(res.FR.perm.u.1$p.value, 1)
        testthat::expect_gte(res.FR.perm.u.1$p.value, 0)
        testthat::expect_lte(res.FR.perm.a.1$p.value, 1)
        testthat::expect_gte(res.FR.perm.a.1$p.value, 0)
        # statistic is not NA
        testthat::expect_false(is.na(res.FR.appr.u.1$statistic))
        testthat::expect_false(is.na(res.FR.appr.a.1$statistic))
        testthat::expect_false(is.na(res.FR.perm.u.1$statistic))
        testthat::expect_false(is.na(res.FR.perm.a.1$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.FR.appr.u.1, "htest")
        testthat::expect_s3_class(res.FR.appr.a.1, "htest")
        testthat::expect_s3_class(res.FR.perm.u.1, "htest")
        testthat::expect_s3_class(res.FR.perm.a.1, "htest")
      })
    }
  }
}

################################################################################
set.seed(0305)
testgTestBased(1, DataSimilarity::FR, "original")

set.seed(0305)
testgTestCatBased(1, DataSimilarity::FR_cat, "original")

set.seed(0305)
testgTestBased(1, DataSimilarity::CF, "generalized")

set.seed(0305)
testgTestCatBased(1, DataSimilarity::CF_cat, "generalized")

set.seed(0305)
testgTestBased(1, DataSimilarity::CCS, "weighted")

set.seed(0305)
testgTestCatBased(1, DataSimilarity::CCS_cat, "weighted")

set.seed(0305)
testgTestBased(1, DataSimilarity::ZC, "maxtype")

set.seed(0305)
testgTestCatBased(1, DataSimilarity::ZC_cat, "maxtype")


