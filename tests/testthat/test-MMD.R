testMMD <- function(n.iter) {
  if(requireNamespace("kernlab", quietly = TRUE)) {
    for(i in 1:n.iter) {
      set.seed(i)
      X1 <- mvtnorm::rmvnorm(100, sigma = matrix(0.2, 10, 10) + diag(0.8, 10, 10), 
                             mean = runif(10, -2, 2))
      X2 <- mvtnorm::rmvnorm(100, mean = runif(10, -2, 2),
                             sigma = matrix(0.5, 10, 10) + diag(0.5, 10, 10))
      
      set.seed(i)
      res.kmmd <- kernlab::kmmd(X1, X2)
      res.MMD <- DataSimilarity::MMD(X1, as.data.frame(X2), n.perm = 0, seed = i)
      res.MMD.perm <- DataSimilarity::MMD(X1, as.data.frame(X2), n.perm = 10, 
                                          seed = i)
      
      set.seed(i)
      res.kmmd.a <- kernlab::kmmd(X1, X2, asymptotic = TRUE)
      res.MMD.a <- DataSimilarity::MMD(X1, as.data.frame(X2), n.perm = 0, 
                                       asymptotic = TRUE, seed = i)
      res.MMD.perm.a <- DataSimilarity::MMD(X1, as.data.frame(X2), n.perm = 10, 
                                            asymptotic = TRUE, seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.MMD, 11)
        testthat::expect_named(res.MMD, c("statistic", "p.value", "estimate", 
                                          "alternative", "method", "data.name",
                                          "H0", "asymp.H0", "kernel.fun", 
                                          "Rademacher.bound", "asymp.bound"))
        testthat::expect_length(res.MMD.perm, 11)
        testthat::expect_named(res.MMD.perm, c("statistic", "p.value", "estimate", 
                                               "alternative", "method", "data.name", 
                                               "H0", "asymp.H0", "kernel.fun", 
                                               "Rademacher.bound", "asymp.bound"))
        testthat::expect_length(res.MMD.a, 11)
        testthat::expect_named(res.MMD.a, c("statistic", "p.value", "estimate", 
                                            "alternative", "method", "data.name", 
                                            "H0", "asymp.H0", "kernel.fun", 
                                            "Rademacher.bound", "asymp.bound"))
        testthat::expect_length(res.MMD.perm.a, 11)
        testthat::expect_named(res.MMD.perm.a, c("statistic", "p.value", "estimate", 
                                                 "alternative", "method", "data.name", 
                                                 "H0", "asymp.H0", "kernel.fun", 
                                                 "Rademacher.bound", "asymp.bound"))
        # check p values in [0,1]
        testthat::expect_lte(res.MMD.perm$p.value, 1)
        testthat::expect_gte(res.MMD.perm$p.value, 0)
        testthat::expect_lte(res.MMD.perm.a$p.value, 1)
        testthat::expect_gte(res.MMD.perm.a$p.value, 0)
        # check approx. p value is NA
        testthat::expect_true(is.na(res.MMD$p.value))
        testthat::expect_true(is.na(res.MMD.a$p.value))
        # statistic is not NA
        testthat::expect_false(is.na(res.MMD$statistic))
        testthat::expect_false(is.na(res.MMD.perm$statistic))
        testthat::expect_false(is.na(res.MMD.a$statistic))
        testthat::expect_false(is.na(res.MMD.perm.a$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.MMD, "htest")
        testthat::expect_s3_class(res.MMD.perm, "htest")
        testthat::expect_s3_class(res.MMD.a, "htest")
        testthat::expect_s3_class(res.MMD.perm.a, "htest")
      })
      
      testthat::test_that("output values", {
        # check test statistic values
        testthat::expect_equal(res.MMD$statistic,
                               kernlab::mmdstats(res.kmmd)[1], 
                               check.attributes = FALSE)
        testthat::expect_equal(res.MMD.perm$statistic, 
                               kernlab::mmdstats(res.kmmd)[1], 
                               check.attributes = FALSE)
        testthat::expect_equal(res.MMD.a$statistic, 
                               kernlab::mmdstats(res.kmmd.a)[1], 
                               check.attributes = FALSE)
        testthat::expect_equal(res.MMD.perm.a$statistic, 
                               kernlab::mmdstats(res.kmmd.a)[1],
                               check.attributes = FALSE)
      })
      
      
      res.MMD.1 <- DataSimilarity::MMD(X1[, 1, drop = FALSE], 
                                       as.data.frame(X2)[, 1, drop = FALSE], 
                                       n.perm = 0, seed = i)
      res.MMD.perm.1 <- DataSimilarity::MMD(X1[, 1, drop = FALSE], 
                                            as.data.frame(X2)[, 1, drop = FALSE],
                                            n.perm = 3, seed = i)
      res.MMD.a.1 <- DataSimilarity::MMD(X1[, 1, drop = FALSE], 
                                         as.data.frame(X2)[, 1, drop = FALSE],
                                         n.perm = 0, asymptotic = TRUE, seed = i)
      res.MMD.perm.a.1 <- DataSimilarity::MMD(X1[, 1, drop = FALSE], 
                                              as.data.frame(X2)[, 1, drop = FALSE], 
                                              n.perm = 3, asymptotic = TRUE, 
                                              seed = i)
      
      testthat::test_that("output type", {
        # check length and names of output
        testthat::expect_length(res.MMD.1, 11)
        testthat::expect_named(res.MMD.1, c("statistic", "p.value", "estimate", 
                                            "alternative", "method", "data.name",
                                            "H0", "asymp.H0", "kernel.fun", 
                                            "Rademacher.bound", "asymp.bound"))
        testthat::expect_length(res.MMD.perm.1, 11)
        testthat::expect_named(res.MMD.perm.1, c("statistic", "p.value", "estimate", 
                                                 "alternative", "method", "data.name", 
                                                 "H0", "asymp.H0", "kernel.fun", 
                                                 "Rademacher.bound", "asymp.bound"))
        testthat::expect_length(res.MMD.a.1, 11)
        testthat::expect_named(res.MMD.a.1, c("statistic", "p.value", "estimate", 
                                              "alternative", "method", "data.name", 
                                              "H0", "asymp.H0", "kernel.fun", 
                                              "Rademacher.bound", "asymp.bound"))
        testthat::expect_length(res.MMD.perm.a.1, 11)
        testthat::expect_named(res.MMD.perm.a.1, c("statistic", "p.value", "estimate", 
                                                   "alternative", "method", "data.name", 
                                                   "H0", "asymp.H0", "kernel.fun", 
                                                   "Rademacher.bound", "asymp.bound"))
        # check p values in [0,1]
        testthat::expect_lte(res.MMD.perm.1$p.value, 1)
        testthat::expect_gte(res.MMD.perm.1$p.value, 0)
        testthat::expect_lte(res.MMD.perm.a.1$p.value, 1)
        testthat::expect_gte(res.MMD.perm.a.1$p.value, 0)
        # check approx. p value is NA
        testthat::expect_true(is.na(res.MMD.1$p.value))
        testthat::expect_true(is.na(res.MMD.a.1$p.value))
        # statistic is not NA
        testthat::expect_false(is.na(res.MMD.1$statistic))
        testthat::expect_false(is.na(res.MMD.perm.1$statistic))
        testthat::expect_false(is.na(res.MMD.a.1$statistic))
        testthat::expect_false(is.na(res.MMD.perm.a.1$statistic))
        # output should be numeric
        testthat::expect_s3_class(res.MMD.1, "htest")
        testthat::expect_s3_class(res.MMD.perm.1, "htest")
        testthat::expect_s3_class(res.MMD.a.1, "htest")
        testthat::expect_s3_class(res.MMD.perm.a.1, "htest")
      })
    }
  }
}

set.seed(0305)
testMMD(1)
