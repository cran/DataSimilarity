################################################################################
##          HELPER FUNCTIONS FOR UNIVARIATE TWO-SAMPLE STATISTICS             ##
##                                                                            ##
################################################################################
MD <- function(x1, x2) {
  return(mean(x1) - mean(x2))
}

tStat <- function(x1, x2) {
  return(stats::t.test(x1, x2)$statistic)
}

AUC <- function(x1, x2) {
  if(!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package \"pROC\" required for using method AUC().")
  }
  a <- pROC::auc(response = c(rep(0, length(x1)), rep(1, length(x2))), 
                 predictor = c(x1, x2), quiet = TRUE)
  return(as.numeric(a))
}

################################################################################
##              HELPER FUNCTIONS FOR DIRECTION PROJECTION STEP                ##
##                                                                            ##
################################################################################
dwdProj <- function(X1, X2) {
  if(!requireNamespace("DWDLargeR", quietly = TRUE)) {
    stop("Package \"DWDLargeR\" required for using method dwdProj().")
  }
  if(!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package \"Matrix\" required for using method dwdProj().")
  }
  if(requireNamespace("purrr", quietly = TRUE)) {
    dwd.classif <- purrr::quietly(DWDLargeR::genDWD)
  } else {
    dwd.classif <- DWDLargeR::genDWD
  }
  y <- c(rep(1, nrow(X1)), rep(-1, nrow(X2)))
  pooled.samp <- rbind(X1, X2)
  # calculate the best penalty parameter
  C.opt <- DWDLargeR::penaltyParameter(Matrix::Matrix(t(pooled.samp), sparse = TRUE), y, 
                                       expon = 1, rmzeroFea = 0)
  
  # Unclear from documentation but genDWD throws error if X is not sparse matrix
  mod <- dwd.classif(X = Matrix::Matrix(t(pooled.samp), sparse = TRUE), y = y, 
                     C = C.opt, expon = 1, rmzeroFea = 0)
  if(requireNamespace("purrr", quietly = TRUE)) {
    if(length(mod$warnings) > 0) {
      warning(mod$warnings)
    }
    if(length(mod$messages) > 0) {
      message(mod$messages)
    }
    mod <- mod$result
  }
  # Documentation says w is unit vector but in practice does not always have length 1
  proj <- drop(as.matrix(pooled.samp) %*% mod$w / sqrt(sum(mod$w^2)))
  return(proj)
}

svmProj <- function(X1, X2) {
  if(!requireNamespace("e1071", quietly = TRUE)) {
    stop("Package \"e1071\" required for using method svmProj().")
  }
  pooled.samp <- rbind(X1, X2)
  mod <- e1071::svm(x = pooled.samp, y = c(rep(1, nrow(X1)), rep(-1, nrow(X2))), 
                    type = "C-classification", kernel = "linear")
  # calculate normal vector of separating hyperplane using dual solution:
  # w = sum_{i = 1}^n y_i \alpha\_i x_i = c^T X, where c_i = y_i \alpha\_i are
  # given by mod$coefs
  norm.vec <- drop(t(mod$coefs) %*% as.matrix(pooled.samp[mod$index, , drop = FALSE]))
  norm.vec <- norm.vec / sqrt(sum(norm.vec^2)) 
  proj <- drop(as.matrix(pooled.samp) %*% norm.vec)
  return(proj)
}
