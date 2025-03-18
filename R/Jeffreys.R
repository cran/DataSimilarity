################################################################################
##                            JEFFREYS DIVERGENCE                             ##
##                                                                            ##
################################################################################
Jeffreys <- function(X1, X2, method = "KLIEP", verbose = FALSE, seed = 42) {
  if(!requireNamespace("densratio", quietly = TRUE)) {
    stop("Package \"densratio\" required for using method Jeffreys().")
  }
  set.seed(seed)
  dname <- c(deparse1(substitute(X1)), deparse1(substitute(X2)))
  if(!(inherits(X1, "matrix") | inherits(X1, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(!(inherits(X2, "matrix") | inherits(X2, "data.frame"))) {
    stop("X1 must be provided as a data.frame or matrix.")
  }
  if(ncol(X1) != ncol(X2)) {
    stop("All datasets must have the same number of variables.")
  }
  new_x1 <- X1
  new_x2 <- X2
  ratio_obj1 <- densratio::densratio(X1, X2, method = method, verbose = verbose)
  ratio_obj2 <- densratio::densratio(X2, X1, method = method, verbose = verbose)
  hatR1 <- ratio_obj1$compute_density_ratio(new_x1)
  hatR2 <- ratio_obj2$compute_density_ratio(new_x2)
  
  Div <- mean(log(hatR1)) + mean(log(hatR2))
  names(Div) <- "Div"
  
  res <- list(statistic = Div, 
              p.value = NULL, 
              estimate = NULL, 
              alternative = paste0("The distributions of ", 
                                   paste0(dname, collapse = " and "),
                                   " are unequal."), 
              method = "Jeffreys Divergence",  
              data.name = paste0(dname, collapse = ", "),
              parameters = NULL)
  class(res) <- "htest"
  return(res)
}

