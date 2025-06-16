DataSimilarity <- function(X1, X2, method, ...) {
  ds.fun <- match.fun(method)
  arg.list <- as.list(match.call(expand.dots = TRUE))
  arg.list <- arg.list[names(arg.list) != "method"][-1]
  do.call(ds.fun, arg.list)
}