findSimilarityMethod <- function(Numeric = FALSE, Categorical = FALSE, 
                                 Target.Inclusion = FALSE, Multiple.Samples = FALSE, 
                                 only.names = TRUE, ...) {
  arg.list <- as.list(match.call())[-1]
  arg.list <- arg.list[names(arg.list) != "only.names"]
  fulfilled <- names(arg.list)[unlist(arg.list)]
  if(length(fulfilled) > 0) {
    red.dat <- DataSimilarity::method.table[, fulfilled, drop = FALSE]
    ind <- rowSums(!is.na(red.dat) & red.dat != "Unfulfilled") == length(fulfilled)
    red.methds <- DataSimilarity::method.table[ind, ]
  } else {
    red.methds <- DataSimilarity::method.table
  }
  if(only.names & nrow(red.methds) > 0) {
    return(red.methds$Implementation)
  } else if(nrow(red.methds) == 0) {
    message("No suitable method found. Returning all possible methods.")
    return(DataSimilarity::method.table)
  } else {
    return(red.methds)
  }
}
