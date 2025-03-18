################################################################################
##                              HELPER FUNCTIONS                              ##
##                                                                            ##
################################################################################
findPartition <- function(tree, X1, X2) {
  f <- tree$frame
  s <- labels(tree, digits = 16)
  ids <- as.numeric(rownames(f))
  leaves <- ids[f$var == "<leaf>"]
  vars <- setdiff(colnames(X1), "y")
  in.eq <- vector(mode = "list", length = length(leaves))
  for(i in seq(along = leaves)) {
    id <- leaves[i]
    # go from leaf node to root and save all inequalities of the nodes on this path
    while(id > 1) {
      split <- s[which(ids == id)]
      min <- grepl(">=", split)
      split <- strsplit(split, ">=|< ")
      v <- split[[1]][1]
      val <- as.numeric(trimws(split[[1]][2]))
      in.eq[[i]] <- rbind(in.eq[[i]], 
                          if(min){
                            data.frame(var = v, 
                                       min = val, 
                                       max = max(X1[, v], X2[, v]))
                          } else {
                            data.frame(var = v, 
                                       min = min(X1[, v], X2[, v]), 
                                       max = val)
                          })
      id <- id %/% 2
    }
    # if a variable was used in multiple splits: take intersection of splits
    if(anyDuplicated(in.eq[[i]]$var)) {
      in.eq[[i]] <- do.call(rbind, tapply(in.eq[[i]], in.eq[[i]]$var, function(x) {
        x <- as.data.frame(x)
        data.frame(var = x$var[1], min = max(x$min), max = min(x$max))
      }))
      rownames(in.eq[[i]]) <- NULL
    }
    # add variables that were not used in any split
    rest.vars <- setdiff(vars, in.eq[[i]]$var)
    for(v in rest.vars) {
      in.eq[[i]] <- rbind(in.eq[[i]], 
                          data.frame(var = v, 
                                     min = min(X1[, v], X2[, v]), 
                                     max = max(X1[, v], X2[, v])))
    }
    in.eq[[i]] <- in.eq[[i]][order(in.eq[[i]]$var), ]
  }
  return(in.eq)
}

intersectPartitions <- function(parti1, parti2) {
  combis <- expand.grid(1:length(parti1), 1:length(parti2))
  sec.parti <- apply(combis, 1, function(ind) {
    i <- ind[1]
    j <- ind[2]
    merged <- merge(parti1[[i]], parti2[[j]], by = "var")
    r <- data.frame(var = merged$var, min = pmax(merged$min.x, merged$min.y, na.rm = TRUE), 
                    max = pmin(merged$max.x, merged$max.y, na.rm = TRUE))[order(merged$var), ]
    if(any(r$min > r$max)) r <- NULL
    return(r)
  }, simplify = FALSE)
  not.empti <- !sapply(sec.parti, is.null)
  return(list(parti = sec.parti[not.empti], combis = combis[not.empti, ]))
}

affinityCoef <- function(P, Q) {
  sum(sqrt(P * Q))
}

calcP <- function(sec.parti, data, n = "joint") {
  n <- match.arg(n, c("joint", "conditional"))
  
  P.hat <- sapply(sec.parti, function(part) {
    tmp <- data
    for(k in 1:nrow(part)) {
      tmp <- tmp[tmp[, part$var[k]] >= part$min[k] & tmp[, part$var[k]] <= part$max[k], ]
    }
    if(nrow(tmp) == 0) return(rep(0, length(unique(data[, "y"]))))
    return(table(factor(tmp[, "y"], levels = unique(data[, "y"]))) / 
             ifelse(n == "joint", nrow(data), nrow(tmp)))
  })
  return(P.hat)
}

calculateGCR <- function(X1, X2, tune = TRUE, k = 5, n.eval = 100, ...) {
  if(!requireNamespace("rpart", quietly = TRUE)) {
    stop("Package \"rpart\" required for using method GGRL() and NKT().")
  }
  X1 <- as.data.frame(X1)
  X2 <- as.data.frame(X2)
  if(tune){
    if(!requireNamespace("e1071", quietly = TRUE)) {
      stop("Package \"e1071\" required for using method GGRL() and NKT() with tuning.")
    }
    tree1 <- e1071::best.rpart(y ~ ., data = X1, 
                               tunecontrol = e1071::tune.control(sampling = "cross", 
                                                                 cross = k, random = n.eval), 
                               minsplit = 2^(1:7), minbucket = 2^(0:6), 
                               cp = 10^seq(-4, -1, by = 0.001))
    tree2 <- e1071::best.rpart(y ~ ., data = X2, 
                               tunecontrol = e1071::tune.control(sampling = "cross", 
                                                                 cross = k, random = n.eval), 
                               minsplit = 2^(1:7), minbucket = 2^(0:6), 
                               cp = 10^seq(-4, -1, by = 0.001))
  } else {
    tree1 <- rpart::rpart(y ~ ., data = X1, method = "class", ...)
    tree2 <- rpart::rpart(y ~ ., data = X2, method = "class", ...)
  }
  parti1 <- findPartition(tree1, X1, X2)
  parti2 <- findPartition(tree2, X2, X1)
  res.intersect <- intersectPartitions(parti1, parti2)
  return(list(res.intersect = res.intersect, parti1 = parti1, parti2 = parti2))
}


f.a <- function(sec.parti, X1, X2) {
  P1 <- colSums(calcP(sec.parti, X1, n = "joint"))
  P2 <- colSums(calcP(sec.parti, X2, n = "joint"))
  if(!(abs(sum(P1) - 1) < 1e-8 && abs(sum(P2) - 1) < 1e-8)) {
    warning(sprintf(paste0("Something went wrong in the calculation of the attribute ",
                           "space probability distributions. sum(P1) - 1 = %e and ", 
                           "sum(P2) - 1 = %e."), 
                    abs(sum(P1) - 1), abs(sum(P2) - 1) ))
  }
  diffs <- abs(P1 - P2)
  return(diffs)
}

f.s <- function(sec.parti, X1, X2) {
  P1 <- colSums(calcP(sec.parti, X1, n = "joint"))
  P2 <- colSums(calcP(sec.parti, X2, n = "joint"))
  if(!(abs(sum(P1) - 1) < 1e-8 && abs(sum(P2) - 1) < 1e-8)) {
    warning(sprintf(paste0("Something went wrong in the calculation of the attribute ",
                           "space probability distributions. sum(P1) - 1 = %e and ", 
                           "sum(P2) - 1 = %e."), 
                    abs(sum(P1) - 1), abs(sum(P2) - 1) ))
  }
  diffs <- ifelse(P1 + P2 > 0 , 2 * abs(P1 - P2) / (P1 + P2), 0)
  return(diffs)
}


findPartitionCat <- function(tree, X1, X2) {
  f <- tree$frame
  s <- labels(tree, minlength = 52)
  ids <- as.numeric(rownames(f))
  leaves <- ids[f$var == "<leaf>"]
  vars <- setdiff(colnames(X1), "y")
  levs <- lapply(X1[, vars], levels)
  names(levs) <- vars
  in.eq <- rep(list(levs), length(leaves))
  for(i in seq(along = leaves)) {
    id <- leaves[i]
    # go from leaf node to root and save all inequalities of the nodes on this path
    while(id > 1) {
      split <- s[which(ids == id)]
      split <- strsplit(split, "=")
      v <- split[[1]][1]
      val <- trimws(split[[1]][2])
      val <- strsplit(val, ",")[[1]]
      # if left child node: reduce categories for variable to value, 
      # if right child node: reduce categories for variable to all but value
      
      # Problem: if level was not observed in this part of the tree, it is not
      # listed here
      in.eq[[i]][[v]] <- intersect(val, in.eq[[i]][[v]])
      id <- id %/% 2
    }
  }
  return(in.eq)
}

intersectPartitionsCat <- function(parti1, parti2) {
  combis <- expand.grid(1:length(parti1), 1:length(parti2))
  sec.parti <- apply(combis, 1, function(ind) {
    i <- ind[1]
    j <- ind[2]
    r <- lapply(1:length(parti1[[i]]), function(k) intersect(parti1[[i]][[k]], parti2[[j]][[k]]))
    if(any(sapply(r, length) == 0)) r <- NULL
    return(r)
  }, simplify = FALSE)
  not.empti <- !sapply(sec.parti, is.null)
  return(list(parti = sec.parti[not.empti], combis = combis[not.empti, ]))
}

calculateGCRCat <- function(X1, X2, tune = TRUE, k = 5, n.eval = 100, ...) {
  if(!requireNamespace("rpart", quietly = TRUE)) {
    stop("Package \"rpart\" required for using method GGRL() and NKT().")
  }
  X1 <- as.data.frame(X1)
  X2 <- as.data.frame(X2)
  if(tune){
    if(!requireNamespace("e1071", quietly = TRUE)) {
      stop("Package \"e1071\" required for using method GGRL() and NKT() with tuning.")
    }
    tree1 <- e1071::best.rpart(y ~ ., data = X1, 
                               tunecontrol = e1071::tune.control(sampling = "cross", 
                                                                 cross = k, random = n.eval), 
                               minsplit = 2^(1:7), minbucket = 2^(0:6), 
                               cp = 10^seq(-4, -1, by = 0.001))
    tree2 <- e1071::best.rpart(y ~ ., data = X2, 
                               tunecontrol = e1071::tune.control(sampling = "cross", 
                                                                 cross = k, random = n.eval), 
                               minsplit = 2^(1:7), minbucket = 2^(0:6), 
                               cp = 10^seq(-4, -1, by = 0.001))
  } else {
    tree1 <- rpart::rpart(y ~ ., data = X1, method = "class", ...)
    tree2 <- rpart::rpart(y ~ ., data = X2, method = "class", ...)
  }
  parti1 <- findPartitionCat(tree1, X1, X2)
  parti2 <- findPartitionCat(tree2, X2, X1)
  res.intersect <- intersectPartitionsCat(parti1, parti2)
  return(list(res.intersect = res.intersect, parti1 = parti1, parti2 = parti2))
}


calcPCat <- function(sec.parti, data, n = "joint") {
  n <- match.arg(n, c("joint", "conditional"))
  
  P.hat <- sapply(sec.parti, function(part) {
    tmp <- data
    for(k in 1:length(part)) {
      tmp <- tmp[tmp[, colnames(data) != "y"][, k] %in% part[[k]], ]
    }
    if(nrow(tmp) == 0) return(rep(0, length(unique(data[, "y"]))))
    return(table(factor(tmp[, "y"], levels = unique(data[, "y"]))) / 
             ifelse(n == "joint", nrow(data), nrow(tmp)))
  })
  return(P.hat)
}

f.aCat <- function(sec.parti, X1, X2) {
  P1 <- colSums(calcPCat(sec.parti, X1, n = "joint"))
  P2 <- colSums(calcPCat(sec.parti, X2, n = "joint"))
  if(!(abs(sum(P1) - 1) < 1e-8 && abs(sum(P2) - 1) < 1e-8)) {
    warning(sprintf(paste0("Something went wrong in the calculation of the attribute ",
                           "space probability distributions. sum(P1) - 1 = %e and ", 
                           "sum(P2) - 1 = %e. Probably not all level combinations ", 
                           "were present in one of the datasets."), 
                    abs(sum(P1) - 1), abs(sum(P2) - 1) ))
  }
  diffs <- abs(P1 - P2)
  return(diffs)
}

f.sCat <- function(sec.parti, X1, X2) {
  P1 <- colSums(calcPCat(sec.parti, X1, n = "joint"))
  P2 <- colSums(calcPCat(sec.parti, X2, n = "joint"))
  if(!(abs(sum(P1) - 1) < 1e-8 && abs(sum(P2) - 1) < 1e-8)) {
    warning(sprintf(paste0("Something went wrong in the calculation of the attribute ",
                           "space probability distributions. sum(P1) - 1 = %e and ", 
                           "sum(P2) - 1 = %e. Probably not all level combinations ", 
                           "were present in one of the datasets."), 
                    abs(sum(P1) - 1), abs(sum(P2) - 1) ))
  }
  diffs <- ifelse(P1 + P2 > 0 , 2 * abs(P1 - P2) / (P1 + P2), 0)
  return(diffs)
}
