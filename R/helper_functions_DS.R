################################################################################
##                                HELPER FUNCTIONS                            ##
##                                                                            ##
################################################################################
######### Computing Rank Energy Statistic #########
# source: https://github.com/NabarunD/MultiDistFree
computeStatistic <- function(x, y, m = nrow(x), n = nrow(y), dim = ncol(x), 
                             gridch = randtoolbox::torus(m + n, dim)) { 
  # torus generates matrix with random numbers in (0, 1) (according to Halton sequence?)
  colnames(x) <- colnames(y) <- paste0("X", 1:ncol(x))
  comdata <- rbind(x, y)
  # calculate rank assignment: 
  distmat <- matrix(0, nrow = m + n, ncol = m + n)
  t.gridch <- t(gridch)
  for(i in 1:(m + n)){
    distmat[i, ] <- apply((comdata[i, ] - t.gridch), 2 , function(x) sum(x^2))
  }
  assignmentFUN <- clue::solve_LSAP(distmat)
  # solve_LSAP finds an optimal assignment of rows to columns
  assignmentSOL <- cbind(seq_along(assignmentFUN), assignmentFUN)
  # calculate energy statistic on ranks: 
  randenergySTAT <- energy::eqdist.etest(gridch[assignmentSOL[ , 2], ], 
                                         sizes = c(m, n), R = 1)
  return(randenergySTAT$statistic)
}

######### Generating universal distribution ##########
# source: https://github.com/NabarunD/MultiDistFree
genNullDist <- function(M, N, dim, niter = 30000, fixgrid = randtoolbox::torus(M + N, dim)) {
  tstat <- numeric(niter)
  for(i in 1:niter) {
    ranper <- sample(M + N)
    tstat[i] <- energy::eqdist.etest(fixgrid[ranper, ], sizes = c(M, N), R = 1)$statistic
  }
  return(tstat)
}