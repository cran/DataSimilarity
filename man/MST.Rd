\name{MST}
\alias{MST}
\title{
Minimum Spanning Tree (MST)
}
\description{
Calculte the edge matrix of a minimum spanning tree based on a distance matrix, used as helper functions in \code{\link{CCS}}, \code{\link{CF}}, \code{\link{FR}}, and \code{\link{ZC}}. 
This function is a wrapper around \code{\link[ade4:mstree]{mstree}}.
}
\usage{
MST(dists, K = 1)
}
\arguments{
  \item{dists}{
Distance matrix as \code{dist} object.
}
  \item{K}{
Component number (default: \code{K = 1}).
}
}
\details{
For more details see \code{\link[ade4:mstree]{mstree}}.
}
\value{
Object of class \code{neig}.
}

\seealso{
\code{\link{CCS}}, \code{\link{CF}}, \code{\link{FR}}, \code{\link{ZC}}
}
\examples{
set.seed(1234)
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
dists <- stats::dist(rbind(X1, X2))
if(requireNamespace("ade4", quietly = TRUE)) {
  # MST
  MST(dists)
  # 5-MST
  MST(dists, K = 5) 
}
}
\keyword{ numeric }
\keyword{ graph-based }
\keyword{ nearest-neighbor-based }
