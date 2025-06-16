\name{knn}
\alias{knn}
\alias{knn.fast}
\alias{knn.bf}
\title{
K-Nearest Neighbor Graph
}
\description{
Calculte the edge matrix of a K-nearest neighbor graph based on a distance matrix, used as helper functions in \code{\link{SH}}
}
\usage{
knn(dists, K = 1)
knn.fast(dists, K = 1)
knn.bf(dists, K = 1)
}
\arguments{
  \item{dists}{
Distance matrix
}
  \item{K}{
Number of nearest neighbors to consider (default: \code{K = 1})
}
}
\details{
\code{knn.bf} uses brute force to find the \code{K} nearest neighbors but does not require additional packages. 
\code{knn} uses the \code{\link[dbscan:kNN]{kNN}} implementation of the \pkg{dbscan} package. 
\code{knn.fast} uses the \code{\link[FNN:get.knn]{get.knn}} implementation of the \pkg{FNN} package that uses a kd-tree for fast K-nearest neighbor search. 

}
\value{
The edge matrix of the K-nearest neighbor graph. 
The first column gives the index of the first node of each edge. 
The second column gives the index of the second node of each edge.
Thus, the second entry of each row is one of the K nearest neighbors of the first entry in each row.
}

\seealso{
\code{\link{SH}}
}
\examples{
set.seed(1234)
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
dists <- stats::dist(rbind(X1, X2))
# Nearest neighbor graph
if(requireNamespace("dbscan", quietly = TRUE)) {
  knn(dists)
}
if(requireNamespace("FNN", quietly = TRUE)) {
  knn.fast(dists)
}
knn.bf(dists)
# 5-Nearest neighbor graph
if(requireNamespace("dbscan", quietly = TRUE)) {
  knn(dists, K = 5)
}
if(requireNamespace("FNN", quietly = TRUE)) {
  knn.fast(dists, K = 5)
}
knn.bf(dists, K = 5)
}
\keyword{ numeric }
\keyword{ graph-based }
\keyword{ nearest-neighbor-based }
