\name{SH}
\alias{SH}
\encoding{UTF-8}
\title{
Schilling-Henze Nearest Neighbor Test
}
\description{
Performs the Schilling-Henze two-sample test for multivariate data (\cite{Schilling, 1986; Henze, 1988}). 
}
\usage{
SH(X1, X2, K = 1, graph.fun = knn.bf, dist.fun = stats::dist, n.perm = 0, 
    dist.args = NULL, seed = 42)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{K}{
Number of nearest neighbors to consider (default: 1)
}
  \item{graph.fun}{
Function for calculating a similarity graph using the distance matrix on the pooled sample (default: \code{\link{knn.bf}} which searches for the \code{K} nearest neighbors by ranking all pairwise distances, alternative: \code{\link{knn}} which is a wrapper for extracting the edge matrix from the result of \code{\link[dbscan]{kNN}} in \pkg{dbscan}, \code{\link{knn.fast}} which is a wrapper for the approximative KNN implementation \code{\link[FNN]{get.knn}} in \pkg{FNN}, or any other function that calculates the KNN edge matrix from a distance matrix and the number of nearest neighbors \code{K}).
}
  \item{dist.fun}{
Function for calculating a distance matrix on the pooled dataset (default: \code{\link[stats:dist]{stats::dist}}, Euclidean distance).
}
  \item{n.perm}{
Number of permutations for permutation test (default: 0, asymptotic test is performed).
}
  \item{dist.args}{
Named list of further arguments passed to \code{dist.fun}.
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
The test statistic is the proportion of edges connecting points from the same dataset in a \code{K}-nearest neighbor graph calculated on the pooled sample (standardized with expectation and SD under the null). 

Low values of the test statistic indicate similarity of the datasets. Thus, the null hypothesis of equal distributions is rejected for high values. 

For \code{n.perm = 0}, an asymptotic test using the asymptotic normal approximation of the conditional null distribution is performed. For \code{n.perm > 0}, a permutation test is performed. 
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab No \tab No \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the test statistic}
\item{p.value}{Asymptotic or permutation p value}
\item{estimate}{The number of within-sample edges}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
}
\references{
Schilling, M. F. (1986). Multivariate Two-Sample Tests Based on Nearest Neighbors. Journal of the American Statistical Association, 81(395), 799-806. \doi{10.2307/2289012}

Henze, N. (1988). A Multivariate Two-Sample Test Based on the Number of Nearest Neighbor Type Coincidences. The Annals of Statistics, 16(2), 772-783. 

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}
\note{
The default of \code{K=1} is chosen rather arbitrary based on computational speed as there is no good rule for chossing \code{K} proposed in the literature so far. Typical values for \code{K} chosen in the literature are 1 and 5.
}


\seealso{
\code{\link{knn}}, \code{\link{BQS}}, \code{\link{FR}}, \code{\link{CF}}, \code{\link{CCS}}, \code{\link{ZC}} for other graph-based tests, 
\code{\link{FR_cat}}, \code{\link{CF_cat}}, \code{\link{CCS_cat}}, and \code{\link{ZC_cat}} for versions of the test for categorical data
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform Schilling-Henze test
SH(X1, X2)
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ graph-based }
\keyword{ nearest-neighbor-based }
\concept{ dataset similarity }
