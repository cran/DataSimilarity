\name{BQS}
\alias{BQS}
\encoding{UTF-8}
\title{
Barakat et al. (1996) Two-Sample Test
}
\description{
Performs the nearest-neighbor-based multivariate two-sample test of \cite{Barakat et al. (1996).}
}
\usage{
BQS(X1, X2, dist.fun = stats::dist, n.perm = 0, dist.args = NULL, seed = 42)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{dist.fun}{
Function for calculating a distance matrix on the pooled dataset (default: \code{stats::dist}, Euclidean distance).
}
  \item{n.perm}{
Number of permutations for permutation test (default: 0, no test is performed).
}
  \item{dist.args}{
Named list of further arguments passed to \code{dist.fun} (default: \code{NULL}).
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
The test is an extension of the \cite{Schilling (1986)} and \cite{Henze (1988)} 
neighbor test that bypasses choosing the number of nearest neighbors to consider. 
The Schilling-Henze test statistic is the proportion of edges connecting points 
from the same dataset in a \code{K}-nearest neighbor graph calculated on the pooled sample (standardized with expectation and SD under the null). 
\cite{Barakat et al. (1996)} take the weighted sum of the Schilling-Henze test 
statistics for \eqn{K = 1,\dots,N-1}, where \eqn{N} denotes the pooled sample size. 

As for the Schilling-Henze test, low values of the test statistic indicate similarity of the datasets. Thus, the null hypothesis of equal distributions is rejected for high values. 
A permutation test is performed if \code{n.perm} is set to a positive number. 
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
\item{p.value}{Permutation p value (if \code{n.perm} > 0)}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
}
\references{
Barakat, A.S., Quade, D. and Salama, I.A. (1996), Multivariate Homogeneity Testing Using an Extended Concept of Nearest Neighbors. Biom. J., 38: 605-612. \doi{10.1002/bimj.4710380509}

Schilling, M. F. (1986). Multivariate Two-Sample Tests Based on Nearest Neighbors. Journal of the American Statistical Association, 81(395), 799-806. \doi{10.2307/2289012}

Henze, N. (1988). A Multivariate Two-Sample Test Based on the Number of Nearest Neighbor Type Coincidences. The Annals of Statistics, 16(2), 772-783. 

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\seealso{
\code{\link{SH}}, \code{\link{FR}}, \code{\link{CF}}, \code{\link{CCS}}, \code{\link{ZC}} for other graph-based tests, 
\code{\link{FR_cat}}, \code{\link{CF_cat}}, \code{\link{CCS_cat}}, and \code{\link{ZC_cat}} for versions of the test for categorical data

}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform Barakat et al. test
BQS(X1, X2)
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ nearest-neighbor-based}
\keyword{ graph-based }
\concept{ dataset similarity }
