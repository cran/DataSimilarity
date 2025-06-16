\name{ZC}
\alias{ZC}
\encoding{UTF-8}
\title{
Maxtype Edge-Count Test
}
\description{
Performs the maxtype edge-count two-sample test for multivariate data proposed by \cite{Zhang and Chen (2017)}. The implementation here uses the \code{\link[gTests]{g.tests}} implementation from the \pkg{gTests} package.
}
\usage{
ZC(X1, X2, dist.fun = stats::dist, graph.fun = MST, n.perm = 0, 
    dist.args = NULL, graph.args = NULL, maxtype.kappa = 1.14, seed = NULL)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{dist.fun}{
Function for calculating a distance matrix on the pooled dataset (default: \code{\link[stats:dist]{stats::dist}}, Euclidean distance).
}
  \item{graph.fun}{
Function for calculating a similarity graph using the distance matrix on the pooled sample (default: \code{\link{MST}}, Minimum Spanning Tree).
}
  \item{n.perm}{
Number of permutations for permutation test (default: 0, asymptotic test is performed).
}
  \item{dist.args}{
Named list of further arguments passed to \code{dist.fun} (default: \code{NULL}).
}
  \item{graph.args}{
Named list of further arguments passed to \code{graph.fun} (default: \code{NULL}).
}
  \item{maxtype.kappa}{
Parameter \eqn{\kappa} of the test (default: 1.14). See details.
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
}
\details{
The test is an enhancement of the Friedman-Rafsky test (original edge-count test)  that aims at detecting both location and scale alternatives and is more flexible than the generalized edge-count test of Chen and Friedman (2017). The test statistic is the maximum of two statistics. The first statistic ist the weighted edge-count statistic multiplied by a factor \eqn{\kappa}. The second statistic is the absolute value of the standardized difference of edge-counts within the first and within the second sample. 

Low values of the test statistic indicate similarity of the datasets. Thus, the null hypothesis of equal distributions is rejected for high values. 

For \code{n.perm = 0}, an asymptotic test using the asymptotic normal approximation of the null distribution is performed. For \code{n.perm > 0}, a permutation test is performed. 

This implementation is a wrapper function around the function \code{\link[gTests]{g.tests}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see the \code{\link[gTests]{g.tests}}. 
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
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
}
\references{
Zhang, J. and Chen, H. (2022). Graph-Based Two-Sample Tests for Data with Repeated Observations. Statistica Sinica 32, 391-415, \doi{10.5705/ss.202019.0116}.

Chen, H., and Zhang, J. (2017). gTests: Graph-Based Two-Sample Tests. R package version 0.2, \url{https://CRAN.R-project.org/package=gTests}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\seealso{
\code{\link{FR}} for the original edge-count test, \code{\link{CF}} for the generalized edge-count test, \code{\link{CCS}} for the weighted edge-count test, \code{\link{gTests}} for performing all these edge-count tests at once, \code{\link{SH}} for performing the Schilling-Henze nearest neighbor test, 
\code{\link{CCS_cat}}, \code{\link{FR_cat}}, \code{\link{CF_cat}}, \code{\link{ZC_cat}}, and \code{\link{gTests_cat}} for versions of the test for categorical data
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform maxtype edge-count test
if(requireNamespace("gTests", quietly = TRUE)) {  
  # Using MST
  ZC(X1, X2)
  # Using 5-MST
  ZC(X1, X2, graph.args = list(K = 5))
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ graph-based }
\concept{ dataset similarity }
