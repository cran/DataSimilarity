\name{gTests}
\alias{gTests}
\encoding{UTF-8}
\title{
Graph-Based Tests
}
\description{
Performs the edge-count two-sample tests for multivariate data implementated in \code{\link[gTests]{g.tests}} from the \pkg{gTests} package. This function is inteded to be used e.g. in comparison studies where all four graph-based tests need to be calculated at the same time. Since large parts of the calculation coincide, using this function should be faster than computing all four statistics individually.
}
\usage{
gTests(X1, X2, dist.fun = stats::dist, graph.fun = MST, 
        n.perm = 0, dist.args = NULL, graph.args = NULL,
        maxtype.kappa = 1.14,  seed = NULL)
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
Named list of further arguments passed to \code{dist.fun}.
}
  \item{graph.args}{
Named list of further arguments passed to \code{graph.fun}.
}
  \item{maxtype.kappa}{
Parameter \eqn{\kappa} of the maxtype test (default: 1.14). See \code{\link{ZC}}.
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
}
\details{
The original, weighted, generalized and maxtype edge-count test are performed.

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
A list with the following components:
\item{statistic }{Observed values of the test statistics}
\item{p.value}{Asymptotic or permutation p values}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
}
\references{
Friedman, J. H., and Rafsky, L. C. (1979). Multivariate Generalizations of the Wald-Wolfowitz and Smirnov Two-Sample Tests. The Annals of Statistics, 7(4), 697-717. 

Chen, H. and Friedman, J.H. (2017). A New Graph-Based Two-Sample
Test for Multivariate and Object Data. Journal of the American Statistical Association, 112(517), 397-409. \doi{10.1080/01621459.2016.1147356}

Chen, H., Chen, X. and Su, Y. (2018). A Weighted Edge-Count Two-Sample
Test for Multivariate and Object Data. Journal of the American Statistical Association, 113(523), 1146-1155, \doi{10.1080/01621459.2017.1307757}

Zhang, J. and Chen, H. (2022). Graph-Based Two-Sample Tests for Data with Repeated Observations. Statistica Sinica 32, 391-415, \doi{10.5705/ss.202019.0116}.

Chen, H., and Zhang, J. (2017). gTests: Graph-Based Two-Sample Tests. R package version 0.2, \url{https://CRAN.R-project.org/package=gTests}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\seealso{
\code{\link{FR}} for the original edge-count test, \code{\link{CF}} for the generalized edge-count test, \code{\link{CCS}} for the weighted edge-count test, and \code{\link{ZC}} for the maxtype edge-count test,
\code{\link{gTests_cat}}, \code{\link{CCS_cat}}, \code{\link{FR_cat}}, \code{\link{CF_cat}}, and \code{\link{ZC_cat}} for versions of the tests for categorical data
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform edge-count tests
if(requireNamespace("gTests", quietly = TRUE)) {
  gTests(X1, X2)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ graph-based }
\concept{ dataset similarity }
