\name{gTests_cat}
\alias{gTests_cat}
\encoding{UTF-8}
\title{
Graph-Based Tests for Discrete Data
}
\description{
Performs the edge-count two-sample tests for multivariate categorical data implementated in \code{\link[gTests]{g.tests}} from the \pkg{gTests} package. This function is inteded to be used e.g. in comparison studies where all four graph-based tests need to be calculated at the same time. Since large parts of the calculation coincide, using this function should be faster than computing all four statistics individually.
}
\usage{
gTests_cat(X1, X2, dist.fun = function(x, y) sum(x != y), graph.type = "mstree", 
            K = 1, n.perm = 0, maxtype.kappa = 1.14, seed = 42)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{dist.fun}{
Function for calculating a distance matrix on the pooled dataset (default: Number of unequal components).
}
  \item{graph.type}{
Character specifying which similarity graph to use. Possible options are \code{"mstree"} (default, Minimum Spanning Tree) and \code{"nnlink"} (Nearest Neighbor Graph).
}
  \item{K}{
Parameter for graph (default: 1). If \code{graph.type = "mstree"}, a \code{K}-MST is constructed (\code{K=1} is the classical MST). If \code{graph.type = "nnlink"}, \code{K} gives the number of neighbors considered in the \code{K}-NN graph.
}
  \item{n.perm}{
Number of permutations for permutation test (default: 0, asymptotic test is performed).
}
  \item{maxtype.kappa}{
Parameter \eqn{\kappa} of the maxtype test (default: 1.14). See \code{\link{ZC}}.
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
The original, weighted, generalized and maxtype edge-count test are performed.

For discrete data, the similarity graph used in the test is not necessarily unique. This can be solved by either taking a union ("u") of all optimal similarity graphs or averaging ("a") the test statistics over all optimal similarity graphs. For details, see \cite{Zhang and Chen (2022)}. Both options are performed here. 

For \code{n.perm = 0}, an asymptotic test using the asymptotic normal approximation of the null distribution is performed. For \code{n.perm > 0}, a permutation test is performed. 

This implementation is a wrapper function around the function \code{\link[gTests]{g.tests}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see the \code{\link[gTests]{g.tests}}. 
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab No \tab Yes \tab No \cr
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

Chen, H. and Friedman, J.H. (2017). A New Graph-Based Two-Sample Test for Multivariate and Object Data. Journal of the American Statistical Association, 112(517), 397-409. \doi{10.1080/01621459.2016.1147356}

Chen, H., Chen, X. and Su, Y. (2018). A Weighted Edge-Count Two-Sample Test for Multivariate and Object Data. Journal of the American Statistical Association, 113(523), 1146-1155, \doi{10.1080/01621459.2017.1307757}

Zhang, J. and Chen, H. (2022). Graph-Based Two-Sample Tests for Data with Repeated Observations. Statistica Sinica 32, 391-415, \doi{10.5705/ss.202019.0116}.

Chen, H., and Zhang, J. (2017). gTests: Graph-Based Two-Sample Tests. R package version 0.2, \url{https://CRAN.R-project.org/package=gTests}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\seealso{
\code{\link{FR_cat}} for the original edge-count test, \code{\link{CF_cat}} for the generalized edge-count test, \code{\link{CCS_cat}} for the weighted edge-count test, and \code{\link{ZC_cat}} for the maxtype edge-count test, 
\code{\link{gTests}}, \code{\link{FR}}, \code{\link{CF}}, \code{\link{CCS}}, and \code{\link{ZC}} for versions of the test for continuous data
}
\examples{
# Draw some data
X1cat <- matrix(sample(1:4, 300, replace = TRUE), ncol = 3)
X2cat <- matrix(sample(1:4, 300, replace = TRUE, prob = 1:4), ncol = 3)
# Perform edge-count tests
if(requireNamespace("gTests", quietly = TRUE)) {
  gTests_cat(X1cat, X2cat)
}
}
\keyword{ htest }
\keyword{ categorical }
\keyword{ graph-based }
\concept{ dataset similarity }