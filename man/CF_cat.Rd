\name{CF_cat}
\alias{CF_cat}
\encoding{UTF-8}
\title{
Generalized Edge-Count Test for Discrete Data
}
\description{
Performs the generalized edge-count two-sample test for multivariate data proposed by \cite{Chen and Friedman (2017)}. The implementation here uses the \code{\link[gTests]{g.tests}} implementation from the \pkg{gTests} package.
}
\usage{
CF_cat(X1, X2, dist.fun, agg.type, graph.type = "mstree", K = 1, n.perm = 0, 
        seed = NULL)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{dist.fun}{
Function for calculating the distance of two observations. Should take two vectors as its input and return their distance as a scalar value.
}
  \item{agg.type}{
Character giving the method for aggregating over possible similarity graphs. Options are \code{"u"} for union of possible similarity graphs and \code{"a"} for averaging over test statistics calculated on possible similarity graphs. 
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
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
}
\details{
The test is an enhancement of the Friedman-Rafsky test (original edge-count test) that aims at detecting both location and scale alternatives. The test statistic is given as 
\deqn{S = (R_1 - \mu_1, R_2 - \mu_2)\Sigma^{-1} \binom{R_1 - \mu_1}{R_2 - \mu_2}, \text{ where}}
\eqn{R_1} and \eqn{R_2} denote the number of edges in the similarity graph connecting points within the first and second sample \eqn{X_1} and \eqn{X_2}, respectively, \eqn{\mu_1 = \text{E}_{H_0}(R_1)}, \eqn{\mu_2 = \text{E}_{H_0}(R_2)} and \eqn{\Sigma} is the covariance matrix of \eqn{R_1} and \eqn{R_2} under the null. 

For discrete data, the similarity graph used in the test is not necessarily unique. This can be solved by either taking a union of all optimal similarity graphs or averaging the test statistics over all optimal similarity graphs. For details, see \cite{Zhang and Chen (2022)}. 

High values of the test statistic indicate dissimilarity of the datasets as the number of edges connecting points within the same sample is high meaning that points are more similar within the datasets than between the datasets.

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
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the test statistic}
\item{parameter }{Degrees of freedom for \eqn{\chi^2} distribution under \eqn{H_0} (only for asymptotic test)}
\item{p.value}{Asymptotic or permutation p value}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
}
\references{
Chen, H. and Friedman, J.H. (2017). A New Graph-Based Two-Sample
Test for Multivariate and Object Data. Journal of the American Statistical Association, 112(517), 397-409. \doi{10.1080/01621459.2016.1147356}

Zhang, J. and Chen, H. (2022). Graph-Based Two-Sample Tests for Data with Repeated Observations. Statistica Sinica 32, 391-415, \doi{10.5705/ss.202019.0116}.

Chen, H., and Zhang, J. (2017). gTests: Graph-Based Two-Sample Tests. R package version 0.2, \url{https://CRAN.R-project.org/package=gTests}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149}
}

\seealso{
\code{\link{FR_cat}} for the original edge-count test, \code{\link{CCS_cat}} for the weighted edge-count test, \code{\link{ZC_cat}} for the maxtype edge-count test, \code{\link{gTests_cat}} for performing all these edge-count tests at once, 
\code{\link{CCS}}, \code{\link{FR}}, \code{\link{CF}}, \code{\link{ZC}}, and \code{\link{gTests}} for versions of the tests for continuous data, and \code{\link{SH}} for performing the Schilling-Henze nearest neighbor test
}
\examples{
set.seed(1234)
# Draw some data
X1cat <- matrix(sample(1:4, 300, replace = TRUE), ncol = 3)
X2cat <- matrix(sample(1:4, 300, replace = TRUE, prob = 1:4), ncol = 3)
# Perform generalized edge-count test
if(requireNamespace("gTests", quietly = TRUE)) {
  CF_cat(X1cat, X2cat, dist.fun = function(x, y) sum(x != y), agg.type = "a")
}
}
\keyword{ htest }
\keyword{ categorical }
\keyword{ graph-based }
\concept{ dataset similarity }