\name{CCS}
\alias{CCS}
\encoding{UTF-8}
\title{
Weighted Edge-Count Two-Sample Test
}
\description{
Performs the weighted edge-count two-sample test for multivariate data proposed by \cite{Chen, Chen and Su (2018)}. The test is intended for comparing two samples with unequal sample sizes. The implementation here uses the \code{\link[gTests]{g.tests}} implementation from the \pkg{gTests} package.
}
\usage{
CCS(X1, X2, dist.fun = stats::dist, graph.fun = MST, n.perm = 0, 
    dist.args = NULL, graph.args = NULL, seed = 42)
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
  \item{seed}{
Random seed (default: 42)
}
}
\details{
The test is an enhancement of the Friedman-Rafsky test (original edge-count test) that aims at improving the test's power for unequal sample sizes by weighting. The test statistic is given as 
\deqn{Z_w = \frac{R_w - \text{E}_{H_0}(R_w)}{\sqrt{\text{Var}_{H_0}(R_w)}}, \text{ where}}
\deqn{R_w = \frac{n_1}{n_1+n_2} R_1 + \frac{n_2}{n_1+n_2} R_2}
and \eqn{R_1} and \eqn{R_2} denote the number of edges in the similarity graph connecting points within the first and second sample \eqn{X_1} and \eqn{X_2}, respectively. 

High values of the test statistic indicate dissimilarity of the datasets as the number of edges connecting points within the same sample is high meaning that points are more similar within the datasets than between the datasets.
 
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
Chen, H., Chen, X. and Su, Y. (2018). A Weighted Edge-Count Two-Sample Test for Multivariate and Object Data. Journal of the American Statistical Association, 113(523), 1146-1155, \doi{10.1080/01621459.2017.1307757}

Chen, H., and Zhang, J. (2017). gTests: Graph-Based Two-Sample Tests. R package version 0.2, \url{https://CRAN.R-project.org/package=gTests}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link{FR}} for the original edge-count test, \code{\link{CF}} for the generalized edge-count test, \code{\link{ZC}} for the maxtype edge-count test, \code{\link{gTests}} for performing all these edge-count tests at once, \code{\link{SH}} for performing the Schilling-Henze nearest neighbor test,
\code{\link{CCS_cat}}, \code{\link{FR_cat}}, \code{\link{CF_cat}}, \code{\link{ZC_cat}}, and \code{\link{gTests_cat}} for versions of the test for categorical data
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform weighted edge-count test
if(requireNamespace("gTests", quietly = TRUE)) {
  CCS(X1, X2)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ graph-based }
\concept{ dataset similarity }
