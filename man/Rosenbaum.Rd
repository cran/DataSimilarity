\name{Rosenbaum}
\alias{Rosenbaum}
\encoding{UTF-8}
\title{
Rosenbaum Crossmatch Test
}
\description{
Performs the \cite{Rosenbaum (2005)} crossmatch two-sample test. The implementation here uses the \code{\link[crossmatch]{crossmatchtest}} implementation from the \pkg{crossmatch} package.
}
\usage{
Rosenbaum(X1, X2, exact = FALSE, dist.fun = stats::dist, dist.args = NULL, seed = NULL)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{exact}{
Should the exact null distribution be used? (default: \code{FALSE}). The exact distribution calculation is only possible for a pooled sample size of less than 340 due to numerical reasons. If \code{exact = FALSE} or the sample size limit is reached, an asymptotic test is performed.
}
  \item{dist.fun}{
Function for calculating a distance matrix on the pooled dataset (default: \code{\link[stats:dist]{stats::dist}}, Euclidean distance).
}
  \item{dist.args}{
Named list of further arguments passed to \code{dist.fun} (default: \code{NULL}).
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
}
\details{
The test statistic is calculated as the standardized number of edges connecting points from different samples in a non-bipartite matching. The non-bipartite matching is calculated using the implementation from the \code{\link[nbpMatching:nbpMatching-package]{nbpMatching}} package. The null hypothesis of equal distributions is rejected for small values of the test statistic as high values of the crossmatch statistic indicate similarity between datasets. 

This implementation is a wrapper function around the function \code{\link[crossmatch]{crossmatchtest}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see \code{\link[crossmatch]{crossmatchtest}}. 
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
\item{p.value}{Asymptotic p value}
\item{estimate}{Unstandardized crossmatch count}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{stderr}{Standard deviation of the test statistic under the null}
\item{mu0}{Expectation of the test statistic under the null}
}
\references{
Rosenbaum, P.R. (2005), An exact distribution-free test comparing two multivariate distributions based on adjacency, Journal of the Royal Statistical Society: Series B (Statistical Methodology), 67, 4, 515-530. 

Heller, R., Small, D., Rosenbaum, P. (2024). crossmatch: The Cross-match Test. R package version 1.4, \url{https://CRAN.R-project.org/package=crossmatch}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\seealso{
\code{\link{FR}}, \code{\link{CF}}, \code{\link{CCS}}, \code{\link{ZC}}

\code{\link{Petrie}}, \code{\link{MMCM}} for multi-sample versions of the test
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform crossmatch test
if(requireNamespace("crossmatch", quietly = TRUE)) {
  Rosenbaum(X1, X2)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ graph-based }
\concept{ dataset similarity }