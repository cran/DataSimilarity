\name{MMCM}
\alias{MMCM}
\encoding{UTF-8}
\title{
Multisample Mahalanobis Crossmatch (MMCM) Test
}
\description{
Performs the multisample Mahalanobis crossmatch (MMCM) test (\cite{Mukherjee et al., 2022}).
}
\usage{
MMCM(X1, X2, ..., dist.fun = stats::dist, dist.args = NULL, seed = NULL)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{\dots}{
Optionally more datasets as matrices or data.frames
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
The test is an extension of the \cite{Rosenbaum (2005)} crossmatch test to multiple samples. Its test statistic is the Mahalanobis distance of the observed cross-counts of all pairs of datasets.

It aims to improve the power for large dimensions or numbers of groups compared to another extension, the multisample crossmatch (MCM) test (\cite{Petrie, 2016}).

The observed cross-counts are calculated using the functions \code{\link[nbpMatching]{distancematrix}} and \code{\link[nbpMatching]{nonbimatch}} from the \pkg{nbpMatching} package.

Small values of the test statistic indicate similarity of the datasets, therefore the test rejects the null hypothesis of equal distributions for large values of the test statistic. 
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab Yes \tab Yes \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the test statistic}
\item{p.value}{Asymptotic p value}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
}
\references{
Mukherjee, S., Agarwal, D., Zhang, N. R. and Bhattacharya, B. B. (2022). Distribution-Free Multisample Tests Based on Optimal Matchings With Applications to Single Cell Genomics, Journal of the American Statistical Association, 117(538), 627-638, \doi{10.1080/01621459.2020.1791131}

Rosenbaum, P. R. (2005). An Exact Distribution-Free Test Comparing Two Multivariate Distributions Based on Adjacency. Journal of the Royal Statistical Society. Series B (Statistical Methodology), 67(4), 515-530. 

Petrie, A. (2016). Graph-theoretic multisample tests of equality in distribution for high dimensional data. Computational Statistics & Data Analysis, 96, 145-158, \doi{10.1016/j.csda.2015.11.003}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\note{
In case of ties in the distance matrix, the optimal non-bipartite matching might not be defined uniquely. 
Here, the observations are matched in the order in which the samples are supplied. 
When searching for a match, the implementation starts at the end of the pooled sample. 
Therefore, with many ties (e.g. for categorical data), observations from the first dataset are often matched with ones from the last dataset and so on.
This might affect the validity of the test negatively.
}


\seealso{
\code{\link{Petrie}}, \code{\link{Rosenbaum}}
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
X3 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform MMCM test 
if(requireNamespace("nbpMatching", quietly = TRUE)) {
   MMCM(X1, X2, X3)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ categorical }
\keyword{ k-sample }
\keyword{ graph-based }
\concept{ dataset similarity }