\name{Petrie}
\alias{Petrie}
\encoding{UTF-8}
\title{
Multisample Crossmatch (MCM) Test
}
\description{
Performs the multisample crossmatch (MCM) test (\cite{Petrie, 2016}).
}
\usage{
Petrie(X1, X2, ..., dist.fun = stats::dist, dist.args = NULL, seed = NULL)
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
The test is an extension of the \cite{Rosenbaum (2005)} crossmatch test to multiple samples that uses the crossmatch count of all pairs of samples. 

The observed cross-counts are calculated using the functions \code{\link[nbpMatching]{distancematrix}} and \code{\link[nbpMatching]{nonbimatch}} from the \pkg{nbpMatching} package.

High values of the multisample crossmatch statistic indicate similarity between the datasets. Thus, the test rejects the null hypothesis of equal distributions for low values of the test statistic. 
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
\item{estimate}{Observed multisample edge-count}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{stderr}{Standard deviation under the null}
\item{mu0}{Expectation under the null}
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
\code{\link{MMCM}}, \code{\link{Rosenbaum}}
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform MCM test 
if(requireNamespace("nbpMatching", quietly = TRUE)) {
   Petrie(X1, X2)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ categorical }
\keyword{ k-sample }
\keyword{ graph-based }
\concept{ dataset similarity }