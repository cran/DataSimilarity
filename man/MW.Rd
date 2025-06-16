\name{MW}
\alias{MW}
\encoding{UTF-8}
\title{
Nonparametric Graph-Based LP (GLP) Test
}
\description{
Performs the nonparametric graph-based LP (GLP) multisample test proposed by \cite{Mokhopadhyay and Wang (2020)}. The implementation here uses the \code{\link[LPKsample]{GLP}} implementation from the \pkg{LPKsample} package.
}

\usage{
MW(X1, X2, ..., sum.all = FALSE, m.max = 4, components = NULL, alpha = 0.05, 
    c.poly = 0.5, clust.alg = "kmeans", n.perm = 0, combine.criterion = "kernel", 
    multiple.comparison = TRUE, compress.algorithm = FALSE, nbasis = 8, seed = NULL)
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
  \item{sum.all}{
Should all components be summed up for calculating the test statistic? (default: \code{FALSE}, only significant components are summed up)
}
  \item{m.max}{
Maximum order of LP components to investigate (default: 4)
}
  \item{components}{
Vector specifying which components to test. If \code{components} is not \code{NULL} (default), only the specified components are examined and \code{m.max} is ignored.
}
  \item{alpha}{
Significance level \eqn{\alpha} (default: 0.05)
}
  \item{c.poly}{
Parameter for polynomial kernel (default: 0.5)
}
  \item{clust.alg}{
Character specifying the cluster algorithm used in graph community detection. possible options are \code{"kmeans"} (default) and \code{"mclust"}.
}
  \item{n.perm}{
Number of permutations for permutation test (default: 0, asymptotic test is performed).
}
  \item{combine.criterion}{
Character specifying how to obtain the overall test result based on the component-wise results. Possible options are \code{"kernel"} meaning that an overall kernel \eqn{W} is computed based on the significant components and the LP graph test is run on \eqn{W}, and \code{"pvalue"} which uses Fisher's method to combine the p values from each component.
}
  \item{multiple.comparison}{
Should an adjustment for multiple comparisons be used when determining which components are significant? (default: \code{TRUE})
}
  \item{compress.algorithm}{
Should smooth compression of Laplacian spectra be used for testing? (default: \code{FALSE}). It is recommended to set this to \code{TRUE} for large sample sizes.
}
  \item{nbasis}{
Number of bases used for approximation when \code{compress.algorithm = TRUE} (default: 8)
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
}
\details{
The GLP statistic is based on learning an LP graph kernel using a pre-specified number of LP components and performing clustering on the eigenvectors of the Laplacian matrix for this learned kernel. The cluster assignment is tested for association with the true dataset memberships for each component of the LP graph kernel. The results are combined by either constructing a super-kernel using specific components and performing the cluster and test step again or by using the combination of the significant components after adjustment for multiple testing. 

Small values of the GLP statistic indicate dataset similarity. Therefore, the test rejects for large values. 
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab No \tab Yes \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the GLP test statistic}
\item{p.value}{Asymptotic or permutation overall p value}
\item{null.value }{Needed for pretty printing of results}
\item{alternative }{Needed for pretty printing of results}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{alternative }{The alternative hypothesis}
}
\references{
Mukhopadhyay, S. and Wang, K. (2020). A nonparametric approach to high-dimensional k-sample comparison problems, Biometrika, 107(3), 555-572, \doi{10.1093/biomet/asaa015}

Mukhopadhyay, S. and Wang, K. (2019). Towards a unified statistical theory of spectralgraph analysis, \doi{10.48550/arXiv.1901.07090}

Mukhopadhyay, S., Wang, K. (2020). LPKsample: LP Nonparametric High Dimensional K-Sample Comparison. R package version 2.1, \url{https://CRAN.R-project.org/package=LPKsample}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\note{
When \code{sum.all = FALSE} and no components are significant, the test statistic value is always set to zero. 

Note that the implementation cannot handle univariate data.
}

\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform GLP test 
if(requireNamespace("LPKsample", quietly = TRUE)) {
  MW(X1, X2, n.perm = 100)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ k-sample }
\keyword{ graph-based }
\concept{ dataset similarity }