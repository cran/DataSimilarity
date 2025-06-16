\name{kerTests}
\alias{kerTests}
\encoding{UTF-8}
\title{
Generalized Permutation-Based Kernel (GPK) Two-Sample Test
}
\description{
Performs the generalized permutation-based kernel two-sample tests proposed by \cite{Song and Chen (2021)}. The implementation here uses the \code{\link[kerTests]{kertests}} implementation from the \pkg{kerTests} package. This function is inteded to be used e.g. in comparison studies where all four test statistics need to be calculated at the same time. Since large parts of the calculation coincide, using this function should be faster than computing all four statistics individually.
}
\usage{
kerTests(X1, X2, n.perm = 0, sigma = findSigma(X1, X2), r1 = 1.2, 
          r2 = 0.8, seed = NULL)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{n.perm}{
Number of permutations for permutation test (default: 0, fast test is performed). For \code{fast = FALSE}, only the permutation test and no asymptotic test is available. For \code{fast = TRUE}, either an asymptotic test (set \code{n.perm = 0}) and a permutation test (set \code{n.perm} > 0) can be performed.
}
  \item{sigma}{
Bandwidth parameter of the kernel. By default the median heuristic is used to choose \code{sigma}.
}
  \item{r1}{
Constant in the test statistic \eqn{Z_{W, r1}} for the fast test (default: 1.2, proposed in original article)
}
  \item{r2}{
Constant in the test statistic \eqn{Z_{W, r2}} for the fast test (default: 0.8, proposed in original article)
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
}
\details{
The GPK test is motivated by the observation that the MMD test performs poorly for detecting differences in variances. The unbiased MMD\eqn{^2} estimator for a given kernel function \eqn{k} can be written as 
\deqn{\text{MMD}_u^2 = \alpha + \beta - 2\gamma, \text{ where}}
\deqn{\alpha = \frac{1}{n_1^2 - n_1}\sum_{i=1}^{n_1}\sum_{j=1, j\ne i}^{n_1} k(X_{1i}, X_{1j}),}
\deqn{\beta = \frac{1}{n_2^2 - n_2}\sum_{i=1}^{n_2}\sum_{j=1, j\ne i}^{n_2} k(X_{2i}, X_{2j}),}
\deqn{\gamma = \frac{1}{n_1 n_2}\sum_{i=1}^{n_1}\sum_{j=1}^{n_2} k(X_{1i}, X_{2j}).}
The GPK test statistic is defined as
\deqn{\text{GPK} = (\alpha - \text{E}(\alpha), \beta - \text{E}(\beta))\Sigma^{-1} \binom{\alpha - \text{E}(\alpha)}{\beta - \text{E}(\beta)}}
\deqn{= Z_{W,1}^2 + Z_D^2\text{ with}}
\deqn{Z_{W,r} = \frac{W_r - \text{E}(W_r)}{\sqrt{\text{Var}(W_r)}}, W_r = r\frac{n_1 \alpha}{n_1 + n_2}, }
\deqn{Z_D = \frac{D - \text{E}(D)}{\sqrt{\text{Var}(D)}}, D = n_1(n_1 - 1)\alpha - n_2(n_2 - 1)\beta,}
where the expectations are calculated under the null and \eqn{\Sigma} is the covariance matrix of \eqn{\alpha} and \eqn{\beta} under the null. 

The asymptotic null distribution for GPK is unknown. Therefore, only a permutation test can be performed.

For \eqn{r \ne 1}, the asymptotic null distribution of \eqn{Z_{W,r}} is normal, but for \eqn{r} further away from 1, the test performance decreases. Therefore, \eqn{r_1 = 1.2} and \eqn{r_2 = 0.8} are proposed as a compromise.  

For the fast GPK test, three (asymptotic or permutation) tests based on \eqn{Z_{W, r1}}, \eqn{Z_{W, r2}} and \eqn{Z_{D}} are concucted and the overall p value is calculated as 3 times the minimum of the three p values. 

For the fast MMD test, only the two asymptotic tests based on \eqn{Z_{W, r1}}, \eqn{Z_{W, r2}} are used and the p value is 2 times the minimum of the two p values. This is an approximation of the MMD-permutation test, see \code{\link{MMD}}.

This implementation is a wrapper function around the function \code{\link[kerTests]{kertests}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see the \code{\link[kerTests]{kertests}}. 
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
\item{null.value }{Needed for pretty printing of results}
\item{alternative }{Needed for pretty printing of results}
\item{method }{Description of the test}
\item{data.name }{Needed for pretty printing of results}
}
\references{
Song, H. and Chen, H. (2021). Generalized Kernel Two-Sample Tests. arXiv preprint. \doi{10.1093/biomet/asad068}.

Song H, Chen H (2023). kerTests: Generalized Kernel Two-Sample Tests. R package version 0.1.4, \url{https://CRAN.R-project.org/package=kerTests}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\seealso{
\code{\link{GPK}}, \code{\link{MMD}}
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform GPK tests
if(requireNamespace("kerTests", quietly = TRUE)) {
  kerTests(X1, X2, n.perm = 100)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ kernel-based }
\keyword{ MMD }
\concept{ dataset similarity }
