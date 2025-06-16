\name{MMD}
\alias{MMD}
\encoding{UTF-8}
\title{
Maximum Mean Discrepancy (MMD) Test
}
\description{
Performs a two-sample test based on the maximum mean discrepancy (MMD) using either, the Rademacher or the asmyptotic bounds or a permutation testing procedure. The implementation adds a permutation test to the \code{\link[kernlab]{kmmd}} implementation from the \pkg{kernlab} package.
}
\usage{
MMD(X1, X2, n.perm = 0, alpha = 0.05, asymptotic = FALSE, replace = TRUE, 
    n.times = 150, frac = 1, seed = NULL, ...)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{n.perm}{
Number of permutations for permutation test (default: 0, asymptotic test is performed).
}
  \item{alpha}{
Significance level of the test (default: 0.05). Used to calculate asymptotic or Rademacher bound.
}
  \item{asymptotic}{
Should the asymptotic bound be calculated? (default: \code{FALSE}, Rademacher bound is used, \code{TRUE} calculation of asymptotic bounds is suitable for smaller datasets)
}
  \item{replace}{
Should sampling with replacement be used in computation of asymptotic bounds? (default: \code{TRUE})
}
  \item{n.times}{
Number of repetitions for sampling procedure (default: 150)
}
  \item{frac}{
Fraction of points to sample (default: 1)
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
  \item{\dots}{
Further arguments passed to \code{\link[kernlab]{kmmd}} specifying the kernel. E.g. \code{kernel} for passing the kernel as a character (default: \code{rbfdot} RBF kernel function) and \code{kpar} for passing the kernel parameter(s) as a named list (default: \code{"automatic"} uses heuristic for choosing a good bandwidth for the RBF or Laplace kernel). For details, see \code{\link[kernlab]{kmmd}}.
}
}
\details{
For a given kernel function \eqn{k} an unbiased estimator for MMD\eqn{^2} is defined as
\deqn{\widehat{\text{MMD}}^2(\mathcal{H}, X_1, X_2)_{U} =  \frac{1}{n_1(n_1-1)}\sum_{i=1}^{n_1}\sum_{\substack{j=1 \\ j\neq i}}^{n_1} k\left(X_{1i}, X_{1j}\right) \\ 
     + \frac{1}{n_2(n_2-1)}\sum_{i=1}^{n_2}\sum_{\substack{j=1 \\ j\neq i}}^{n_2} k\left(X_{2i}, X_{2j}\right)\\
	 - \frac{2}{n_1 n_2}\sum_{i=1}^{n_1}\sum_{\substack{j = 1 \\ j\neq i}}^{n_2} k\left(X_{1i}, X_{2j}\right).}
Its square root is returned as the statistic here. 

The theoretical MMD of two distributions is equal to zero if and only if the two distributions coincide. Therefore, low values indicate similarity of datasets and the test rejects for large values. 

The orignal proposal of the test is based on critical values calculated asymptotically or using Rademacher bounds. Here, the option for calculating a permutation p value is added. The Rademacher bound is always returned. Additionally, the asymptotic bound can be returned depending on the value of \code{asymptotic}. 

This implementation is a wrapper function around the function \code{\link[kernlab]{kmmd}} that modifies the in- and output of that function to match the other functions provided in this package. Moreover, a permutation test is added. For more details see the \code{\link[kernlab]{kmmd}}. 
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab When suitable kernel function is passed \tab No \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the test statistic}
\item{p.value}{Permutation p value}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{alternative }{The alternative hypothesis}
\item{H0}{Is \eqn{H_0} rejected according to the Rademacher bound?}
\item{asymp.H0}{Is \eqn{H_0} rejected according to the asymptotic bound?}
\item{kernel.fun}{Kernel function used}
\item{Rademacher.bound}{The Rademacher bound}
\item{asymp.bound}{The asymptotic bound}
}
\references{
Gretton, A., Borgwardt, K., Rasch, M., Schölkopf, B. and Smola, A. (2006). A Kernel Method for the Two-Sample-Problem. Neural Information Processing Systems 2006, Vancouver. \url{https://papers.neurips.cc/paper/3110-a-kernel-method-for-the-two-sample-problem.pdf}

Muandet, K., Fukumizu, K., Sriperumbudur, B. and Schölkopf, B. (2017). Kernel Mean Embedding of Distributions: A Review and Beyond. Foundations and Trends® in Machine Learning, 10(1-2), 1-141. \doi{10.1561/2200000060}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform MMD test 
if(requireNamespace("kernlab", quietly = TRUE)) {
  MMD(X1, X2, n.perm = 100)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ categorical }
\keyword{ kernel-based }
\keyword{ MMD }
\concept{ dataset similarity }
