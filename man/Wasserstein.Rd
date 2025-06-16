\name{Wasserstein}
\alias{Wasserstein}
\encoding{UTF-8}
\title{
Wasserstein Distance Based Test
}
\description{
Performs a permutation two-sample test based on the Wasserstein distance. The implementation here uses the \code{\link[Ecume]{wasserstein_permut}} implementation from the \pkg{Ecume} package.
}
\usage{
Wasserstein(X1, X2, n.perm = 0, fast = (nrow(X1) + nrow(X2)) > 1000, 
            S = max(1000, (nrow(X1) + nrow(X2))/2), seed = NULL, ...)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{n.perm}{
Number of permutations for permutation test (default: 0, no test is performed).
}
  \item{fast}{
Should the \code{\link[transport]{subwasserstein}} approximate function be used? (default: \code{TRUE} if the pooled sample size is more than 1000)
}
  \item{S}{
Number of samples to use for approximation if \code{fast = TRUE}. See \code{\link[transport]{subwasserstein}}
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
  \item{\dots}{
Other parameters passed to \code{\link[transport]{wasserstein}} or \code{\link[transport]{wasserstein1d}}, e.g. the power \eqn{p\ge 1}
}
}
\details{
A permutation test for the \eqn{p}-Wasserstein distance is performed. By default, the 1-Wasserstein distance is calculated using Euclidean distances. The \eqn{p}-Wasserstein distance between two probability measures \eqn{\mu} and \eqn{\nu} on a Euclidean space \eqn{M} is defined as 
\deqn{W_p(\mu, \nu) = \left(\inf_{\gamma\in\Gamma(\mu,\nu)}\int_{M\times M} ||x - y||^p \text{d} \gamma(x, y)\right)^{\frac{1}{p}},}
where \eqn{\Gamma(\mu,\nu)} is the set of probability measures on \eqn{M\times M} such that \eqn{\mu} and \eqn{\nu} are the marginal distributions.

As the Wasserstein distance of two distributions is a metric, it is zero if and only if the distributions coincides. Therefore, low values of the statistic indicate similarity of the datasets and the test rejects for high values. 

This implementation is a wrapper function around the function \code{\link[Ecume]{wasserstein_permut}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see the \code{\link[Ecume]{wasserstein_permut}}. 
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
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
}
\references{
Rachev, S. T. (1991). Probability metrics and the stability of stochastic models. John Wiley & Sons, Chichester.

Roux de Bezieux, H. (2021). Ecume: Equality of 2 (or \eqn{k}) Continuous Univariate and Multivariate Distributions. R package version 0.9.1, \url{https://CRAN.R-project.org/package=Ecume}

Schuhmacher, D., Bähre, B., Gottschlich, C., Hartmann, V., Heinemann, F., Schmitzer, B. and Schrieber, J. (2019). transport: Computation of Optimal Transport Plans and Wasserstein Distances. R package version 0.15-0. \url{https://cran.r-project.org/package=transport}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform Wasserstein distance based test 
if(requireNamespace("Ecume", quietly = TRUE)) {
  Wasserstein(X1, X2, n.perm = 100)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ probability metric }
\concept{ dataset similarity }