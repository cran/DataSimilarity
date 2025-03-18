\name{BF}
\alias{BF}
\encoding{UTF-8}
\title{
Baringhaus and Franz (2010) rigid motion invariant multivariate two-sample test
}
\description{
The function implements the \cite{Baringhaus and Franz (2010)} multivariate two-sample test. The implementation here uses the \code{\link[cramer:cramer.test]{cramer.test}} implementation from the \pkg{cramer} package.
}
\usage{
BF(X1, X2, n.perm = 0, just.statistic = n.perm <= 0, kernel = "phiLog", 
    sim = "ordinary", maxM = 2^14, K = 160, seed = 42)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{n.perm}{
Number of permutations for permutation or Bootstrap test, respectively (default: 0, no permutation test performed)
}
  \item{just.statistic}{
Should only the test statistic be calculated without performing any test (default: \code{TRUE} if number of permutations is set to 0 and \code{FALSE} if number of permutations is set to any positive number)
}
  \item{kernel}{
Name of the kernel function as character. Possible options are \code{"phiLog"} (default), \code{"phiFracA"}, and \code{"phiFracB"}. Alternatively, a user-defined function can be supplied. The function should allow a matrix as input and fulfill the following properties. The output should be non-negative, the value of 0 should be mapped to 0, and the first derivative should be non-constant completely monotone.
}
  \item{sim}{
Type of Bootstrap or eigenvalue method for testing. Possible options are \code{"ordinary"} (default) for ordinary Boostrap, \code{"permutation"} for permutation testing, or \code{"eigenvalue"} for bootstrapping the limit distribution (especially good for datasets too large for performing Bootstrapping). For more details see \code{\link[cramer:cramer.test]{cramer.test}}
}
  \item{maxM}{
Maximum number of points used for fast Fourier transform involved in eigenvalue method for approximating the null distribution (default: 2^14). Ignored if \code{sim} is either \code{"ordinary"} or \code{"permutation"}. For more details see \code{\link[cramer:cramer.test]{cramer.test}}.
}
  \item{K}{
Upper value up to which the integral for calculating the distribution function from the characteristic function is evaluated (default: 160). Note: when \code{K} is increased, it is necessary to also increase \code{maxM}. Ignored if sim is either \code{"ordinary"} or \code{"permutation"}. For more details see \code{\link[cramer:cramer.test]{cramer.test}}.
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
The \cite{Bahrinhaus and Franz (2010)} test statistic
\deqn{T_{n_1, n_2} = \frac{n_1 n_2}{n_1+n_2}\left(\frac{2}{n_1 n_2}\sum_{i=1}^{n_1}\sum_{j=1}^{n_2} \phi(||X_{1i} - X_{2j}||^2) -  \frac{1}{n_1^2}\sum_{i,j=1}^{n_1} \phi(||X_{1i} - X_{1j}||^2) -  \frac{1}{n_2^2}\sum_{i,j=1}^{n_2} \phi(||X_{2i} - X_{2j}||^2)\right)}
is defined using a kernel function \eqn{\phi}. A choice recommended preferably for location alternatives is \deqn{\phi_{\text{log}}(x) = \log(1 + x),} two choices recommended preferably for dispersion alternatives are \deqn{\phi_{\text{FracA}}(x) = 1 - \frac{1}{1+x}} and \deqn{\phi_{\text{FracB}}(x) = 1 - \frac{1}{(1+x)^2}.} 

The theoretical statistic underlying this test statistic is zero if and only if  the distributions coincide. Therefore, low values of the test statistic incidate similarity of the datasets while high values indicate differences between the datasets.

This implementation is a wrapper function around the function \code{\link[cramer:cramer.test]{cramer.test}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see \code{\link[cramer:cramer.test]{cramer.test}}. 
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab No \tab No \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{method }{Description of the test}
\item{d }{Number of variables in each dataset}
\item{m }{Sample size of first dataset}
\item{n }{Sample size of second dataset}
\item{statistic }{Observed value of the test statistic}
\item{p.value}{Boostrap/ permutation p value (only if \code{n.perm} > 0)}
\item{sim }{Type of Boostrap or eigenvalue method (only if \code{n.perm} > 0)}
\item{n.perm }{Number of permutations for permutation or Boostrap test}
\item{hypdist }{Distribution function under the null hypothesis reconstructed via fast Fourier transform. \code{$x} contains the x-values, \code{$Fx} contains the corresponding distribution function values. (only if \code{n.perm} > 0)}
\item{ev }{Eigenvalues and eigenfunctions when using the eigenvalue method (only if \code{n.perm} > 0)}
\item{data.name }{The dataset names}
\item{alternative }{The alternative hypothesis}
}
\references{
Baringhaus, L. and Franz, C. (2010). Rigid motion invariant two-sample tests, Statistica Sinica 20, 1333-1361

Franz, C. (2024). cramer: Multivariate Nonparametric Cramer-Test for the Two-Sample-Problem. R package version 0.9-4, https://CRAN.R-project.org/package=cramer.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link{Bahr}}, \code{\link{Cramer}}, \code{\link{Energy}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform Baringhaus and Franz test 
if(requireNamespace("cramer", quietly = TRUE)) {
  BF(X1, X2, n.perm = 100)
  BF(X1, X2, n.perm = 100, kernel = "phiFracA")
  BF(X1, X2, n.perm = 100, kernel = "phiFracB")
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ inter-point distances }
\concept{ dataset similarity }