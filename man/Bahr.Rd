\encoding{UTF-8}
\name{Bahr}
\alias{Bahr}
\title{
Bahr (1996) Multivariate Two-sample Test
}
\description{
The function implements the \cite{Bahr (1996)} multivariate two-sample test. This test is a special case of the rigid-motion invariant multivariate two-sample test of \cite{Baringhaus and Franz (2010)}. The implementation here uses the \code{\link[cramer]{cramer.test}} implementation from the \pkg{cramer} package.
}
\usage{
Bahr(X1, X2, n.perm = 0, just.statistic = n.perm <= 0, 
      sim = "ordinary", maxM = 2^14, K = 160, seed = NULL)
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
  \item{sim}{
Type of Bootstrap or eigenvalue method for testing. Possible options are \code{"ordinary"} (default) for ordinary Boostrap, \code{"permutation"} for permutation testing, or \code{"eigenvalue"} for bootstrapping the limit distribution (especially good for datasets too large for performing Bootstrapping). For more details see \code{\link[cramer:cramer.test]{cramer.test}}
}
  \item{maxM}{
Maximum number of points used for fast Fourier transform involved in eigenvalue method for approximating the null distribution (default: 2^14). Ignored if sim is either \code{"ordinary"} or \code{"permutation"}. For more details see \code{\link[cramer]{cramer.test}}.
}
  \item{K}{
Upper value up to which the integral for calculating the distribution function from the characteristic function is evaluated (default: 160). Note: when \code{K} is increased, it is necessary to also increase \code{maxM}. Ignored if sim is either \code{"ordinary"} or \code{"permutation"}. For more details see \code{\link[cramer:cramer.test]{cramer.test}}.
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
}
\details{
The \cite{Bahr (1996)} test is a specialcase of the test of \cite{Bahrinhaus and Franz (2010)} 
\deqn{T_{n_1, n_2} = \frac{n_1 n_2}{n_1+n_2}\left(\frac{2}{n_1 n_2}\sum_{i=1}^{n_1}\sum_{j=1}^{n_2} \phi(||X_{1i} - X_{2j}||^2) -  \frac{1}{n_1^2}\sum_{i,j=1}^{n_1} \phi(||X_{1i} - X_{1j}||^2) -  \frac{1}{n_2^2}\sum_{i,j=1}^{n_2} \phi(||X_{2i} - X_{2j}||^2)\right)}
where the kernel function \eqn{\phi} is set to \deqn{\phi_{\text{Bahr}}(x) = 1 - \exp(-x/2).} 
The theoretical statistic underlying this test statistic is zero if and only if 
the distributions coincide. Therefore, low values of the test statistic incidate similarity of the datasets while high values indicate differences between the datasets.

This implementation is a wrapper function around the function \code{\link[cramer:cramer.test]{cramer.test}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see the \code{\link[cramer:cramer.test]{cramer.test}}. 
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
\item{alternative }{The alternative hypothesis
}
}
\references{
Baringhaus, L. and Franz, C. (2010). Rigid motion invariant two-sample tests, Statistica Sinica 20, 1333-1361

Bahr, R. (1996). Ein neuer Test fuer das mehrdimensionale Zwei-Stichproben-Problem bei allgemeiner Alternative, German, Ph.D. thesis, University of Hanover

Franz, C. (2024). cramer: Multivariate Nonparametric Cramer-Test for the Two-Sample-Problem. R package version 0.9-4, \url{https://CRAN.R-project.org/package=cramer}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link{BF}}, \code{\link{Cramer}}, \code{\link{Energy}}
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform Bahr test 
if(requireNamespace("cramer", quietly = TRUE)) {
  Bahr(X1, X2, n.perm = 100)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ inter-point distances }
\concept{ dataset similarity }

