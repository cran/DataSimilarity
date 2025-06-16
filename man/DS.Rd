\name{DS}
\alias{DS}
\encoding{UTF-8}
\title{
Rank-Based Energy Test (Deb and Sen, 2021)
}
\description{
Performs the multivariate rank-based two-sample test using measure transportation by \cite{Deb and Sen (2021)}. 
}
\usage{
DS(X1, X2, n.perm = 0, rand.gen = NULL, seed = NULL)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{n.perm}{
Number of permutations for permuation test (default: 0, no permutation test performed)
}
  \item{rand.gen}{
Function that generates a grid of (random) numbers in \eqn{(0,1)} of dimension \eqn{n \times k} (\eqn{n} and \eqn{k} are inputs of this function). Default is \code{NULL} in which case, \code{\link[randtoolbox:halton]{randtoolbox::halton}} is used.
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
}
\details{
The test proposed by \cite{Deb and Sen (2021)} is a rank-based version of the Energy statistic (\cite{Székely and Rizzo, 2004}) that does not rely on any moment assumptions. Its test statistic is the Energy statistic applied to the rank map of both samples. The multivariate ranks are computed using optimal transport with a multivariate uniform distribution as the reference distribution. 

For the rank version of the Energy statistic it still holds that the value zero is attained if and only if the two distributions coincide. Therefore, low values of the empirical test statistic indicate similarity between the datasets and the null hypothesis of equal distributions is rejected for large values. 
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
\item{p.value}{Permutation p value}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
}
\references{
Original implementation: \url{https://github.com/NabarunD/MultiDistFree}

Deb, N. and Sen, B. (2021). Multivariate Rank-Based Distribution-Free Nonparametric Testing Using Measure Transportation, Journal of the American Statistical Association. \doi{10.1080/01621459.2021.1923508}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}
\author{
Original implementation by Nabarun Deb, Bodhisattva Sen

Minor modifications by Marieke Stolte
}
\note{
The implementation is a modification of the code supplied by \cite{Deb and Sen (2021)} for the simulation study presented in the original article. It generalizes the implementation and includes small modifications for computation speed. 
}

\seealso{
\code{\link{Energy}}
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform Deb and Sen test 
if(requireNamespace("randtoolbox", quietly = TRUE) & 
    requireNamespace("clue", quietly = TRUE)) {
  DS(X1, X2, n.perm = 100)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ inter-point distances }
\keyword{ rank-based }
\concept{ dataset similarity }
