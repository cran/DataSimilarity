\name{Energy}
\alias{Energy}
\encoding{UTF-8}
\title{
Energy Statistic and Test
}
\description{
Performs the Energy statistic multi-sample test (\cite{Székely and Rizzo, 2004}). The implementation here uses the \code{\link[energy]{eqdist.etest}} implementation from the \pkg{energy} package.
}
\usage{
Energy(X1, X2, ..., n.perm = 0, seed = 42)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{\dots}{
Further datasets as matrices or data.frames
}
  \item{n.perm}{
Number of permutations for Bootstrap test (default: 0, no Bootstrap test performed)
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
The Energy statistic (\cite{Székely and Rizzo, 2004}) for two datasets \eqn{X_1} and \eqn{X_2} is defined as 
\deqn{T_{n_1, n_2} = \frac{n_1 n_2}{n_1+n_2}\left(\frac{1}{n_1 n_2}\sum_{i=1}^{n_1}\sum_{j=1}^{n_2} ||X_{1i} - X_{2j}|| -  \frac{1}{2n_1^2}\sum_{i,j=1}^{n_1} ||X_{1i} - X_{1j}|| -  \frac{1}{2n_2^2}\sum_{i,j=1}^{n_2} ||X_{2i} - X_{2j}||\right).}
This is equal to the Cramér test statistitic (\cite{Baringhaus and Franz, 2004}).
The multi-sample version is defined as the sum of the Energy statistics for all pairs of samples. 

The population Energy statistic for two distributions is equal to zero if and only if the two distributions coincide. Therefore, small values of the empirical statistic indicate similarity between datasets and the permutation test rejects the null hypothesis of equal distributions for large values. 

This implementation is a wrapper function around the function \code{\link[energy]{eqdist.etest}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see the \code{\link[energy]{eqdist.etest}}. 
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab No \tab Yes \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{call}{The function call}
\item{statistic }{Observed value of the test statistic}
\item{p.value}{Bootstrap p value}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
}
\references{
Szekely, G. J. and Rizzo, M. L. (2004) Testing for Equal Distributions in High Dimension, InterStat, November (5).

Szekely, G. J. (2000) Technical Report 03-05: E-statistics: Energy of Statistical Samples, Department of Mathematics and Statistics, Bowling Green State University.

Rizzo, M., Szekely, G. (2022). energy: E-Statistics: Multivariate Inference via the Energy of Data. R package version 1.7-11, \url{https://CRAN.R-project.org/package=energy}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\note{
The test based on the Energy statistic (\cite{Székely and Rizzo, 2004}) is equivalent to the Cramér test (\cite{Baringhaus and Franz, 2004}). 
}

\seealso{
\code{\link{Cramer}}, \code{\link{DISCOB}}, \code{\link{DISCOF}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform Energy test
if(requireNamespace("energy", quietly = TRUE)) {
  Energy(X1, X2, n.perm = 100)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ k-sample }
\keyword{ inter-point distances }
\concept{ dataset similarity }
