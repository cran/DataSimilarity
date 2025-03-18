\name{BallDivergence}
\alias{BallDivergence}
\encoding{UTF-8}
\title{
Ball Divergence based two- or \eqn{k}-sample test
}
\description{
The function implements the \cite{Pan et al. (2018)} multivariate two- or \eqn{k}-sample test based on the Ball Divergence. The implementation here uses the \code{\link[Ball]{bd.test}} implementation from the \pkg{Ball} package.
}
\usage{
BallDivergence(X1, X2, ..., n.perm = 0, seed = 42, num.threads = 0, 
                kbd.type = "sum", weight = c("constant", "variance"), 
                args.bd.test = NULL)
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
  \item{n.perm}{
Number of permutations for permutation test (default: 0, no permutation test performed). Note that for more than two samples, no test is performed.
}
  \item{seed}{
Random seed (default: 42)
}
  \item{num.threads}{
Number of threads (default: 0, all available cores are used)
}
  \item{kbd.type}{
Character specifying which k-sample test statistic will be used. Must be one of \code{"sum"} (default), \code{"maxsum"}, or \code{"max"}.
}
  \item{weight}{
Character specifying the weight form of the Ball Divergence test statistic. Must be one of \code{"constant"} (default) or \code{"variance"}.
}
  \item{args.bd.test}{
Further arguments passed to \code{\link[Ball:bd.test]{bd.test}} as a named list.
}
}
\details{
For \code{n.perm = 0}, the asymptotic test is performed. For \code{n.perm > 0}, a permutation test is performed.

The Ball Divergence is defined as the square of the measure difference over a given closed ball collection. The empirical test performed here is based on the difference between averages of metric ranks. It is robust to outliers and heavy-tailed data and suitable for imbalanced sample sizes.

The Ball Divergence of two distributions is zero if and only if the distributions coincide. Therefore, low values of the test statistic indicate similarity and the test rejects for large values of the test statistic.

For the \eqn{k}-sample problem the pairwise Ball divergences can be summarized in different ways. First, one can simply sum up all pairwise Ball divergences (\code{kbd.type = "sum"}). Next, one can find the sample with the largest difference to the other, i.e. take the maximum of the sums of all Ball divergences for each sample with all other samples (\code{kbd.type = "maxsum"}). Last, one can sum up the largest \eqn{k-1} pairwise Ball divergences (\code{kbd.type = "max"}).

This implementation is a wrapper function around the function \code{\link[Ball:bd.test]{bd.test}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see \code{\link[Ball:bd.test]{bd.test}} and \code{\link[Ball:bd.test]{bd}}. 
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab No \tab Yes \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the test statistic}
\item{p.value }{Permutation p value (only if \code{n.perm} > 0 and for two datasets)}
\item{n.perm }{Number of permutations for permutation test}
\item{size }{Number of observations for each dataset}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{alternative }{The alternative hypothesis}
}
\references{
Pan, W., T. Y. Tian, X. Wang, H. Zhang (2018). Ball Divergence: Nonparametric two sample test, Annals of Statistics 46(3), 1109-1137, \doi{10.1214/17-AOS1579}.

J. Zhu, W. Pan, W. Zheng, and X. Wang (2021). Ball: An R Package for Detecting Distribution Difference and Association in Metric Spaces, Journal of Statistical Software, 97(6), \doi{10.18637/jss.v097.i06}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Calculate Ball Divergence and perform test 
if(requireNamespace("Ball", quietly = TRUE)) {
  BallDivergence(X1, X2, n.perm = 100)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ testing approach }
\concept{ dataset similarity }
