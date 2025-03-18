\name{BG}
\alias{BG}
\encoding{UTF-8}
\title{
Biau and Gyorfi (2005) two-sample homogeneity test
}
\description{
The function implements the \cite{Biau and Gyorfi (2005)} two-sample homogeneity test. This test uses the \eqn{L_1}-distance between two empicial distribution functions restricted to a finite partition.
}
\usage{
BG(X1, X2, partition = rectPartition, exponent = 0.8, eps = 0.01, seed = 42, ...)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame  
}
  \item{X2}{
Second dataset as matrix or data.frame of the same sample size as \code{X1}
}
  \item{partition}{
Function that creates a finite partition for the subspace spanned by the two datasets (default: \code{rectPartition}, see Details)
}
  \item{exponent}{
Exponent used in the partition function, should be between 0 and 1 (default: 0.8)
}
  \item{eps}{
Small threshold to guarantee edge points are included (default: 0.01)
}
  \item{seed}{
Random seed (default: 42)
}
  \item{...}{
Further arguments to be passed to the partition function
}
}
\details{
The \cite{Biau and Gyorfi (2005)} two-sample homogeneity test is defined for two datasets of the same sample size.

By default a rectangular partition (\code{\link{rectPartition}}) is being calculated under the assumption of approximately equal cell probabilities. Use the \code{exponent} argument to choose the number of elements of the partition \eqn{m_n} accoring to the convergence criteria in \cite{Biau and Gyorfi (2005)}. By default choose \eqn{m_n = n^{0.8}}. For each of the \eqn{p} variables of the datasets, create \eqn{m_n^{1/p} + 1} cutpoints along the range of both datasets to define the partition, and ensure at least three cutpoints exist per variable (min, max, and one point splitting the data into two bins).

The test statistic is the \eqn{L_1}-distance between the vectors of the proportions of points falling into each cell of the partition for each dataset.
An asymptotic test is performed using a standardized version of the \eqn{L_1} distance that is approximately standard normally distributed (Corollary to Theorem 2 in \cite{Biau and Gyorfi (2005)}).
Low values of the test statistic indicate similarity. Therefore, the test rejects for large values of the test statistic.
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab No \tab No \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the (asymptotic) test statistic}
\item{p.value}{p value}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{alternative }{The alternative hypothesis}
}
\references{
Biau G. and Gyorfi, L. (2005). On the asymptotic properties of a nonparametric \eqn{L_1}-test statistic of homogeneity, IEEE Transactions on Information Theory, 51(11), 3965-3973. \doi{10.1109/TIT.2005.856979}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link{rectPartition}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform BG test 
BG(X1, X2)
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ comparison of CDFs }
\concept{ dataset similarity }
