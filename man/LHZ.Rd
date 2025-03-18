\name{LHZ}
\alias{LHZ}
\encoding{UTF-8}
\title{
Li et al. (2022) empirical characteristic distance
}
\description{
The function implements the \cite{Li et al. (2022)} empirical characteristic distance between two datasets.
}
\usage{
LHZ(X1, X2, n.perm = 0, seed = 42)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{n.perm}{
Number of permutations for permutation test (default: 0, no permutation test performed)
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
The test statistic
\deqn{T_{n, m} = \frac{1}{n^2} \sum_{j, q = 1}^n \left( \left\Vert \frac{1}{n} \sum_{k=1}^n e^{i\langle X_k, X_j-X_q \rangle} - \frac{1}{m} \sum_{l=1}^m e^{i\langle Y_l, X_j-X_q\rangle} \right\Vert^2 \right) + \frac{1}{m^2} \sum_{j, q = 1}^m \left( \left\Vert \frac{1}{n} \sum_{k=1}^n e^{i\langle X_k, Y_j-Y_q \rangle} - \frac{1}{m} \sum_{l=1}^m e^{i\langle Y_l, Y_j-Y_q\rangle} \right\Vert^2 \right)
}
is calculated according to \cite{Li et al. (2022)}. The datasets are denoted by \eqn{X} and \eqn{Y} with respective sample sizes \eqn{n} and \eqn{m}. By \eqn{X_j} the \eqn{i}-th row of dataset \eqn{X} is denoted. Furthermore, \eqn{\Vert \cdot \Vert} indicates the Euclidian norm and \eqn{\langle X_i, X_j \rangle} indicates the inner product between \eqn{X_i} and \eqn{X_j}. 

Low values of the test statistic indicate similarity. Therefore, the permutation test rejects for large values of the test statistic. 
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
\item{statistic }{Observed value of the test statistic}
\item{p.value}{Permutation p value (only if \code{n.perm} > 0)}
\item{data.name }{The dataset names}
\item{alternative }{The alternative hypothesis}
}
\references{
Li, X., Hu, W. and Zhang, B. (2022). Measuring and testing homogeneity of distributions by characteristic distance, Statistical Papers 64 (2), 529-556, \doi{10.1007/s00362-022-01327-7}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\seealso{
\code{\link{LHZStatistic}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Calculate LHZ statistic
LHZ(X1, X2)
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ comparison of characteristic functions }
\concept{ dataset similarity }
