\name{BMG}
\alias{BMG}
\encoding{UTF-8}
\title{
Biswas et al. (2014) two-sample run test
}
\description{
The function implements the \cite{Biswas, Mukhopadhyay and Gosh (2014)} distribution-free two-sample run test. This test uses a heuristic approach to calculate the shortest Hamilton path between the two datasets using the \code{HamiltonPath} function. By default the asymptotic version of the test is calculated.
}
\usage{
BMG(X1, X2, seed = 42, asymptotic = TRUE)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame  
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{seed}{
Random seed (default: 42)
}
  \item{asymptotic}{
Should the asymptotic version of the test be performed (default: \code{TRUE})
}
}
\details{
The test counts the number of edges in the shortest Hamilton path calculated on the pooled sample that connect points from different samples, i.e.
\deqn{T_{m,n} = 1 + \sum_{i = 1}^{N-1} U_i, }
where \eqn{U_i} is an indicator function with \eqn{U_i = 1} if the \eqn{i}th edge connects points from different samples and \eqn{U_i = 0} otherwise.

For a combined sample size \code{N} smaller or equal to 1030, the exact version of the \cite{Biswas, Mukhopadhyay and Gosh (2014)} test can be calculated. It uses the univariate run statistic (\cite{Wald and Wolfowitz, 1940}) to calculate the test statistic. For \code{N} larger than 1030, the calculation for the exact version breaks.

If an asymptotic test is performed the asymptotic null distribution is given by
\deqn{T_{m, n}^{*} \sim \mathcal{N}(0, 4\lambda^2(1-\lambda)^2)}
where \eqn{T_{m, n}^{*}= \sqrt{N} (T_{m, n} / N - 2  \lambda (1 - \lambda))} the asymptotic test statistic, \eqn{\lambda = m/N} and \eqn{m} is the sample size of the first dataset. Therefore, low absolute values of the asymptotic test statistic indicate similarity of the two datasets whereas high absolute values indicate differences between the datasets.
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab No \tab No \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the test statistic (note: this is not the asymptotic test statistic)}
\item{p.value}{(asymptotic) p value}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{alternative }{The alternative hypothesis}
}
\references{
Biswas, M., Mukhopadhyay, M. and Ghosh, A. K. (2014). A distribution-free two-sample run test applicable to high-dimensional data, Biometrika 101 (4), 913-926, \doi{10.1093/biomet/asu045}

Wald, A. and Wolfowitz, J. (1940). On a test whether two samples are from the same distribution, Annals of Mathematical Statistic 11, 147-162

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link{HamiltonPath}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform BMG test 
BMG(X1, X2)
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ graph-based }
\concept{ dataset similarity }
