\name{BG2}
\alias{BG2}
\encoding{UTF-8}
\title{
Biswas and Ghosh (2014) Two-Sample Test
}
\description{
Performs the \cite{Biswas and Ghosh (2014)} two-sample test for high-dimensional data. 
}
\usage{
BG2(X1, X2, n.perm = 0, seed = NULL)
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
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
}
\details{
The test is based on comparing the means of the distributions of the within-sample and between-sample distances of both samples. It is intended for the high dimension low sample size (HDLSS) setting and claimed to perform better in this setting than the tests of \cite{Friedman and Rafsky (1979)}, \cite{Schilling (1986)} and \cite{Henze (1988)} and the Cramér test of \cite{Baringhaus and Franz (2004)}. 

The statistic is defined as 
\deqn{T = ||\hat{\mu}_{D_F} - \hat{\mu}_{D_G}||^2_2, \text{ where}}
\deqn{\hat{\mu}_{D_F} = \left[\hat{\mu}_{FF} = \frac{2}{n_1(n_1 - 1)}\sum_{i=1}^{n_1}\sum_{j=i+1}^{n_1}||X_{1i} - X_{1j}||, \hat{\mu}_{FG} = \frac{1}{n_1 n_2}\sum_{i=1}^{n_1}\sum_{j=1}^{n_2}||X_{1i} - X_{2j}||\right], }
\deqn{\hat{\mu}_{D_G} = \left[\hat{\mu}_{FG} = \frac{1}{n_1 n_2}\sum_{i=1}^{n_1}\sum_{j=1}^{n_2}||X_{1i} - X_{2j}||, \hat{\mu}_{GG} = \frac{2}{n_2(n_2 - 1)}\sum_{i=1}^{n_2}\sum_{j=i+1}^{n_2}||X_{2i} - X_{2j}||\right]. }

For testing, the scaled statistic 
\deqn{T^* = \frac{N\hat{\lambda}(1 - \hat{\lambda})}{2\hat{\sigma}_0^2} T \text{ with}}
\deqn{\hat{\lambda} = \frac{n_1}{N},}
\deqn{\hat{\sigma}_0^2 = \frac{n_1S_1 + n_2S_2}{N}, \text{ where}}
\deqn{S_1 = \frac{1}{\binom{n_1}{3}} \sum_{1\le i < j < k \le n_1} ||X_{1i} - X_{1j}||\cdot ||X_{1i} - X_{1k}|| - \hat{\mu}_{FF}^2 \text{ and}}
\deqn{S_2 = \frac{1}{\binom{n_2}{3}} \sum_{1\le i < j < k \le n_2} ||X_{2i} - X_{2j}||\cdot ||X_{2i} - X_{2k}|| - \hat{\mu}_{GG}^2}
is used as it is asymptotically \eqn{\chi^2_1}-distributed.

In both cases, low values indicate similarity of the datasets. Thus, the test rejects the null hypothesis of equal distributions for large values of the test statistic. 

For \code{n.perm > 0}, a permutation test is conducted. Otherwise, an asymptotic test using the asymptotic distibution of \eqn{T^*} is performed.
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
\item{p.value}{Asymptotic or permutation p value}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
}
\references{
Biswas, M., Ghosh, A.K. (2014). A nonparametric two-sample test applicable to high dimensional data.
Journal of Multivariate Analysis, 123, 160-171, \doi{10.1016/j.jmva.2013.09.004}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link{Energy}}, \code{\link{Cramer}} 
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform Biswas and Ghosh test
BG2(X1, X2)
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ inter-point distances }
\concept{ dataset similarity }
