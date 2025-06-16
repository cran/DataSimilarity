\name{HMN}
\alias{HMN}
\encoding{UTF-8}
\title{
Random Forest Based Two-Sample Test
}
\description{
Performs the random forest based two-sample test proposed by \cite{Hediger et al. (2022)}. The implementation here uses the \code{\link[hypoRF]{hypoRF}} implementation from the \pkg{hypoRF} package.
}
\usage{
HMN(X1, X2, n.perm = 0, statistic = "PerClassOOB", normal.approx = FALSE, 
    seed = NULL, ...)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{n.perm}{
Number of permutations for permutation test (default: 0, binomial test is performed).
}
  \item{statistic}{
Character specifying the test statistic. Possible options are \code{"PerClassOOB"} (default) corresponding to the sum of out-of-bag (OOB) per class errors, and \code{"OverallOOB"} corresponding to the overall OOB error.
}
  \item{normal.approx}{
Should a normal approximation be used in the permutation test procedure? (default: \code{FALSE})
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
  \item{...}{
Arguments passed to \code{\link[ranger]{ranger}}
}
}
\details{
For the test, a random forest is fitted to the pooled dataset where the target variable is the original dataset membership. The test statistic is either the overall out-of-bag classification accuracy or the sum or mean of the per-class out-of-bag errors for the permutation test. For the asymptotic test (\code{n.perm = 0}), the pooled dataset is split into a training and test set and the test statistic is either the overall classification error on the test set or the mean of the per-class classification errors on the test set. In the former case, a binomial test is performed, in the latter case, a Wald test is performed. If the underlying distributions coincide, classification errors close to chance level are expected. The test rejects for small classification errors. 

Note that the per class OOB statistic differs for the permutation test and approximate test: for the permutation test, the sum of the per class OOB errors is returned, for the asymptotic version, the standardized sum is returned.

This implementation is a wrapper function around the function \code{\link[hypoRF]{hypoRF}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see \code{\link[hypoRF]{hypoRF}}. 
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab Yes \tab No \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the test statistic}
\item{parameter}{Paremeter(s) of the null distribution}
\item{p.value}{Asymptotic p value}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{val}{The OOB statistic values for the permuted data (for \code{n.perm > 0})}
\item{varest}{The estimated variance of the OOB statistic values for the permuted data (for \code{n.perm > 0})}
\item{importance_ranking}{Variable importance (for \code{importance = "impurity"})}
\item{cutoff}{The quantile of the importance distribution at level \eqn{\alpha}}
}
\references{
Hediger, S., Michel, L., Näf, J. (2022). On the use of random forest for two-sample testing. Computational Statistics & Data Analysis, 170, 107435, \doi{10.1016/j.csda.2022.107435}.

Simon, H., Michel, L., Näf, J. (2021). hypoRF: Random Forest Two-Sample Tests. R package version 1.0.0,\url{https://CRAN.R-project.org/package=hypoRF}.
  
Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\seealso{
\code{\link[ranger]{ranger}}, \code{\link{C2ST}}, \code{\link{YMRZL}}
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform random forest based test (low number of permutations due to runtime, 
# should be chosen considerably higher in practice) 
if(requireNamespace("hypoRF", quietly = TRUE)) {
  HMN(X1, X2, n.perm = 10)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ categorical }
\keyword{ binary classification }
\concept{ dataset similarity }