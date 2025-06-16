\name{engineerMetric}
\alias{engineerMetric}
\encoding{UTF-8}
\title{
Engineer Metric
}
\description{
The function implements the \eqn{L_q}-engineer metric for comparing two multivariate distributions.
}
\usage{
engineerMetric(X1, X2, type = "F", seed = NULL)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{type}{
Character specifying the type of \eqn{L_q}-norm to use. Reasonable options are \code{"O"}, \code{"o"}, \code{"1"}, for the \eqn{L_1}-norm, \code{"I"}, and \code{"i"}, for the \eqn{L_\infty}-norm, and \code{"F"}, \code{"f"}, \code{"E"}, \code{"e"} (the default) for the \eqn{L_2}-norm (Euclidean norm). 
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided. Method is deterministic, seed is only set for consistency with other methods.
}
}
\details{
The engineer is a primary propability metric that is defined as 
\deqn{\text{EN}(X_1, X_2; q) = \left[ \sum_{i = 1}^{p} \left| \text{E}\left(X_{1i}\right) - \text{E}\left(X_{2i}\right)\right|^q\right]^{\min(q, 1/q)} \text{ with } q> 0,}
where \eqn{X_{1i}, X_{2i}} denote the \eqn{i}th component of the \eqn{p}-dimensional random vectors \eqn{X_1\sim F_1} and \eqn{X_2\sim F_2}.

In the implementation, expectations are estimated by column means of the respective datasets. 
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
\item{data.name }{The dataset names}
\item{method }{Description of the test}
\item{alternative }{The alternative hypothesis
}
}
\references{
Rachev, S. T. (1991). Probability metrics and the stability of stochastic models. John Wiley & Sons, Chichester.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149}
}

\note{
The seed argument is only included for consistency with other methods. The result of the metric calculation is deteministic.
}

\seealso{
\code{\link{Jeffreys}}
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Calculate engineer metric
engineerMetric(X1, X2)
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ probability metric }
\concept{ dataset similarity }
