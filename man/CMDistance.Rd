\name{CMDistance}
\alias{CMDistance}
\encoding{UTF-8}
\title{
Constrained Minimum Distance
}
\description{
Calculates the Constrained Minimum Distance (\cite{Tatti, 2007}) between two datasets.
}
\usage{
CMDistance(X1, X2, binary = NULL, cov = FALSE,
            S.fun = function(x) as.numeric(as.character(x)), 
            cov.S = NULL, Omega = NULL, seed = NULL)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{binary}{
Should the simplified form for binary data be used? (default: \code{NULL}, it is checked internally if each variable in the pooled dataset takes on exactly two distinct values)
}
  \item{cov}{
If the the binary version is used, should covariances in addition to means be used as features? (default: \code{FALSE}, corresponds to example 3 in Tatti (2007), \code{TRUE} corresponds to example 4). Ignored if \code{binary = FALSE}.
}
  \item{S.fun}{
Feature function (default: \code{NULL}). Should be supplied as a function that takes one observation vector as its input. Ignored if \code{binary = TRUE} (default: \code{NULL}).
}
  \item{cov.S}{
Covariance matix of feature function (default: \code{NULL}). Ignored if \code{binary = TRUE}.
}
  \item{Omega}{
Sample space as matrix (default: \code{NULL}, the sample space is derived from the data internally). Each row represents one value in the sample space. Used for calculating the covariance matrix if \code{cov.S = NULL}. Either \code{cov.S} or \code{Omega} must be given. Ignored if \code{binary = TRUE}.
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
}
\details{
The constrained minimum (CM) distance is not a distance between distributions but rather a distance based on summaries. These summaries, called frequencies and denoted by \eqn{\theta}, are averages of feature functions \eqn{S} taken over the dataset. The constrained minimum distance of two datasets \eqn{X_1} and \eqn{X_2} can be calculated as 
\deqn{d_{CM}(X_1, X_2 |S)^2 = (\theta_1 - \theta_2)^T\text{Cov}^{-1}(S)(\theta_1 - \theta_2), }
where \eqn{\theta_i = S(X_i)} is the frequency with respect to the \eqn{i}-th dataset, \eqn{i = 1, 2}, and 
\deqn{\text{Cov}(S) = \frac{1}{|\Omega|}\sum_{\omega\in\Omega} S(\omega)S(\omega)^T - \left(\frac{1}{|\Omega|}\sum_{\omega\in\Omega} S(\omega)\right)\left(\frac{1}{|\Omega|}\sum_{\omega\in\Omega} S(\omega)\right)^T,}
where \eqn{\Omega} denotes the sample space.

Note that the implementation can only handle limited dimensions of the sample space. 
The error message 

\code{"Error in rep.int(rep.int(seq_len(nx), rep.int(rep.fac, nx)), orep) : invalid 'times' value"} 

occurs when the sample space becomes too large to enumerate all its elements.
In case of binary data and \eqn{S} chosen as a conjunction or parity function \eqn{T_{F}} on a family of itemsets, the calculation of the CMD simplifies to 
\deqn{d_{CM}(D_1, D_2 | S_{F}) = 2 ||\theta_1 - \theta_2||_2,}
where \eqn{\theta_i = T_{F}(X_i), i = 1, 2,} as the sample space and covariance matrix are known. In case of more than two categories, either the sample space or the covariance matrix of the feature function must be supplied.

Small values of the CM Distance indicate similarity between the datasets. No test is conducted.
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab No \tab Yes \tab No \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the CM Distance}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{binary, cov, S.fun, cov.S, Omega }{Input parameters}
}
\references{
Tatti, N. (2007). Distances between Data Sets Based on Summary Statistics. JMRL 8, 131-154. 

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149}
}

\note{
Note that there is an error in the calculation of the covariance matrix in A.4 Proof of Lemma 8 in Tatti (2007). The correct covariance matrix has the form \deqn{\text{Cov}[T_{\mathcal{F}}] = 0.25I} since \deqn{\text{Var}[T_A] = \text{E}[T_A^2] - \text{E}[T_A]^2 = 0.5 - 0.5^2 = 0.25} following from the correct statement that \eqn{\text{E}[T_A^2] = \text{E}[T_A] = 0.5}. Therefore, formula (4) changes to \deqn{d_{CM}(D_1, D_2 | S_{\mathcal{F}}) = 2 ||\theta_1 - \theta_2||_2} and the formula in example 3 changes to \deqn{d_{CM}(D_1, D_2 | S_{I}) = 2 ||\theta_1 - \theta_2||_2.} Our implementation is based on these corrected formulas. If the original formula was used, the results on the same data calculated with the formula for the binary special case and the results calculated with the general formula differ by a factor of \eqn{\sqrt{2}}.
}

\examples{
# Test example 2 in Tatti (2007)
CMDistance(X1 = data.frame(c("C", "C", "C", "A")), 
           X2 = data.frame(c("C", "A", "B", "A")),
           binary = FALSE, S.fun = function(x) as.numeric(x == "C"),
           Omega = data.frame(c("A", "B", "C")))

# Demonstration of corrected calculation
set.seed(1234)
X1bin <- matrix(sample(0:1, 100 * 3, replace = TRUE), ncol = 3)
X2bin <- matrix(sample(0:1, 100 * 3, replace = TRUE, prob = 1:2), ncol = 3)
CMDistance(X1bin, X2bin, binary = TRUE, cov = FALSE)
Omega <- expand.grid(0:1, 0:1, 0:1)
S.fun <- function(x) x
CMDistance(X1bin, X2bin, binary = FALSE, S.fun = S.fun, Omega = Omega)
CMDistance(X1bin, X2bin, binary = FALSE, S.fun = S.fun, cov.S = 0.5 * diag(3))
CMDistance(X1bin, X2bin, binary = FALSE, S.fun = S.fun, 
            cov.S = 0.5 * diag(3))$statistic * sqrt(2)

# Example for non-binary data
set.seed(1234)
X1cat <- matrix(sample(1:4, 300, replace = TRUE), ncol = 3)
X2cat <- matrix(sample(1:4, 300, replace = TRUE, prob = 1:4), ncol = 3)
CMDistance(X1cat, X2cat, binary = FALSE, S.fun = S.fun, 
           Omega = expand.grid(1:4, 1:4, 1:4))
CMDistance(X1cat, X2cat, binary = FALSE, S.fun = function(x) as.numeric(x == 1), 
           Omega = expand.grid(1:4, 1:4, 1:4))
CMDistance(X1cat, X2cat, binary = FALSE, S.fun = function(x){ 
           c(x, x[1] * x[2], x[1] * x[3], x[2] * x[3])}, 
           Omega = expand.grid(1:4, 1:4, 1:4))
}
\keyword{ htest }
\keyword{ categorical }
\keyword{ summary-statistics-based }
\concept{ dataset similarity }
