\name{OTDD}
\alias{OTDD}
\alias{hammingDist}
\encoding{UTF-8}
\title{
Optimal Transport Dataset Distance
}
\description{
The function implements the optimal transport dataset distance (\cite{Alvarez-Melis and Fusi, 2020}). The distance combines the distance between features and the distance between label distributions. 
}
\usage{
OTDD(X1, X2, target1 = "y", target2 = "y", method = "precomputed.labeldist", 
      feature.cost = stats::dist, lambda.x = 1, lambda.y = 1, p = 2, ground.p = 2,
      sinkhorn = FALSE, debias = FALSE, inner.ot.method = "exact", inner.ot.p = 2, 
      inner.ot.ground.p = 2, inner.ot.sinkhorn = FALSE, inner.ot.debias = FALSE,
      seed = NULL)
hammingDist(x) 
}

\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{target1}{
Character specifying the column name of the class variable in the first dataset (default: \code{"y"})
}
  \item{target2}{
Character specifying the column name of the class variable in the second dataset (default: \code{"y"})
}
  \item{method}{
Character specifying the method for computing the OTDD. Possible options are \code{"augmentation"}, i.e. computing the optimal transport on the augmented dataset, and \code{"precomputed.labeldist"}, i.e. the usual computation of label distances (default).
}
  \item{feature.cost}{
Function that calculates the distance matrix on the pooled feature dataset (default: \code{\link[stats:dist]{stats::dist}}). Ignored if \code{method = precomputed.labeldist}, then \eqn{L_p}-distance is used as feature cost. 
}
  \item{lambda.x, lambda.y}{
Weights of the feature distances and label distances in the overall cost (default: 1, equally weighted). Note that values unequal to one are only supported for \code{method = precomputed.labeldist}.
}
  \item{p}{
Power \eqn{p} of the \eqn{p}-Wasserstein distance for the outer optimal transport problem (default: 2).
}
  \item{ground.p}{
Power \eqn{p} of the \eqn{L_p}-norm used in calculation of Wasserstein distance for the outer optimal transport problem (default: 2). Ignored if \code{method = "precomputed.labeldist"}.
}
  \item{sinkhorn}{
Should the Sinkhorn approximation be used for solving the outer optimal transport problem? (default: \code{FALSE}, exact solution is used)
}
  \item{debias}{
Should debiased estimator be used when using Sinkhorn approximation for outer optimal transport problem? (default: \code{FALSE})
}
  \item{inner.ot.method}{
Method for computing the label distances. Possible options are \code{"exact"} (the default), i.e. calculating the solution to the optimal transport of the label distributions, \code{"gaussian.approx"}, i.e. calculating the Wasserstein distance of the labels using a Gaussian approximation of the label distributions, \code{"naive.upperbound"}, i.e. calculating the upperbound \eqn{d_{UB}}, \code{"only.means"}, i.e. approximating the label distance by computing the Euclidean distance of the mean vectors of the label distributions. Ignored if \code{method = "augmentation"}.
}
  \item{inner.ot.p}{
Power \eqn{p} of the \eqn{p}-Wasserstein distance for the inner optimal transport problem (default: 2). Used only if \code{method = "precomputed.labeldist"} and \code{inner.ot.method = "exact"}.
}
  \item{inner.ot.ground.p}{
Power \eqn{p} of the \eqn{L_p}-norm used in calculation of Wasserstein distance for the outer optimal transport problem (default: 2). Used only if \code{method = "precomputed.labeldist"} and \code{inner.ot.method = "exact"}.
}
  \item{inner.ot.sinkhorn}{
Should the Sinkhorn approximation be used for solving the inner optimal transport problem? (default: \code{FALSE}, exact solution is used). Used only if \code{method = "precomputed.labeldist"} and \code{inner.ot.method = "exact"}.
}
  \item{inner.ot.debias}{
Should debiased estimator be used when using Sinkhorn approximation for inner optimal transport problem? (default: \code{FALSE}). Used only if \code{method = "precomputed.labeldist"} and \code{inner.ot.method = "exact"}.
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
  \item{x}{
Dataset for which the distance matrix of pairwise Hamming distances is calculated.
}
}
\details{
\cite{Alvarez-Melis and Fusi (2020)} define a dataset distance that takes into account both the feature variables as well as a target (label) variable. The idea is to compute the optimal transport based on a cost function that is a combination of the feature distance and the Wasserstein distance between the label distributions. The label distribution refers to the distribution of features for a given label. With this, the distance between feature-label pairs \eqn{z:= (x, y)} can be defined as 
\deqn{
d_{\mathcal{Z}}(z, z^\prime) := (d_{\mathcal{X}}(x, x^{\prime})^p + W_p^p(\alpha_y, \alpha_{y^{\prime}}))^{1/p},
}
where \eqn{\alpha_y} denotes the distribution \eqn{\text{P}(X|Y=y)} for label \eqn{y} over the feature space. With this, the optimal transport dataset distance is defined as 
\deqn{
d_{OT}(\mathcal{D}_1, \mathcal{D}_2) = \min_{\pi\in\Pi(\alpha, \beta)} \int_{\mathcal{Z}\times\mathcal{Z}} d_{\mathcal{Z}}(z, z^\prime)^p \text{d }\pi(z, z^\prime), 
}
where 
\deqn{
\Pi(\alpha, \beta) := \{\pi_{1,2}\in\mathcal{P}(\mathcal{X}\times\mathcal{X}) | \pi_1 = \alpha, \pi_2 = \beta\}
}
is the set of joint distributions with \eqn{\alpha} and \eqn{\beta} as marginals. 

Here, we use the Wasserstein distance implementation from the \pkg{approxOT} package for solving the optimal transport problems. 

There are multiple simplifications implemented. First, under the assumption that the metric on the feature space coincides with the ground metric in the optimal transport problem on the labels and that all covariance matrices of the label distributions commute (rarely fulfilled in practice), the computation reduces to solving the optimal transport problem on the datasets augmented with the means and covariance matrices of the label distributions. This simplification is used when setting \code{method = "augmentation"}. Next, the Sinkhorn approximation can be utilized both for calculating the solution of the overall (outer) optimal transport problem (\code{sinkhorn = TRUE}) and for the inner optimal transport problem for computing the label distances (\code{inner.ot.sinkhorn = TRUE}). The solution of the inner problem can also be sped up by using a normal approximation of the label distributions (\code{inner.ot.method = "gaussian.approx"}) which results in a closed form expression of the solution. \code{inner.ot.method = "only.means"} further simplifies the calculation by using only the means of these Gaussians, which corresponds to assuming equal covariances in all Gaussian approximations of the label distributions. Using \code{inner.ot.method = "upper.bound"} uses a distribution-agnostic upper bound to bypass the solution of the inner optimal transport problem. 

For categorical data, specify an appropriate \code{feature.cost} and use \code{method = "precomputed.labeldist"} and \code{inner.ot.method = "exact"}. A pre-implemented option is setting \code{feature.cost = hammingDist} for using the Hamming distance for categorical data. When implementing an appropriate function that takes the pooled dataset without the target column as input and gives a distance matrix as the output, a mix of categorical and numerical data is also possible.
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     Yes \tab Yes \tab Yes \tab No \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the test statistic}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{alternative }{The alternative hypothesis}
}
\references{
Interactive visualizations: \url{https://www.microsoft.com/en-us/research/blog/measuring-dataset-similarity-using-optimal-transport/}

Alvarez-Melis, D. and Fusi, N. (2020). Geometric Dataset Distances via Optimal Transport. In Advances in Neural Information Processing Systems 33 21428-21439. 

Original python implementation: Alvarez-Melis, D., and Yang, C. (2024). Optimal Transport Dataset Distance (OTDD). \url{https://github.com/microsoft/otdd}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}
\author{
Original python implementation: David Alvarez-Melis, Chengrun Yang

R implementation: Marieke Stolte
}
\note{
Especially for large numbers of variables and low numbers of observations, it can happen that the Gaussian approximation of the inner OT problem fails since the estimated covariance matrix for one label distribution is numerically no longer psd. An error is thrown in that case. 
}

\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
y1 <- rbinom(100, 1, 1 / (1 + exp(1 - X1 \%*\% rep(0.5, 10))))
y2 <- rbinom(100, 1, 1 / (1 + exp(1 - X2 \%*\% rep(0.7, 10))))
X1 <- data.frame(X = X1, y = y1)
X2 <- data.frame(X = X2, y = y2)
# Calculate OTDD
\donttest{
if(requireNamespace("approxOT", quietly = TRUE) & 
    requireNamespace("expm", quietly = TRUE)) {
  OTDD(X1, X2)
  OTDD(X1, X2, sinkhorn = TRUE, inner.ot.sinkhorn = TRUE)
  OTDD(X1, X2, method = "augmentation") 
  OTDD(X1, X2, inner.ot.method = "gaussian.approx")
  OTDD(X1, X2, inner.ot.method = "means.only")
  OTDD(X1, X2, inner.ot.method = "naive.upperbound")
}
}

# For categorical data
X1cat <- matrix(sample(LETTERS[1:4], 300, replace = TRUE), ncol = 3)
X2cat <- matrix(sample(LETTERS[1:4], 300, replace = TRUE, prob = 1:4), ncol = 3)
y1 <- sample(0:1, 300, TRUE)
y2 <- sample(0:1, 300, TRUE)
X1 <- data.frame(X = X1cat, y = y1)
X2 <- data.frame(X = X2cat, y = y2)
\donttest{
if(requireNamespace("approxOT", quietly = TRUE) &
    requireNamespace("expm", quietly = TRUE)) {
  OTDD(X1, X2, feature.cost = hammingDist)
  OTDD(X1, X2, sinkhorn = TRUE, inner.ot.sinkhorn = TRUE, feature.cost = hammingDist)
}
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ categorical }
\keyword{ target variable }
\keyword{ distance / similarity measure }
\concept{ dataset similarity }
