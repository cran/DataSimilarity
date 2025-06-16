\name{SC}
\alias{SC}
\encoding{UTF-8}
\title{
Graph-Based Multi-Sample Test
}
\description{
Performs the graph-based multi-sample test for high-dimensional data proposed by \cite{Song and Chen (2022)}. The implementation here uses the \code{\link[gTestsMulti]{gtestsmulti}} implementation from the \pkg{gTestsMulti} package.
}
\usage{
SC(X1, X2, ..., n.perm = 0, dist.fun = stats::dist, graph.fun = MST, 
    dist.args = NULL, graph.args = NULL, type = "S", seed = NULL)
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
Number of permutations for permutation test (default: 0, no permutation test performed)
}
  \item{dist.fun}{
Function for calculating a distance matrix on the pooled dataset (default: \code{\link[stats:dist]{stats::dist}}, Euclidean distance).
}
  \item{graph.fun}{
Function for calculating a similarity graph using the distance matrix on the pooled sample (default: \code{\link{MST}}, Minimum Spanning Tree).
}
  \item{dist.args}{
Named list of further arguments passed to \code{dist.fun} (default: \code{NULL}).
}
  \item{graph.args}{
Named list of further arguments passed to \code{graph.fun} (default: \code{NULL}).
}
  \item{type}{
Character specifying the test statistic to use. Possible options are \code{"S"} (default) and \code{"SA"}. See details.
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
}
\details{
Two multi-sample test statistics are defined by \cite{Song and Chen (2022)} based on a similarity graph. The first one is defined as 
\deqn{S = S_W + S_B, \text{ where}}
\deqn{S_W = (R_W - \text{E}(R_W))^T \Sigma_W^{-1}(R_W - \text{E}(R_W)),}
\deqn{S_B = (R_B - \text{E}(R_B))^T \Sigma_W^{-1}(R_B - \text{E}(R_B)),}
with \eqn{R_W} denoting the vector of within-sample edge counts and \eqn{R_B} the vector of between-sample edge counts. Expectations and covariance matrix are calculated under the null. 

The second statistic is defined as 
\deqn{S_A = (R_A - \text{E}(R_A))^T \Sigma_W^{-1}(R_A - \text{E}(R_A)), }
where \eqn{R_A} is the vector of all linearly independent edge counts, i.e. the edge counts for all pairs of samples except the last pair \eqn{k-1} and \eqn{k}.

This implementation is a wrapper function around the function \code{\link[gTestsMulti]{gtestsmulti}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see the \code{\link[gTestsMulti]{gtestsmulti}}.
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
\item{p.value }{Permutation p value (only if \code{n.perm} > 0)}
\item{estimate }{Estimated KMD value}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
}
\references{
Song, H. and Chen, H. (2022). New graph-based multi-sample tests for high-dimensional and non- Euclidean data. \doi{10.48550/arXiv.2205.13787}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link{gTestsMulti}} for performing both tests at once, \code{\link{MST}}
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform Song and Chen test 
if(requireNamespace("gTestsMulti", quietly = TRUE)) {
  SC(X1, X2, n.perm = 100)
  SC(X1, X2, n.perm = 100, type = "SA")
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ k-sample }
\keyword{ graph-based }
\concept{ dataset similarity }