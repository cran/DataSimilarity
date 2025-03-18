\name{KMD}
\alias{KMD}
\encoding{UTF-8}
\title{
Kernel Measure of Multi-Sample Dissimilarity (KMD)
}
\description{
Calculates the kernel measure of multi-sample dissimilarity (KMD) and performs a permutation multi-sample test (\cite{Huang and Sen, 2023}). The implementation here uses the \code{\link[KMD]{KMD}} and \code{\link[KMD]{KMD_test}} implementations from the \pkg{KMD} package.
}
\usage{
KMD(X1, X2, ..., n.perm = 0, graph = "knn", k = ceiling(N/10), 
    kernel = "discrete", seed = 42)
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
Number of permutations for permutation test (default: 0, no permutation test performed). 
}
  \item{graph}{
Graph used in calculation of KMD. Possible options are \code{"knn"} (default) and \code{"mst"}.
}
  \item{k}{
Number of neighbors for construction of \code{k}-nearest neighbor graph. Ignored for \code{graph = "mst"}.
}
  \item{kernel}{
Kernel used in calculation of KMD. Can either be \code{"discrete"} (default) for use of the discrete kernel or a kernal matrix with numbers of rows and columns corresponding to the number of datasets. For the latter, the entry in the \eqn{i}-th row and \eqn{j}-th column corresponds to the kernel value \eqn{k(i,j)}.
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
Given the pooled sample \eqn{Z_1, \dots, Z_N} and the corresponding sample memberships \eqn{\Delta_1,\dots, \Delta_N} let \eqn{\mathcal{G}} be a geometric graph on \eqn{\mathcal{X}} such that an edge between two points \eqn{Z_i} and \eqn{Z_j} in the pooled sample implies that \eqn{Z_i} and \eqn{Z_j} are close, e.g. \eqn{K}-nearest neighbor graph with \eqn{K\ge 1} or MST. Denote by \eqn{(Z_i,Z_j)\in\mathcal{E}(\mathcal{G})} that there is an edge in \eqn{\mathcal{G}} connecting \eqn{Z_i} and \eqn{Z_j}. Moreover, let \eqn{o_i} be the out-degree of \eqn{Z_i} in \eqn{\mathcal{G}}. Then an estimator for the KMD \eqn{\eta} is defined as 
\deqn{\hat{\eta} := \frac{\frac{1}{N} \sum_{i=1}^N \frac{1}{o_i} \sum_{j:(Z_i,Z_j)\in\mathcal{E}(\mathcal{G})} K(\Delta_i, \Delta_j) - \frac{1}{N(N-1)} \sum_{i\ne j} K(\Delta_i, \Delta_j)}{\frac{1}{N}\sum_{i=1}^N K(\Delta_i, \Delta_i) - \frac{1}{N(N-1)} \sum_{i\ne j} K(\Delta_i, \Delta_j)}.}


Euclidean distances are used for computing the KNN graph (ties broken at random) and the MST. 

For \code{n.perm == 0}, an asymptotic test using the asymptotic normal approximation of the null distribution is performed. For this, the KMD is standardized by the null mean and standard deviation. For \code{n.perm > 0}, a permutation test is performed, i.e. the observed KMD statistic is compared to the permutation KMD statistics. 

The theoretical KMD of two distributions is zero if and only if the distributions coincide. It is upper bound by one. Therefore, low values of the empirical KMD indicate similarity and the test rejects for high values. 

\cite{Huang and Sen (2023)} recommend using the \eqn{k}-NN graph for its flexibility, but the choice of \eqn{k} is unclear. Based on the simulation results in the original article, the recommended values are \eqn{k = 0.1 N} for testing and \eqn{k = 1} for estimation. For increasing power it is beneficial to choose large values of \eqn{k}, for consistency of the tests, \eqn{k = o(N / \log(N))} together with a continuous distribution of inter-point distances is sufficient, i.e. \eqn{k} cannot be chosen too large compared to \eqn{N}. On the other hand, in the context of estimating the KMD, choosing \eqn{k} is a bias-variance trade-off with small values of \eqn{k} decreasing the bias and larger values of \eqn{k} decreasing the variance (for more details see discussion in Appendix D.3 of \cite{Huang and Sen (2023)}).

This implementation is a wrapper function around the functions \code{\link[KMD]{KMD}} and \code{\link[KMD]{KMD_test}} that modifies the in- and output of those functions to match the other functions provided in this package. For more details see \code{\link[KMD]{KMD}} and \code{\link[KMD]{KMD_test}}. 
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
\item{p.value }{Permutation / asymptotic p value}
\item{estimate }{Estimated KMD value}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{graph }{Graph used for calculation}
\item{k }{Number of neighbors used if \code{graph} is the KNN graph.}
\item{kernel }{Kernel used for calculation}
}
\references{
Huang, Z. and Sen, B. (2023). A Kernel Measure of Dissimilarity between \eqn{M} Distributions. Journal of the American Statistical Association, 0, 1-27. \doi{10.1080/01621459.2023.2298036}.

Huang, Z. (2022). KMD: Kernel Measure of Multi-Sample Dissimilarity. R package version 0.1.0, \url{https://CRAN.R-project.org/package=KMD}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link{MMD}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform KMD test 
if(requireNamespace("KMD", quietly = TRUE)) {
  KMD(X1, X2, n.perm = 100)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ k-sample }
\keyword{ kernel-based }
\concept{ dataset similarity }