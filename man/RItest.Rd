\name{RItest}
\alias{RItest}
\encoding{UTF-8}
\title{
Multisample RI Test
}
\description{
Performs the (modified/ multiscale/ aggregated) RI test (\cite{Paul et al., 2021}). The implementation is based on the \code{\link[HDLSSkST]{RItest}}, \code{\link[HDLSSkST]{MTRItest}}, and \code{\link[HDLSSkST]{ARItest}} implementations from the \pkg{HDLSSkST} package.
}
\usage{
RItest(X1, X2, ..., n.clust, randomization = TRUE, version = "original", 
        mult.test = "Holm", kmax = 2 * n.clust, s.psi = 1, s.h = 1, 
        lb = 1, n.perm = 1/alpha, alpha = 0.05, seed = 42)
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
  \item{n.clust}{
Number of clusters (only applicable for \code{version = "original"}).
}
  \item{randomization}{
Should a randomized test be performed? (default: \code{TRUE}, ranomized test is performed)
}
  \item{version}{
Which version of the test should be performed? Possible options are \code{"original"} (default) for the FS test, \code{"modified"} for the MFS test (number of clusters is estimated), \code{"multiscale"} for the MSFS test (all numbers of clusters up to \code{kmax} are tried and results are summarized), \code{"aggregated-knw"} (all pairwise comparisons are tested with the FS test and results are aggregated), and \code{"aggregated-est"} (all pairwise comparisons are tested with the MFS test and results are aggregated).
}
  \item{mult.test}{
Multiple testing adjustment for AFS test and MSFS test. Possible options are \code{"Holm"} (default) and \code{"BenHoch"}.
}
  \item{kmax}{
Maximum number of clusters to try for estimating the number of clusters (default: \code{2*n.clust}).
}
  \item{s.psi}{
Numeric code for function required for calculating the distance for \eqn{K}-means clustering. 
The value \code{1} corresponds to \eqn{\psi(t) = t^2} (the default), 
\code{2} corresponds to \eqn{\psi(t) = 1 - \exp(-t)},
\code{3} corresponds to \eqn{\psi(t) = 1 - \exp(-t^2)},
\code{4} corresponds to \eqn{\psi(t) = \log(1 + t)},
\code{5} corresponds to \eqn{\psi(t) = t}.
}
  \item{s.h}{
Numeric code for function required for calculating the distance for \eqn{K}-means clustering. 
The value \code{1} corresponds to \eqn{h(t) = \sqrt{t}} (the default), and 
\code{2} corresponds to \eqn{h(t) = t}.
}
  \item{lb}{
Length of smaller vectors into which each observation is partitioned (default: 1).
}
  \item{n.perm}{
Number of simulations of the test statistic (default: 1/alpha, minimum number required for running the test, set to a higher value for meaningful test results).
}
  \item{alpha}{
Test level (default: 0.05).
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
The tests are intended for the high dimension low sample size (HDLSS) setting. The idea is to cluster the pooled sample using a clustering algorithm that is suitable for the HDLSS setting and then to compare the clustering to the true dataset membership using the Rand index.
For the original RI test, the number of clusters has to be specified. If no number is specified it is set to the number of samples. 
This is a reasonable number of clusters in many cases. 

However, in some cases, different numbers of clusters might be needed. 
For example in case of multimodal distributions in the datasets, there might be multiple clusters within each dataset. 
Therefore, the modified (MRI) test allows to estimate the number of clusters from the data. 

In case of a really unclear number of clusters, the multiscale (MSRI) test can be applied which calculates the test for each number of clusters up to \code{kmax} and then summarizes the test results using some adjustment for multiple testing. 

These three tests take into account all samples simultaneously. The aggregated (ARI) test instead performs all pairwise FS or MFS tests on the samples and aggregates those results by taking the minimum test statistic value and applying a multiple testing procedure.

For clustering, a \eqn{K}-means algorithm using the generalized version of the Mean Absolute Difference of Distances (MADD) (\cite{Sarkar and Ghosh, 2020}) is applied. 
The MADD is defined as 
\deqn{
\rho_{h,\varphi}(z_i, z_j) = \frac{1}{N-2} \sum_{m\in \{1,\dots, N\}\setminus\{i,j\}} \left| \varphi_{h,\psi}(z_i, z_m) - \varphi_{h,\psi}(z_j, z_m)\right|,
}
where \eqn{z_i \in\mathbb{R}^p, i = 1,\dots,N}, denote points from the pooled sample and 
\deqn{\varphi_{h,\psi}(z_i, z_j) = h\left(\frac{1}{p}\sum_{i=l}^p\psi|z_{il} - z_{jl}|\right),}
with \eqn{h:\mathbb{R}^{+} \to\mathbb{R}^{+}} and \eqn{\psi:\mathbb{R}^{+} \to\mathbb{R}^{+}} continuous and strictly increasing functions. 
The functions \eqn{h} and \eqn{\psi} can be set via changing \code{s.psi} and \code{s.h}.

In all cases, high values of the test statistic correspond to similarity between the datasets. Therefore, the null hypothesis of equal distributions is rejected for low values. 
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
\item{p.value}{Asymptotic p value}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{est.cluster.label}{The estimated cluster label (not for AFS and MSFS)}
\item{observed.cont.table}{The observed contingency table of dataset membership and estimated cluster label (not for AFS)}
\item{crit.value}{The critical value of the test (not for MSFS)}
\item{random.gamma}{The randomization constant of the test (not for MSFS)}
\item{decision}{The (overall) test decision}
\item{decision.per.k}{The test decisions of all individual tests (only for MSFS)}
\item{est.cluster.no}{The estimated number of clusters (not for MSFS)}
}
\references{
Paul, B., De, S. K. and Ghosh, A. K. (2021). Some clustering based exact distribution-free k-sample tests applicable to high dimension, low sample size data, Journal of Multivariate Analysis, \doi{10.1016/j.jmva.2021.104897}

Rand, W. M. (1971). Objective criteria for the evaluation of clustering methods, Journal of the American Statistical association, 66(336):846-850, \doi{10.1080/01621459.1971.10482356}

Holm, S. (1979). A simple sequentially rejective multiple test procedure, Scandinavian journal of statistics, 65-70

Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing, Journal of the Royal statistical society: series B (Methodological) 57.1: 289-300, \doi{10.1111/j.2517-6161.1995.tb02031.x}

Sarkar, S. and Ghosh, A. K. (2020). On Perfect Clustering of High Dimension, Low Sample Size Data. IEEE Transactions on Pattern Analysis and Machine Intelligence 42 2257-2272. \doi{10.1109/TPAMI.2019.2912599}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\note{
In case of \code{version = "multiscale"} the output is a list object and not of class \code{htest} as there are multiple test statistic values and corresponding p values.

Note that the aggregated test cannot handle univariate data. 
}

\seealso{
\code{\link{FStest}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
if(requireNamespace("HDLSSkST", quietly = TRUE)) {
  # Perform RI test 
  RItest(X1, X2, n.clust = 2)
  # Perform MRI test
  RItest(X1, X2, version = "modified")
  # Perform MSRI
  RItest(X1, X2, version = "multiscale")
  # Perform ARI test 
  RItest(X1, X2, n.clust = 2, version = "aggregated-knw")
  RItest(X1, X2, version = "aggregated-est")
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ k-sample }
\keyword{ testing approach }
\concept{ dataset similarity }
