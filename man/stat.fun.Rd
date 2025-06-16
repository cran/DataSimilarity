\name{stat.fun}
\alias{MD}
\alias{tStat}
\alias{AUC}
\alias{stat.fun}
\encoding{UTF-8}
\title{
Univariate Two-Sample Statistics for DiProPerm Test
}
\description{
Helper functions for calculating univariate two-sample statistic for the Direction-Projection-Permutation (DiProPerm) two-sample test for high-dimensional data (\cite{Wei et al., 2016})
}
\usage{
MD(x1, x2)
tStat(x1, x2)
AUC(x1, x2)
}
\arguments{
  \item{x1}{
Numeric vector of scores for the first sample.
}
  \item{x2}{
Numeric vector of scores for the second sample.
}
}
\details{
The DiProPerm test works by first combining the datasets into a pooled dataset and creating a target variable with the dataset membership of each observation.
A binary linear classifier is then trained on the class labels and the normal vector of the separating hyperplane is calculated. 
The data from both samples is projected onto this normal vector. This gives a scalar score for each observation.
On these projection scores, a univariate two-sample statistic is calculated. 
The permutation null distribution of this statistic is calculated by permuting the dataset labels and repeating the whole procedure with the permuted labels. 
The functions here correspond to the univariate two-sample statistics suggested in the original article of \cite{Wei et al., 2016}.
}
\value{
A numeric scalar giving the observed two-sample statistic value.
}
\references{
Wei, S., Lee, C., Wichers, L., & Marron, J. S. (2016). Direction-Projection-Permutation for High-Dimensional Hypothesis Tests. Journal of Computational and Graphical Statistics, 25(2), 549-569. \doi{10.1080/10618600.2015.1027773}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 

}


\seealso{
\code{\link{DiProPerm}}
}
\examples{
set.seed(1234)
# Just for demonstration calculate univariate two-sample statistics separately
x1 <- rnorm(100)
x2 <- rnorm(100, mean = 0.5)
MD(x1, x2)
tStat(x1, x2)
if(requireNamespace("pROC", quietly = TRUE)) {
  AUC(x1, x2)
}

# Draw some multivariate data for the DiProPerm test
set.seed(1234)
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform DiProPerm test 
# Note: For real applications, n.perm should be set considerably higher
# Low values for n.perm chosen for demonstration due to runtime
\donttest{
if(requireNamespace("DWDLargeR", quietly = TRUE)) {
  DiProPerm(X1, X2, n.perm = 10, stat.fun = MD)
  DiProPerm(X1, X2, n.perm = 10, stat.fun = tStat)
  if(requireNamespace("pROC", quietly = TRUE)) {
    DiProPerm(X1, X2, n.perm = 10, stat.fun = AUC, direction = "greater")
  }
}
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ binary classification }
\concept{ dataset similarity }
