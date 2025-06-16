\name{dipro.fun}
\alias{dwdProj}
\alias{svmProj}
\alias{dipro.fun}
\encoding{UTF-8}
\title{
Direction-Projection Functions for DiProPerm Test
}
\description{
Helper functions performing the direction and projection step using different classifiers for the Direction-Projection-Permutation (DiProPerm) two-sample test for high-dimensional data (\cite{Wei et al., 2016})
}
\usage{
dwdProj(X1, X2)
svmProj(X1, X2)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
}
\details{
The DiProPerm test works by first combining the datasets into a pooled dataset and creating a target variable with the dataset membership of each observation.
A binary linear classifier is then trained on the class labels and the normal vector of the separating hyperplane is calculated. 
The data from both samples is projected onto this normal vector. This gives a scalar score for each observation.
On these projection scores, a univariate two-sample statistic is calculated. 
The permutation null distribution of this statistic is calculated by permuting the dataset labels and repeating the whole procedure with the permuted labels. 
The functions here correspond to the direction and projection step for either the DWD or SVM classifier as proposed by \cite{Wei et al., 2016}.

The DWD model implementation \code{\link[DWDLargeR:genDWD]{genDWD}} in the \pkg{DWDLargeR} package is used with the penalty parameter \code{C} calculated with \code{\link[DWDLargeR:penaltyParameter]{penaltyParameter}} using the recommended default values. More details on the algorithm can be found in \cite{Lam et al. (2018)}. 

For the SVM, the implementation \code{\link[e1071:svm]{svm}} in the \pkg{e1071} package is used with default parameters. 

}
\value{
A numeric vector containing the projected values for each observation in the pooled sample
}
\references{
Lam, X. Y., Marron, J. S., Sun, D., & Toh, K.-C. (2018). Fast Algorithms for Large-Scale Generalized Distance Weighted Discrimination. Journal of Computational and Graphical Statistics, 27(2), 368-379. \doi{10.1080/10618600.2017.1366915}

Wei, S., Lee, C., Wichers, L., & Marron, J. S. (2016). Direction-Projection-Permutation for High-Dimensional Hypothesis Tests. Journal of Computational and Graphical Statistics, 25(2), 549-569. \doi{10.1080/10618600.2015.1027773}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 

}


\seealso{
\code{\link{DiProPerm}}
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)

# calculate projections separately (only for demonstration)
\donttest{
dwdProj(X1, X2)
}
svmProj(X1, X2)

# Use within DiProPerm test 
# Note: For real applications, n.perm should be set considerably higher
# No permutations chosen for demonstration due to runtime
\donttest{
if(requireNamespace("DWDLargeR", quietly = TRUE)) {
  DiProPerm(X1, X2, n.perm = 10, dipro.fun = dwdProj)
}
if(requireNamespace("e1071", quietly = TRUE)) {
  DiProPerm(X1, X2, n.perm = 10, dipro.fun = svmProj)
}
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ binary classification }
\concept{ dataset similarity }
