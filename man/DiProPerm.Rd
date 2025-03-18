\name{DiProPerm}
\alias{DiProPerm}
\encoding{UTF-8}
\title{
Direction-Projection-Permutation (DiProPerm) Test 
}
\description{
Performs the Direction-Projection-Permutation (DiProPerm) two-sample test for high-dimensional data (\cite{Wei et al., 2016}). 
}
\usage{
DiProPerm(X1, X2, n.perm = 0, dipro.fun = dwdProj, stat.fun = MD, 
            direction = "two.sided", seed = 42)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{n.perm}{
Number of permutations for permutation test (default: 0, no permutation test performed)
}
  \item{dipro.fun}{
Function performing the direction and projection step using a linear classifier. 
Implemented options are \code{\link{dwdProj}} (default, distance weighted discrimination, DWD), and \code{\link{svmProj}} (support vector machine).
Must take the two datasets as input and output the calculated scores for the pooled sample. 
}
  \item{stat.fun}{
Function that calculates a univariate two-sample statistic from two vectors. 
Implemented options are \code{\link{MD}} (default, mean difference, recommended for detecting mean differendes), \code{\link{tStat}} (t test statistic) and \code{\link{AUC}} (area under the receiver operating curve).
Must take the two numeric vectors as input and output the two sample statistic as a numeric scalar. 
}
  \item{direction}{
Character indicating for which values of the univariate test statistic the test should reject the null hypothesis. 
Possible options are \code{"two.sided"} (reject both for low and high values, appropriate for \code{MD} and \code{tStat}), \code{"greater"} (reject for high values, appropriate for \code{AUC}), or \code{"smaller"} (reject for low values).
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
The DiProPerm test works by first combining the datasets into a pooled dataset and creating a target variable with the dataset membership of each observation. A binary linear classifier is then trained on the class labels and the normal vector of the separating hyperplane is calculated. The data from both samples is projected onto this normal vector. This gives a scalar score for each observation. On these projection scores, a univariate two-sample statistic is calculated. The permutation null distribution of this statistic is calculated by permuting the dataset labels and repeating the whole procedure with the permuted labels. 

At the moment, distance weighted discrimination (DWD), and support vector machine (SVM) are implemented as binary linear classifiers.

The DWD model implementation \code{\link[DWDLargeR:genDWD]{genDWD}} in the \pkg{DWDLargeR} package is used with the penalty parameter \code{C} calculated with \code{\link[DWDLargeR:penaltyParameter]{penaltyParameter}} using the recommended default values. More details on the algorithm can be found in \cite{Lam et al. (2018)}. 

For the SVM, the implementation \code{\link[e1071:svm]{svm}} in the \pkg{e1071} package is used with default parameters. 

Other classifiers can be used by supplying a suitable function for \code{\link{dipro.fun}}.

For the univariate test statistic, implemented options are the mean difference, t statistic and AUC. 
Other suitable statistics can be used by supplying a suitable function of \code{\link{stat.fun}}.

Whether high or low values of the test statistic correspond to similarity of the datasets depends on the chosen univariate statistic. 
This is reflected by the \code{direction} argument which modifies the behavior of the test to reject the null for appropriate values. 
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
\item{p.value}{Permutation p value}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
}
\references{
Lam, X. Y., Marron, J. S., Sun, D., & Toh, K.-C. (2018). Fast Algorithms for Large-Scale Generalized Distance Weighted Discrimination. Journal of Computational and Graphical Statistics, 27(2), 368-379. \doi{10.1080/10618600.2017.1366915}

Wei, S., Lee, C., Wichers, L., & Marron, J. S. (2016). Direction-Projection-Permutation for High-Dimensional Hypothesis Tests. Journal of Computational and Graphical Statistics, 25(2), 549-569. \doi{10.1080/10618600.2015.1027773}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\seealso{
\code{\link{stat.fun}}, \code{\link{dipro.fun}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform DiProPerm test 
# Note: For real applications, n.perm should be set considerably higher than 10
# Low values for n.perm chosen for demonstration due to runtime
\donttest{
if(requireNamespace("DWDLargeR", quietly = TRUE)) {
  DiProPerm(X1, X2, n.perm = 10)
  DiProPerm(X1, X2, n.perm = 10, stat.fun = tStat)
  if(requireNamespace("pROC", quietly = TRUE)) {
    DiProPerm(X1, X2, n.perm = 10, stat.fun = AUC, direction = "greater")
  }
}

if(requireNamespace("e1071", quietly = TRUE)) {
  DiProPerm(X1, X2, n.perm = 10, dipro.fun = svmProj)
  DiProPerm(X1, X2, n.perm = 10, dipro.fun = svmProj, stat.fun = tStat)
  if(requireNamespace("pROC", quietly = TRUE)) {
    DiProPerm(X1, X2, n.perm = 10, dipro.fun = svmProj, stat.fun = AUC, direction = "greater")
  }
}
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ binary classification }
\concept{ dataset similarity }
