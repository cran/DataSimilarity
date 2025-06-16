\name{NKT}
\alias{NKT}
\encoding{UTF-8}
\title{
Decision-Tree Based Measure of Dataset Similarity (\cite{Ntoutsi et al., 2008})
}
\description{
Calculates Decision-Tree Based Measure of Dataset Similarity by \cite{Ntoutsi et al. (2008)}.
}
\usage{
NKT(X1, X2, target1 = "y", target2 = "y", version = 1, tune = TRUE, k = 5, 
      n.eval = 100, seed = NULL, ...)
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
  \item{version}{
Number in \code{1:3} specifying the version for calculating dataset similarity (default:1). See details.
}
  \item{tune}{
Should the decision tree parameters be tuned? (default: \code{TRUE})
}
  \item{k}{
Number of folds used in cross-validation for parameter tuning (default: 5). Ignored if \code{tune = FALSE}.
}
  \item{n.eval}{
Number of evaluations for random search used for parameter tuning (default: 100). Ignored if \code{tune = FALSE}.
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
  \item{\dots}{
Further arguments passed to \code{\link[rpart:rpart]{rpart}}. Ignored if \code{tune = TRUE}.
}

}
\details{
\cite{Ntoutsi et al. (2008)} define three measures of datset similarity based on the intersection of the partitions of the sample space defined by the two decision trees fit to each dataset. Denote by \eqn{\hat{P}_X(\mathcal{X})} the proportion of observations in a dataset that fall into each segment of the joint partition and by \eqn{P_X(Y,\mathcal{X})} the proportion of observations in a dataset that fall into each segment of the joint partition and belong to each class. \deqn{s(p, q) = \sum_{i} \sqrt{p_i \cdot q_i}} defines the similarity index for two vectors \eqn{p} and \eqn{q}. Then the measures of similarity are defined by
\deqn{\text{NTO1} = s(\hat{P}_{X_1}(\mathcal{X}), \hat{P}_{X_2}(\mathcal{X})),}
\deqn{\text{NTO2} = s(\hat{P}_{X_1}(Y, \mathcal{X}), \hat{P}_{X_2}(Y, \mathcal{X})),}
\deqn{\text{NTO3} = S(Y|\mathcal{X})^{T} \hat{P}_{X_1 \cup X_2}(\mathcal{X}),}
where \eqn{S(Y|\mathcal{X})} is the similarity vector with elements 
\deqn{S(Y|\mathcal{X})_i = s(\hat{P}_{X_1}(Y|\mathcal{X})_{i \bullet}, \hat{P}_{X_2}(Y|\mathcal{X})_{i \bullet})} and index \eqn{i \bullet} denotes the \eqn{i}-th row.


The implementation uses \code{\link[rpart:rpart]{rpart}} for fitting classification trees to each dataset. 

\code{\link[e1071:best.rpart]{best.rpart}} is used for hyperparameter tuning if \code{tune = TRUE}. The parameters are tuned using cross-validation and random search. The parameter \code{minsplit} is tuned over \code{2^(1:7)}, \code{minbucket} is tuned over \code{2^(0:6)} and \code{cp} is tuned over \code{10^seq(-4, -1, by = 0.001)}. 

High values of each measure indicate similarity of the datasets. The measures are bounded between 0 and 1. 
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     Yes \tab Yes \tab No \tab No \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the test statistic}
\item{p.value}{NA (no p value calculated)}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{alternative }{The alternative hypothesis}
}
\references{
Ntoutsi, I., Kalousis, A. and Theodoridis, Y. (2008). A general framework for estimating similarity of datasets and decision trees: exploring semantic similarity of decision trees. Proceedings of the 2008 SIAM International Conference on Data Mining, 810-821. \doi{10.1137/1.9781611972788.7}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\seealso{
\code{\link{GGRL}}
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
if(requireNamespace("rpart", quietly = TRUE)) {
  # Calculate all three similarity measures (without tuning the trees due to runtime)
  NKT(X1, X2, "y", version = 1, tune = FALSE)
  NKT(X1, X2, "y", version = 2, tune = FALSE)
  NKT(X1, X2, "y", version = 3, tune = FALSE)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ target variable }
\keyword{ comparison of density functions }
\concept{ dataset similarity }