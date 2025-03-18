\name{GGRL}
\alias{GGRL}
\alias{GGRLCat}
\alias{f.s}
\alias{f.a}
\alias{f.sCat}
\alias{f.aCat}
\encoding{UTF-8}
\title{
Decision-Tree Based Measure of Dataset Distance and Two-Sample Test
}
\description{
Calculates Decision-Tree Based Measure of Dataset Distance by \cite{Ganti et al. (2002)}.
}
\usage{
GGRL(X1, X2, target1 = "y", target2 = "y", n.perm = 0, m = 1, diff.fun = f.a, 
      agg.fun = sum, tune = TRUE, k = 5, n.eval = 100, seed = 42, ...)
GGRLCat(X1, X2, target1 = "y", target2 = "y", n.perm = 0, m = 1, diff.fun = f.aCat, 
        agg.fun = sum, tune = TRUE, k = 5, n.eval = 100, seed = 42, ...)
f.a(sec.parti, X1, X2)
f.s(sec.parti, X1, X2)
f.aCat(sec.parti, X1, X2)
f.sCat(sec.parti, X1, X2)
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
  \item{n.perm}{
Number of permutations for permuation test (default: 0, no permutation test performed)
}
  \item{m}{
subsampling rate for Bootstrap test (default: 1). Ganti et al. (2002) suggest that 0.2-0.3 is sufficient in many cases. Ignored if \code{n.perm <= 0}.
}
  \item{diff.fun}{
Difference function as function (default: \code{f.a}, absolute difference). Other options: \code{f.s} (scaled difference), user specified function that takes greatest common refinement (GCR) partition and both datasets as input and returns vector of difference values for each section in the partition.
}
  \item{agg.fun}{
Aggregate function (default: \code{sum}). Other options are \code{max}, or user specified function that takes output of \code{diff.fun} and aggregates it into a single value. Note that only for \code{sum} it has been shown that the GCR is optimal.
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
Random seed (default: 42)
}
  \item{\dots}{
Further arguments passed to \code{\link[rpart:rpart]{rpart}}. Ignored if \code{tune = TRUE}.
}
  \item{sec.parti}{
Intersected partition as output by \code{calculateGCR}, i.e. a list containing the intersected partition and each partition on its own as dataframes with limits for each variable. 
}
}
\details{
The method first calculates the greatest common refinement (GCR), that is the intersection of the sample space partitions induced by a decision tree fit to the first dataset and a decision tree fit to the second dataset. The proportions of samples falling into each section of the GCR is calculated for each dataset. These proportions are compared using a difference function and the results of this are aggregated by the aggregate function.

The implementation uses \code{\link[rpart:rpart]{rpart}} for fitting classification trees to each dataset. 

\code{\link[e1071:best.rpart]{best.rpart}} is used for hyperparameter tuning if \code{tune = TRUE}. The parameters are tuned using cross-validation and random search. The parameter \code{minsplit} is tuned over \code{2^(1:7)}, \code{minbucket} is tuned over \code{2^(0:6)} and \code{cp} is tuned over \code{10^seq(-4, -1, by = 0.001)}. 

Pre-implemented methods for the difference function are \deqn{f_a(\kappa_1, \kappa_2, n_1, n_2) = |\frac{\kappa_1}{n_1} - \frac{\kappa_2}{n_2}|, }
and \deqn{f_s(\kappa_1, \kappa_2, n_1, n_2) = \frac{|\frac{\kappa_1}{n_1} - \frac{\kappa_2}{n_2}|}{(\frac{\kappa_1}{n_1} + \frac{\kappa_2}{n_2}) / 2}, \text{ if }\kappa_1+\kappa_2>0,}\deqn{= 0 \text{ otherwise,}} where \eqn{\kappa_i} is the number of observations from dataset \eqn{i} in the respective region of the greatest common refinement and \eqn{n_i} are the sample sizes, \eqn{i = 1, 2}.

The aggregate function aggregates the results of the difference function over all regions in the greatest common refinement.
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab Yes \tab No \cr
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
Ganti, V., Gehrke, J., Ramakrishnan, R. and Loh W.-Y. (2002). A Framework for Measuring Differences in Data Characteristics, Journal of Computer and System Sciences, 64(3), \doi{10.1006/jcss.2001.1808}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\note{
  The categorical method might not work properly if certain combinations of the categorical variables are not present in both datasets. This might happen e.g. for a large number of categories or variables and for small numbers of observations. In this case it might happen that the decision tree of the dataset where the combination is missing is unable to match a level of the split variable to one of the child nodes. Therefore, this combination is not part of the partition of the sample space induced by the tree and therefore also not of the greatest common refinement. Thus, some points of the other dataset cannot be sorted into any region of the greatest common refinement and the probabilities in the joint distribution calculated over the greatest common refinement do not sum up to one anymore. A warning is printed in these cases. It is unclear how this affects the performance. 
  
  Note that for small numbers of categories and deep trees it might also happen that the greatest common refinement reduces to all observed combinations of categories in the variables. Then the dataset distance measures is just a complicated way to measure the difference in frequencies of all observed combinations. 
}


\seealso{
\code{\link{NKT}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
y1 <- rbinom(100, 1, 1 / (1 + exp(1 - X1 \%*\% rep(0.5, 10))))
y2 <- rbinom(100, 1, 1 / (1 + exp(1 - X2 \%*\% rep(0.7, 10))))
X1 <- data.frame(X = X1, y = y1)
X2 <- data.frame(X = X2, y = y2)
# Calculate Ganti et al. statistic (without tuning and testing due to runtime)
if(requireNamespace("rpart", quietly = TRUE)) {
  GGRL(X1, X2, "y", "y", tune = FALSE)
}

# Categorical case
set.seed(1234)
X1 <- data.frame(X1 = factor(sample(letters[1:5], 1000, TRUE)), 
                 X2 = factor(sample(letters[1:4], 1000, TRUE)), 
                 X3 = factor(sample(letters[1:3], 1000, TRUE)), 
                 y = sample(0:1, 100, TRUE))
X2 <- data.frame(X1 = factor(sample(letters[1:5], 1000, TRUE, 1:5)), 
                 X2 = factor(sample(letters[1:4], 1000, TRUE, 1:4)), 
                 X3 = factor(sample(letters[1:3], 1000, TRUE, 1:3)), 
                 y = sample(0:1, 100, TRUE))
# Calculate Ganti et al. statistic (without tuning and testing due to runtime)
if(requireNamespace("rpart", quietly = TRUE)) {
  GGRLCat(X1, X2, "y", "y", tune = FALSE)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ categorical }
\keyword{ comparison of density functions }
\concept{ dataset similarity }