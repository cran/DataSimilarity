\name{YMRZL}
\alias{YMRZL}
\encoding{UTF-8}
\title{
Yu et al. (2007) Two-Sample Test
}
\description{
Performs the \cite{Yu et al. (2007)} two-sample test. The implementation here uses the \code{classifier_test} implementation from the \pkg{Ecume} package. 
}
\usage{
YMRZL(X1, X2, n.perm = 0, split = 0.7, control = NULL, 
       train.args = NULL, seed = 42)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{n.perm}{
Number of permutations for permutation test (default: 0, asymptotic test is performed).
}
  \item{split}{
Proportion of observations used for training
}
  \item{control}{
Control parameters for fitting. See \code{\link[caret:trainControl]{trainControl}}. Defaults to \code{caret::trainControl(method = "boot")} as recommended if \code{control = NULL}. The number of Bootstrap samples defaults to 25 and can be set by specifying the \code{number} argument of \code{caret::trainControl}.
}
  \item{train.args}{
Further arguments passed to \code{\link[caret:train]{train}} as a named list.
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
The two-sample test proposed by \cite{Yu et al. (2007)} works by first combining the datasets into a pooled dataset and creating a target variable with the dataset membership of each observation. The pooled sample is then split into training and test set and a classification tree is trained on the training data. The test classification error is then used as a test statistic. If the distributions of the datasets do not differ, the classifier will be unable to distinguish between the datasets and therefore the test error will be close to chance level. The test rejects if the test error is smaller than chance level. 

The tree model is fit by \code{\link[rpart]{rpart}} and the classification error for tuning is by default predicted using the Bootstrap .632+ estimator as recommended by \cite{Yu et al. (2007)}. 

For \code{n.perm > 0}, a permutation test is conducted. Otherwise, an asymptotic binomial test is performed.
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
\item{p.value}{Asymptotic p value}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{classifier }{Chosen classification method (tree)}
}
\references{
Yu, K., Martin, R., Rothman, N., Zheng, T., Lan, Q. (2007). Two-sample Comparison Based on Prediction Error, with Applications to Candidate Gene Association Studies. Annals of Human Genetics, 71(1). \doi{10.1111/j.1469-1809.2006.00306.x}

Lopez-Paz, D., and Oquab, M. (2022). Revisiting classifier two-sample tests. ICLR 2017. \url{https://openreview.net/forum?id=SJkXfE5xx}

Roux de Bezieux, H. (2021). Ecume: Equality of 2 (or k) Continuous Univariate and Multivariate Distributions. R package version 0.9.1, \url{https://CRAN.R-project.org/package=Ecume}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}

\note{
As the idea of the test is very similar to that of the classifier two-sample test by \cite{Lopez-Paz and Oquab (2022)}, the implementation here is based on that \code{\link{C2ST}}. Note that \cite{Lopez-Paz and Oquab (2022)} utilize the classification accuracy instead of the classification error. Moreover, they propose to use a binomial test instead of the permutation test proposed by \cite{Yu et al.}. Here, we implemented both the binomial and the permutation test. 
}

\seealso{
\code{\link{C2ST}}, \code{\link{HMN}} 
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform the Yu et al. test
YMRZL(X1, X2)
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ categorical }
\keyword{ binary classification }
\concept{ dataset similarity }
