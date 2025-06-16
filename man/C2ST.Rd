\name{C2ST}
\alias{C2ST}
\encoding{UTF-8}
\title{
Classifier Two-Sample Test
}
\description{
The function implements the Classifier Two-Sample Test (C2ST) of \cite{Lopez-Paz & Oquab (2017)}. The comparison of multiple (\eqn{\ge 2}) samples is also possible. The implementation here uses the \code{\link[Ecume:classifier_test]{classifier_test}} implementation from the \pkg{Ecume} package. 
}
\usage{
C2ST(X1, X2, ..., split = 0.7, thresh = 0, classifier = "knn", control = NULL, 
      train.args = NULL, seed = NULL)
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
  \item{split}{
Proportion of observations used for training
}
  \item{thresh}{
Value to add to the null hypothesis value (default:0). The null hypothesis tested can be formulated as \eqn{H_0: t = p_0 + } \code{thresh}, where \eqn{t} denotes the test accuracy of the classifier and \eqn{p_0} is the chance level (proportion of largest dataset in pooled sample).
}
  \item{classifier}{
Classifier to use during training (default: \code{"knn"}). See details for possible options. 
}
  \item{control}{
Control parameters for fitting. See \code{\link[caret:trainControl]{trainControl}}. Defaults to \code{NULL} in which case it is set to \code{caret::trainControl(method = "cv")}.
}
  \item{train.args}{
Further arguments passed to \code{\link[caret:train]{train}} as a named list.
}
  \item{seed}{
Random seed (default: NULL). A random seed will only be set if one is provided.
}
}
\details{
The classifier two-sample test works by first combining the datasets into a pooled dataset and creating a target variable with the dataset membership of each observation. The pooled sample is then split into training and test set and a classifier is trained on the training data. The classification accuracy on the test data is then used as a test statistic. If the distributions of the datasets do not differ, the classifier will be unable to distinguish between the datasets and therefore the test accuracy will be close to chance level. The test rejects if the test accuracy is greater than chance level. 

All methods available for classification within the \pkg{caret} framework can be used as methods. A list of possible models can for example be retrieved via 

\code{names(caret::getModelInfo())[sapply(caret::getModelInfo(), function(x) "Classification" \%in\% x$type)]}

This implementation is a wrapper function around the function \code{\link[Ecume]{classifier_test}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see the \code{\link[Ecume]{classifier_test}}.
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab Yes \tab Yes \cr
  }
}
\value{
An object of class \code{htest} with the following components:
\item{statistic }{Observed value of the test statistic}
\item{p.value}{Asymptotic p value}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{classifier }{Chosen classification method}
}
\references{
Lopez-Paz, D., and Oquab, M. (2022). Revisiting classifier two-sample tests. ICLR 2017. \url{https://openreview.net/forum?id=SJkXfE5xx}. 

Roux de Bezieux, H. (2021). Ecume: Equality of 2 (or k) Continuous Univariate and Multivariate Distributions. R package version 0.9.1, \url{https://CRAN.R-project.org/package=Ecume}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link{HMN}}, \code{\link{YMRZL}}
}
\examples{
set.seed(1234)
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform classifier two-sample test 
if(requireNamespace("Ecume", quietly = TRUE)) {
  C2ST(X1, X2)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ binary classification }
\concept{ dataset similarity }
