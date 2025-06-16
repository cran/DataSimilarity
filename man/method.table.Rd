\name{method.table}
\alias{method.table}
\encoding{UTF-8}
\docType{data}
\title{
List of Methods Included in the Package
}
\description{
The dataset contains the subset of methods that are implemented in the \pkg{DataSimilarity} package from the results table of \cite{Stolte et al. (2024)}. 
}
\usage{data("method.table")}
\format{
  A data frame with 42 observations on the following 30 variables that include information on whether or not the method fulfills the theoretical criteria of \cite{Stolte et al. (2024)}. 
  Some criteria are only fulfilled for certain parameter choices of the method ("Conditionally Fulfilled") or do not apply to the method. 
  \code{NA} values mean that there is no information available on whether or not the respective criterion is fulfilled.
  \describe{
    \item{\code{Method}}{a character vector giving the reference or method name}
    \item{\code{Implementation}}{a character vector giving the function name of the implementation in the \pkg{DataSimilarity} package}
    \item{\code{Target.Inclusion}}{a character vector. Can the method handle datasets that include a target variable in a meaningful way?}
    \item{\code{Numeric}}{a character vector. Can the method handle numeric data?}
    \item{\code{Categorical}}{a character vector. Can the method handle categorical data?}
    \item{\code{Unequal.Sample.Sizes}}{a character vector. Can the method handle datasets of different sample sizes?}
    \item{\code{p.Larger.N}}{a character vector. Can the method handle datasets with more variables than observations?}
    \item{\code{Multiple.Samples}}{a character vector. Can the method handle \eqn{k > 2} datasets simultaneously?}
    \item{\code{Without.training}}{a character vector. Does the method work without holding out training data?}
    \item{\code{No.assumptions}}{a character vector. Does the method work without further assumptions?}
    \item{\code{No.parameters}}{a character vector. Does the method work without the specification or tuning of additional parameters?}
    \item{\code{Implemented}}{a character vector. Is the method implemented elsewhere? (NA if no other implementations are known)}
    \item{\code{Complexity}}{a character vector giving the computational complexity of the method.}
    \item{\code{Interpretable.units}}{a character vector. Can a one unit increase of the output value be interpreted?}
    \item{\code{Lower.bound}}{a character vector. Are the output values lower bounded? If known the lower bound is given.}
    \item{\code{Upper.bound}}{a character vector. Are the output values upper bounded? If known the upper bound is given.}
    \item{\code{Rotation.invariant}}{a character vector. Is the method invariant to rotation of all datasets?}
    \item{\code{Location.change.invariant}}{a character vector. Is the method invariant to shifting all datasets?}
    \item{\code{Homogeneous.scale.invariant}}{a character vector. Is the method invariant to scaling all datasets?}
    \item{\code{Positive.definite}}{a character vector. Is the method positive definite, i.e. \eqn{d(F_1, F_2) \ge 0} and \eqn{d(F_1, F_2) = 0 \Leftrightarrow F_1 = F_2} for any two distributions \eqn{F_1, F_2}?}
    \item{\code{Symmetric}}{a character vector. Ist the method symmetric, i.e. \eqn{d(F_1, F_2) = d(F_2, F_1)} for any two distributions \eqn{F_1, F_2}?}
    \item{\code{Triangle.inequality}}{a character vector. Does the method fulfill the triangle inequality, i.e. \eqn{d(F_1, F_2) \le d(F_1, F_3) + d(F_3, F_2)} for any three distributions \eqn{F_1, F_2, F_3}?}
    \item{\code{Consistency.N}}{a character vector. Is the corresponding test consistent for \eqn{N\to\infty}?}
    \item{\code{Consistency.p}}{a character vector. Is the corresponding test consistent for \eqn{p\to\infty}?}
    \item{\code{Number.Fulfilled}}{a numeric vector. Number of fulfilled criteria.}
    \item{\code{Number.Cond.Fulfilled}}{a numeric vector. Number of conditionally fulfilled criteria.}
    \item{\code{Number.Unfulfilled}}{a numeric vector. Number of unfulfilled criteria.}
    \item{\code{Number.NA}}{a numeric vector. Number of criteria for which it is unknown if they are fulfilled.}
    \item{\code{Class}}{a character vector. Class of the taxonomy of \cite{Stolte et al. (2024)} that the method is assigned to based on its underlying idea.}
    \item{\code{Subclass}}{a character vector. Subclass of the taxonomy of \cite{Stolte et al. (2024)} that the method is assigned to based on its underlying idea.}
  }
}
\details{
The dataset is based on the results of \cite{Stolte et al. (2024)}. For explanations on the criteria and on the taxonomy and classes, please refer to that publication. 
A full version of the table can also be found at \url{https://shiny.statistik.tu-dortmund.de/data-similarity/}.
}
\source{
Article describing the criteria and taxonomy: Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 

Full interactive results table: \url{https://shiny.statistik.tu-dortmund.de/data-similarity/}
}

\examples{
data("method.table")

# Workflow for using the DataSimilarity package: 
# Prepare data example: comparing species in iris dataset
data("iris")
iris.split <- split(iris[, -5], iris$Species)
setosa <- iris.split$setosa
versicolor <- iris.split$versicolor
virginica <- iris.split$virginica

# 1. Find appropriate methods that can be used to compare 3 numeric datasets:
findSimilarityMethod(Numeric = TRUE, Multiple.Samples = TRUE)

# get more information 
findSimilarityMethod(Numeric = TRUE, Multiple.Samples = TRUE, only.names = FALSE)

# 2. Choose a method and apply it:
# All suitable methods
possible.methds <- findSimilarityMethod(Numeric = TRUE, Multiple.Samples = TRUE, 
                                          only.names = FALSE)
# Select, e.g., method with highest number of fulfilled criteria
possible.methds$Implementation[which.max(possible.methds$Number.Fulfilled)]

set.seed(1234)
if(requireNamespace("KMD")) {
  DataSimilarity(setosa, versicolor, virginica, method = "KMD")
}

# or directly 
set.seed(1234)
if(requireNamespace("KMD")) {
  KMD(setosa, versicolor, virginica)
}
}
\keyword{datasets}
