\name{findSimilarityMethod}
\alias{findSimilarityMethod}
\encoding{UTF-8}
\title{
Selection of Appropriate Methods for Quantifying the Similarity of Datasets
}
\description{
Find a dataset similarity method for the dataset comparison at hand and display information on suitable methods.
}
\usage{
findSimilarityMethod(Numeric = FALSE, Categorical = FALSE, 
                      Target.Inclusion = FALSE, Multiple.Samples = FALSE, 
                      only.names = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Numeric}{
  Is it required that the method is applicable to numeric data? (default: \code{FALSE})
}
\item{Categorical}{
  Is it required that the method is applicable to categorical data? (default: \code{FALSE})
}
\item{Target.Inclusion}{
  Is it required that the method is applicable to datasets that include a target variable? (default: \code{FALSE})
}
\item{Multiple.Samples}{
  Is it required that the method is applicable to multiple datasets simultaneously? (default: \code{FALSE})
}
\item{only.names}{
  Should only the function names be returned? (default: \code{TRUE}, only names are returned. Setting this to \code{FALSE} returns the whole method table, see \code{\link{method.table}})
}
\item{...}{
  Further criteria that the method should fulfill, see \code{colnames(method.table)}. Each criterion can be used as an argument by supplying \code{criterion = TRUE} to obtain only methods that fulfill the respective criterion.
}
}
\details{
This function is intended to facilitate finding suitable methods. The criteria that a method should fulfill for the application at hand can be specified and a vector of the function names or the full information on the methods is returned.
}
\value{
Either a character vector of function names for \code{only.names = TRUE} or a subset of \code{\link{method.table}} of the selected methods for \code{only.names = FALSE}.
}
\references{
Article describing the criteria and taxonomy: Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 

Full interactive results table: \url{https://shiny.statistik.tu-dortmund.de/data-similarity/}

}

\seealso{
\code{\link{method.table}}, \code{\link{DataSimilarity}}
}
\examples{
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

\concept{ dataset similarity }
