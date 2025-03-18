\name{rectPartition}
\alias{rectPartition}
\encoding{UTF-8}
\title{
Calculate a rectangular partition
}
\description{
The function calculates a rectangular partition of the subspace spanned by the data. Used for \code{\link{BG}}.
}
\usage{
rectPartition(X1, X2, n, p, exponent = 0.8, eps = 0.01)
}
\arguments{
  \item{X1}{
First data set as matrix 
}
  \item{X2}{
Second data set as matrix
}
  \item{n}{
Number of rows in the data
}
  \item{p}{
Number of columns in the data
}
  \item{exponent}{
Exponent to ensure covergence criteria, should be between 0 and 1 (default: 0.8)
}
  \item{eps}{
Small threshold to guarantee edge points are included (default: 0.01)
}
}


\value{
A list with the following components:
\item{A }{A list of \code{p} elements containing the partition cutpoints for every dimension}
\item{m_n }{Total number of elements in the partition}
\item{m_n_d }{Number of partition elements per dimension}
}
\references{
Biau G. and Gyorfi, L. (2005). On the asymptotic properties of a nonparametric \eqn{L_1}-test statistic of homogeneity, IEEE Transactions on Information Theory, 51(11), 3965-3973. \doi{10.1109/TIT.2005.856979}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link{BG}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 5)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 5)
# Calculate partition
rectPartition(X1, X2, n = nrow(X1), p = ncol(X1))
}

\concept{ dataset similarity }
