\name{LHZStatistic}
\alias{LHZStatistic}
\encoding{UTF-8}
\title{
Calculation of the Li et al. (2022) empirical characteristic distance
}
\description{
The function calculates the \cite{Li et al. (2022)} empirical characteristic distance
}
\usage{
LHZStatistic(X1, X2)
}
\arguments{
  \item{X1}{
First dataset as matrix
}
  \item{X2}{
Second dataset as matrix
}
}

\value{
Returns the calculated value for the empirical characteristic distance
}
\references{
Li, X., Hu, W. and Zhang, B. (2022). Measuring and testing homogeneity of distributions by characteristic distance, Statistical Papers 64 (2), 529-556, \doi{10.1007/s00362-022-01327-7}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link{LHZ}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Calculate LHZ statistic
LHZStatistic(X1, X2)
}

\concept{ dataset similarity }
