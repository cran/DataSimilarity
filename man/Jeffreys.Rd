\name{Jeffreys}
\alias{Jeffreys}
\encoding{UTF-8}
\title{
Jeffreys divergence
}
\description{
The function implements Jeffreys divergence by using KL Divergence Approximation (\cite{Sugiyama et al. 2013}). By default, the implementation uses method KLIEP of function \code{\link[densratio]{densratio}} from the \pkg{densratio} package for density ration estimation.
}
\usage{
Jeffreys(X1, X2, method = "KLIEP", verbose = FALSE, seed = 42)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame  
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{method}{
"KLIEP" (default), "uLSIF" or "RuLSIF"
}
  \item{verbose}{
logical (default: FALSE)
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
Jeffreys divergence is calculated as the sum of the two KL-divergences
\deqn{\text{KL}(F_1, F_2) = \int \log\left(\frac{f_1}{f_2}\right) \text{d}F_1}
where each dataset is used as the first dataset once. As suggested by \cite{Sugiyama et al. (2013)} the method KLIEP is used for density ratio estimation by default. Low values of Jeffreys Divergence indicate similarity.  
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
\item{p.value}{p value}
\item{method }{Description of the test}
\item{data.name }{The dataset names}
\item{alternative }{The alternative hypothesis}
}
\references{
Makiyama, K. (2019). densratio: Density Ratio Estimation. R package version 0.2.1, \url{https://CRAN.R-project.org/package=densratio}.
  
Sugiyama, M. and Liu, S. and Plessis, M. and Yamanaka, M. and Yamada, M. and Suzuki, T. and Kanamori, T. (2013). Direct Divergence Approximation between Probability Distributions and Its Applications in Machine Learning. Journal of Computing Science and Engineering. 7. \doi{10.5626/JCSE.2013.7.2.99}

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link[densratio:densratio]{densratio}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Calculate Jeffreys divergence 
if(requireNamespace("densratio", quietly = TRUE)) {
  Jeffreys(X1, X2)
}
}
\keyword{ numeric }
\keyword{ divergence }
\concept{ dataset similarity }
