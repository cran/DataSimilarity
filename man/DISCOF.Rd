\name{DISCOF}
\alias{DISCOF}
\encoding{UTF-8}
\title{
Distance Components (DISCO) Tests
}
\description{
Performs Energy statistics distance components (DISCO) multi-sample tests (\cite{Rizzo and Székely, 2010}). The implementation here uses the \code{\link[energy:disco]{disco}} implementation from the \pkg{energy} package.
}
\usage{
DISCOF(X1, X2, ..., n.perm = 0, alpha = 1, seed = 42)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{\dots}{
Further datasets as matrices or data.frames
}
  \item{n.perm}{
Number of permutations for Bootstrap test (default: 0, no Bootstrap test performed)
}
  \item{alpha}{
Power of the distance used for generalized Energy statistic (default: 1). Has to lie in \eqn{(0,2]}. For values in \eqn{(0, 2)}, consistency of the resulting test has been shown.
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
DISCO is a method for multi-sample testing based on all pairwise between-sample distances. It is analogous to the classical ANOVA. Instead of decomposing squared differences from the sample mean, the total dispersion (generalized Energy statistic) is composed into distance components (DISCO) consisting of the within-sample and between-sample measures of dispersion. 

\code{DISCOF} is based on the DISCO F ratio of the between-sample and within-sample dispersion. Note that the F ration does not follow an F distribution, but is just called F ratio analogous to the ANOVA.

In both cases, small values of the statistic indicate similarity of the datasets and therefore, the null hypothesis of equal distributions is rejected for large values of the statistic.

This implementation is a wrapper function around the function \code{\link[energy]{disco}} that modifies the in- and output of that function to match the other functions provided in this package. For more details see the \code{\link[energy]{disco}}. 
}
\section{Applicability}{
\tabular{llll}{
     Target variable?   \tab Numeric? \tab Categorical? \tab K-sample? \cr
     No \tab Yes \tab No \tab Yes \cr
  }
}
\value{
An object of class \code{disco} with the following components:
\item{call}{The function call}
\item{method }{Description of the test}
\item{statistic }{Vector of observed values of the test statistic}
\item{p.value}{Vector of Bootstrap p values}
\item{k}{Number of samples}
\item{N}{Number of observations}
\item{between}{Between-sample distance components}
\item{withins}{One-way within-sample distance components}
\item{within}{Within-sample distance component}
\item{total}{Total dispersion}
\item{Df.trt}{Degrees of freedom for treatments}
\item{Df.e}{Degrees of freedom for error}
\item{index}{Alpha (exponent on distance)}
\item{factor.names}{Factor names}
\item{factor.levels}{Factor levels}
\item{sample.sizes}{Sample sizes}
\item{stats}{Matrix containing decomposition}
}
\references{
Szekely, G. J. and Rizzo, M. L. (2004) Testing for Equal Distributions in High Dimension, InterStat, November (5).

Rizzo, M. L. and Szekely, G. J. (2010). DISCO Analysis: A Nonparametric Extension of Analysis of Variance, Annals of Applied Statistics, 4(2), 1034-1055.
doi:10.1214/09-AOAS245

Szekely, G. J. (2000) Technical Report 03-05: E-statistics: Energy of Statistical Samples, Department of Mathematics and Statistics, Bowling Green State University.

Rizzo, M., Szekely, G. (2022). energy: E-Statistics: Multivariate Inference via the Energy of Data. R package version 1.7-11, \url{https://CRAN.R-project.org/package=energy}.

Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 
}


\seealso{
\code{\link{DISCOB}}, \code{\link{Energy}}
}
\examples{
# Draw some data
X1 <- matrix(rnorm(1000), ncol = 10)
X2 <- matrix(rnorm(1000, mean = 0.5), ncol = 10)
# Perform DISCO tests
if(requireNamespace("energy", quietly = TRUE)) {
  DISCOF(X1, X2, n.perm = 100)
}
}
\keyword{ htest }
\keyword{ numeric }
\keyword{ k-sample }
\keyword{ inter-point distances }
\concept{ dataset similarity }
