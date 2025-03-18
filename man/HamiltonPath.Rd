\name{HamiltonPath}
\alias{HamiltonPath}
\encoding{UTF-8}
\title{
Shortest Hamilton path
}
\description{
The function implements a heuristic approach to determine the shortest Hamilton path of a graph based on Kruskal's algorithm.
}
\usage{
HamiltonPath(X1, X2, seed = 42)
}
\arguments{
  \item{X1}{
First dataset as matrix
}
  \item{X2}{
Second dataset as matrix
}
  \item{seed}{
Random seed (default: 42)
}
}
\details{
Uses function \code{\link[rlemon]{IsAcyclic}} from package \pkg{rlemon} to check if the addition of an edge leads to a cyclic graph.
}
\value{
Returns an edge list containing only the edges needed to construct the Hamilton path
}

\seealso{
\code{\link{BMG}}
}
\examples{
# create data for two datasets
data <- data.frame(x = c(1.5, 2, 4, 5, 4, 6, 5.5, 8), 
                   y = c(6, 4, 5.5, 3, 3.5, 5.5, 7, 6), 
                   dataset = rep(c(1, 2), each = 4))

plot(data$x, data$y, pch = c(21, 19)[data$dataset])

# divide into the two datasets and calculate Hamilton path
X1 <- data[1:4, ]
X2 <- data[5:8, ]

if(requireNamespace("rlemon", quietly = TRUE)) {
  E <- HamiltonPath(X1, X2)
  
  # plot the resulting edges
  segments(x0 = data$x[E[, 1]], y0 = data$y[E[, 1]],
            x1 = data$x[E[, 2]], y1 = data$y[E[, 2]], 
            lwd = 2)
}
}

\concept{ dataset similarity }

