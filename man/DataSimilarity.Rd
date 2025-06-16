\name{DataSimilarity}
\alias{DataSimilarity}
\encoding{UTF-8}
\title{
Dataset Similarity
}
\description{
Calculate the similarity of two or more datasets
}
\usage{
DataSimilarity(X1, X2, method, ...)
}
\arguments{
  \item{X1}{
First dataset as matrix or data.frame
}
  \item{X2}{
Second dataset as matrix or data.frame
}
  \item{method}{
Name of method for calculating the similarity of the supplied datasets as a character. See Details.
}
  \item{\dots}{
Further arguments passed on to \code{method}
}
}
\details{
The package includes various methods for calculating the similarity of two or more datasets. 
Appropriate methods for a specific situation can be found using the \code{\link{findSimilarityMethod}} function. 
In the following, the available methods are listed sorted by their applicability to datasets with different characteristics. 
We differentiate between the number of datasets (two vs. more than two), the type of data (numeric vs. categorical) and the presence of a target variable in each dataset (with vs. without target variable). 
Typically, this target variable has to be categorical. 
Methods might be applicable in multiple cases. 
Then, they are listed once in each case for which they can be used.
For the list of methods, see below.
}
\section{Methods for two numeric datasets without target variables}{
\describe{
\item{\code{\link{Bahr}} }{The \cite{Bahr (1996)} two-sample test. Compares two numeric datasets based on inter-point distances, special case of the test of \cite{Bahrinhaus and Franz (2010)} (\code{\link{BF}}).}
\item{\code{\link{BallDivergence}} }{Ball divergence based two- or \eqn{k}-sample test for numeric datasets. The Ball Divergence is the square of the measure difference over a given closed ball collection.}
\item{\code{\link{BF}} }{The \cite{Bahrinhaus and Franz (2010)} test. Compares two numeric datasets based on inter-point distances using a kernel function. Different kernel functions are tailored to detecting certain alternatives, e.g. shift or scale.}
\item{\code{\link{BG}} }{The \cite{Biau and Gyorfi (2005)} two-sample homogeneity test. Generalization of the Kolmogorov-Smirnov test for multivariate data, uses the \eqn{L_1}-distance between two empicial distribution functions restricted to a finite partition.}
\item{\code{\link{BG2}} }{The \cite{Biswas and Ghosh (2014)} two-sample test for high-dimensional data. Compares two numeric datasets based on inter-point distances by comparing the means of the distributions of the within-sample and between-sample distances of both samples.}
\item{\code{\link{BMG}} }{The \cite{Biswas, Mukhopadhyay and Gosh (2014)} distribution-free two-sample runs test. Compares two numeric datasets using the Shortest Hamiltonian Path in the pooled sample.}
\item{\code{\link{BQS}} }{The nearest-neighbor-based multivariate two-sample test of \cite{Barakat et al. (1996)}. Modifies the Schilling-Henze nearest neighbor tests (\code{\link{SH}}) such that the number of nearest neighbors does not have to be chosen.}
\item{\code{\link{C2ST}} }{Classifier Two-Sample Test (C2ST) of \cite{Lopez-Paz and Oquab (2017)}. Can be used for multiple samples and categorical data also. Uses the classification accuracy of a classifier that is trained to distinguish between the datasets.}
\item{\code{\link{CCS}} }{Weighted edge-count two-sample test for multivariate data proposed by \cite{Chen, Chen and Su (2018)}. The test is intended for comparing two samples with unequal sample sizes. It is a modification of the graph-based Friedman-Rafsky test \code{\link{FR}}.}
\item{\code{\link{CF}} }{Generalized edge-count two-sample test for multivariate data proposed by \cite{Chen and Friedman (2017)}. The test is intended for better simultaneous detection of shift and scale alternatives. It is a modification of the graph-based Friedman-Rafsky test \code{\link{FR}}.}
\item{\code{\link{Cramer}} }{The Cramér two-sample test \cite{(Baringhaus and Franz, 2004)}. Compares two numeric datasets based on inter-point distances, specialcase of the test of \cite{Bahrinhaus and Franz (2010)} (\code{\link{BF}}), equivalent to the Energy distance \code{\link{Energy}}.}
\item{\code{\link{DiProPerm}} }{Direction Projection Permutation test. Compares two numeric datasets using a linear classifier that distinguishes between the two datasets by projecting all observations onto the normal vector of that classifier and performing a permutation test using a univariate two-sample statistic on these projected scores.}
\item{\code{\link{DISCOB}}, \code{\link{DISCOF}} }{Energy statistics distance components (DISCO) (\cite{Rizzo and Székely, 2010}). Compares two or more numeric datasets based on a decomposition of the total variance similar to ANOVA but using inter-point distances. \code{\link{DISCOB}} uses the between-sample inter-point distances, \code{\link{DISCOF}} uses an F-type statistic that takes the within- and between-sample inter-point distances into account.}
\item{\code{\link{DS}} }{Multivariate rank-based two-sample test using measure transportation by \cite{Deb and Sen (2021)}. Uses a rank version of the \code{\link{Energy}} statistic.}
\item{\code{\link{Energy}} }{The Energy statistic multi-sample test (\cite{Székely and Rizzo, 2004}). Compares two or more numeric datasets based on inter-point distances. Equivalent to the \code{\link{Cramer}} test.}
\item{\code{\link{engineerMetric}} }{The \eqn{L_q}-engineer metric for comparing two multivariate distributions.}
\item{\code{\link{FR}} }{The Friedman-Rafsky two-sample test (original edge-count test) for multivariate data (\cite{Friedman and Rafsky, 1979}). Compares two numeric datasets using the number of edges connecting points from different samples in a similarity graph (e.g. MST) on the pooled sample.}
\item{\code{\link{FStest}} }{Modified/ multiscale/ aggregated FS test (\cite{Paul et al., 2021}). Compares two or more datasets in the high dimension low sample size (HDLSS) setting based on a Fisher test for the independence of a clustering of the data and the true dataset membership.}
\item{\code{\link{GPK}} }{Generalized permutation-based kernel two-sample test proposed by \cite{Song and Chen (2021)}. Modification of the \code{\link{MMD}} test intended to better detect differences in variances.}
\item{\code{\link{HMN}} }{Random-forest based two-sample test by \cite{Hediger et al. (2021)}. Uses the (OOB) classification error of a random forest that is trained to distinguish between two datasets. Can also be used with categorical data. }
\item{\code{\link{Jeffreys}} }{Jeffreys divergence. Symmetrized version of the Kullback-Leibler divergence.}
\item{\code{\link{KMD}} }{Kernel measure of multi-sample dissimilarity (KMD) by \cite{Huang and Sen (2023)}. Uses the association between the features and the sample membership to quantify the dissimilarity of the distributions of two or more numeric datasets.}
\item{\code{\link{LHZ}} }{Characteristic distance by \cite{Li et al. (2022)}. Compares two numeric datasets using their empirical characteristic functions.}
\item{\code{\link{MMCM}} }{Multisample Mahalanobis crossmatch (MMCM) test (\cite{Mukherjee et al., 2022}). Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Rosenbaum}} and to the \code{\link{Petrie}} test.}
\item{\code{\link{MMD}} }{Maximum Mean Discrepancy (MMD). Compares two numeric datasets using a kernel function. Measures the difference between distributions in the reproducing kernel Hilbert space induced by the chosen kernel function.}
\item{\code{\link{MW}} }{Nonparametric graph-based LP (GLP) multisample test proposed by \cite{Mokhopadhyay and Wang (2020)}. Compares two or more numeric datasets based on learning an LP graph kernel using a pre-specified number of LP components and performing clustering on the eigenvectors of the Laplacian matrix for this learned kernel.}
\item{\code{\link{Petrie}} }{\cite{Petrie (2016)} multi-sample cross-match test. Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Rosenbaum}} and to the \code{\link{MMCM}} test.}
\item{\code{\link{RItest}} }{Modified/ multiscale/ aggregated RI test (\cite{Paul et al., 2021}). Compares two or more datasets in the high dimension low sample size (HDLSS) setting based on the Rand index of a clustering of the data and the true dataset membership.}
\item{\code{\link{Rosenbaum}} }{\cite{Rosenbaum (2005)} two-sample cross-match test. Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Petrie}} and to the \code{\link{MMCM}} test.}
\item{\code{\link{SC}} }{Graph-based multi-sample test for high-dimensional data proposed by \cite{Song and Chen (2022)}. Uses the within- and between-sample edge counts in a similarity graph to compare two or more numeric datasets.}
\item{\code{\link{SH}} }{Schilling-Henze nearest neighbor test \cite{(Schilling, 1986; Henze, 1988)}. Uses the number of edges connecting points from different samples in a \eqn{K}-nearest neighbor graph on the pooled sample.}
\item{\code{\link{Wasserstein}} }{Wasserstein distance. Permutation two-sample test for numeric data using the \eqn{p}-Wasserstein distance.}
\item{\code{\link{YMRZL}} }{Tree-based test of \cite{Yu et al. (2007)}. Uses the classification error of a decision tree trained to distinguish between two datasets. Can also be used with categorical data.}
\item{\code{\link{ZC}} }{Max-type edge-count test (\cite{Zhang and Chen, 2019}). Enhancement of the Friedman-Rafsky test (original edge-count test, \code{\link{FR}})  that aims at detecting both location and scale alternatives and is more flexible than the generalized edge-count test of Chen and Friedman (2017) (\code{\link{CF}}).}
}
}

\section{Methods for two numeric datasets with target variables}{
\describe{
\item{\code{\link{GGRL}} }{Decision-tree based measure of dataset distance and two-sample test (\cite{Ganti et al., 2002}). Compares the proportions of datapoints of the two datasets falling into each section of the intersection of the partitions induced by fitting a decision tree on each dataset. }
\item{\code{\link{NKT}} }{Decision--tree based measure of dataset similarity by \cite{Ntoutsi et al. (2008)}. Uses density estimates based on the intersection of the partitions induced by fitting a decision tree on each dataset.}
\item{\code{\link{OTDD}} }{Optimal transport dataset distance (OTDD) (\cite{Alvarez-Melis and Fusi, 2020}). The distance combines the distance between features and the distance between label distributions. }
}
}

\section{Methods for more than two numeric datasets without target variables}{
\describe{
\item{\code{\link{BallDivergence}} }{Ball divergence based two- or \eqn{k}-sample test for numeric datasets. The Ball Divergence is the square of the measure difference over a given closed ball collection.}
\item{\code{\link{C2ST}} }{Classifier Two-Sample Test (C2ST) of \cite{Lopez-Paz and Oquab (2017)}. Can be used for multiple samples and categorical data also. Uses the classification accuracy of a classifier that is trained to distinguish between the datasets.}
\item{\code{\link{DISCOB}}, \code{\link{DISCOF}} }{Energy statistics distance components (DISCO) (\cite{Rizzo and Székely, 2010}). Compares two or more numeric datasets based on a decomposition of the total variance similar to ANOVA but using inter-point distances. \code{\link{DISCOB}} uses the between-sample inter-point distances, \code{\link{DISCOF}} uses an F-type statistic that takes the within- and between-sample inter-point distances into account.}
\item{\code{\link{Energy}} }{The Energy statistic multi-sample test (\cite{Székely and Rizzo, 2004}). Compares two or more numeric datasets based on inter-point distances. Equivalent to the \code{\link{Cramer}} test.}
\item{\code{\link{FStest}} }{Modified/ multiscale/ aggregated FS test (\cite{Paul et al., 2021}). Compares two or more datasets in the high dimension low sample size (HDLSS) setting based on a Fisher test for the independence of a clustering of the data and the true dataset membership.}
\item{\code{\link{KMD}} }{Kernel measure of multi-sample dissimilarity (KMD) by \cite{Huang and Sen (2023)}. Uses the association between the features and the sample membership to quantify the dissimilarity of the distributions of two or more numeric datasets.}
\item{\code{\link{MMCM}} }{Multisample Mahalanobis crossmatch (MMCM) test (\cite{Mukherjee et al., 2022}). Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Rosenbaum}} and to the \code{\link{Petrie}} test.}
\item{\code{\link{MW}} }{Nonparametric graph-based LP (GLP) multi-sample test proposed by \cite{Mokhopadhyay and Wang (2020)}. Compares two or more numeric datasets based on learning an LP graph kernel using a pre-specified number of LP components and performing clustering on the eigenvectors of the Laplacian matrix for this learned kernel.}
\item{\code{\link{Petrie}} }{\cite{Petrie (2016)} multi-sample cross-match test. Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Rosenbaum}} and to the \code{\link{MMCM}} test.}
\item{\code{\link{RItest}} }{Modified/ multiscale/ aggregated RI test (\cite{Paul et al., 2021}). Compares two or more datasets in the high dimension low sample size (HDLSS) setting based on the Rand index of a clustering of the data and the true dataset membership.}
\item{\code{\link{SC}} }{Graph-based multi-sample test for high-dimensional data proposed by \cite{Song and Chen (2022)}. Uses the within- and between-sample edge counts in a similarity graph to compare two or more numeric datasets.}
}
}

\section{Methods for two categorical datasets without target variables}{
\describe{
\item{\code{\link{C2ST}} }{Classifier Two-Sample Test (C2ST) of \cite{Lopez-Paz and Oquab (2017)}. Can be used for multiple samples and categorical data also. Uses the classification accuracy of a classifier that is trained to distinguish between the datasets.}
\item{\code{\link{CCS_cat}} }{Weighted edge-count two-sample test for multivariate data proposed by \cite{Chen, Chen and Su (2018)}. The test is intended for comparing two samples with unequal sample sizes. It is a modification of the graph-based Friedman-Rafsky test \code{\link{FR_cat}}.}
\item{\code{\link{CF_cat}} }{Generalized edge-count two-sample test for multivariate data proposed by \cite{Chen and Friedman (2017)}. The test is intended for better simultaneous detection of shift and scale alternatives. It is a modification of the graph-based Friedman-Rafsky test \code{\link{FR_cat}}.}
\item{\code{\link{CMDistance}} }{Constrained Minimum (CM) distance (\cite{Tatti, 2007}). Compares two categorical datasets using the distance of summaries.}
\item{\code{\link{FR_cat}} }{The Friedman-Rafsky two-sample test (original edge-count test) for multivariate data (\cite{Friedman and Rafsky, 1979}). Compares two numeric datasets using the number of edges connecting points from different samples in a similarity graph (e.g. MST) on the pooled sample.}
\item{\code{\link{HMN}} }{Random-forest based two-sample test by \cite{Hediger et al. (2021)}. Uses the (OOB) classification error of a random forest that is trained to distinguish between two datasets. Can also be used with categorical data. }
\item{\code{\link{MMCM}} }{Multisample Mahalanobis crossmatch (MMCM) test (\cite{Mukherjee et al., 2022}). Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Rosenbaum}} and to the \code{\link{Petrie}} test.}
\item{\code{\link{Petrie}} }{\cite{Petrie (2016)} multi-sample cross-match test. Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Rosenbaum}} and to the \code{\link{MMCM}} test.}
\item{\code{\link{YMRZL}} }{Tree-based test of \cite{Yu et al. (2007)}. Uses the classification error of a decision tree trained to distinguish between two datasets. Can also be used with categorical data.}
\item{\code{\link{ZC_cat}} }{Max-type edge-count test (\cite{Zhang and Chen, 2019}). Enhancement of the Friedman-Rafsky test (original edge-count test, \code{\link{FR}})  that aims at detecting both location and scale alternatives and is more flexible than the generalized edge-count test of Chen and Friedman (2017) (\code{\link{CF}}).}
}
}

\section{Methods for two categorical datasets with target variables}{
\describe{
\item{\code{\link{GGRLCat}} }{Decision-tree based measure of dataset distance and two-sample test (\cite{Ganti et al., 2002}). Compares the proportions of datapoints of the two datasets falling into each section of the intersection of the partitions induced by fitting a decision tree on each dataset. }
\item{\code{\link{OTDD}} }{Optimal transport dataset distance (OTDD) (\cite{Alvarez-Melis and Fusi, 2020}). The distance combines the distance between features and the distance between label distributions. }
}
}

\section{Methods for more than two categorical datasets without target variables}{
\describe{
\item{\code{\link{C2ST}} }{Classifier Two-Sample Test (C2ST) of \cite{Lopez-Paz and Oquab (2017)}. Can be used for multiple samples and categorical data also. Uses the classification accuracy of a classifier that is trained to distinguish between the datasets.}
\item{\code{\link{MMCM}} }{Multisample Mahalanobis crossmatch (MMCM) test (\cite{Mukherjee et al., 2022}). Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Rosenbaum}} and to the \code{\link{Petrie}} test.}
\item{\code{\link{Petrie}} }{\cite{Petrie (2016)} multi-sample cross-match test. Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Rosenbaum}} and to the \code{\link{MMCM}} test.}
}
}

\section{Methods for two datasets with both categorical and numeric variables but without target variables}{
\describe{
\item{\code{\link{BMG}} (in case of no ties, appropriate distance function has to be specified) }{The \cite{Biswas, Mukhopadhyay and Gosh (2014)} distribution-free two-sample runs test. Compares two numeric datasets using the Shortest Hamiltonian Path in the pooled sample.}
\item{\code{\link{BQS}} (in case of no ties, appropriate distance function has to be specified) }{The nearest-neighbor-based multivariate two-sample test of \cite{Barakat et al. (1996)}. Modifies the Schilling-Henze nearest neighbor tests (\code{\link{SH}}) such that the number of nearest neighbors does not have to be chosen.}
\item{\code{\link{C2ST}} }{Classifier Two-Sample Test (C2ST) of \cite{Lopez-Paz and Oquab (2017)}. Can be used for multiple samples and categorical data also. Uses the classification accuracy of a classifier that is trained to distinguish between the datasets.}
\item{\code{\link{CCS}} (in case of no ties, appropriate distance function has to be specified) }{Weighted edge-count two-sample test for multivariate data proposed by \cite{Chen, Chen and Su (2018)}. The test is intended for comparing two samples with unequal sample sizes. It is a modification of the graph-based Friedman-Rafsky test \code{\link{FR}}.}
\item{\code{\link{CF}} (in case of no ties, appropriate distance function has to be specified) }{Generalized edge-count two-sample test for multivariate data proposed by \cite{Chen and Friedman (2017)}. The test is intended for better simultaneous detection of shift and scale alternatives. It is a modification of the graph-based Friedman-Rafsky test \code{\link{FR}}.}
\item{\code{\link{FR}} (in case of no ties, appropriate distance function has to be specified) }{The Friedman-Rafsky two-sample test (original edge-count test) for multivariate data (\cite{Friedman and Rafsky, 1979}). Compares two numeric datasets using the number of edges connecting points from different samples in a similarity graph (e.g. MST) on the pooled sample.}
\item{\code{\link{HMN}} }{Random-forest based two-sample test by \cite{Hediger et al. (2021)}. Uses the (OOB) classification error of a random forest that is trained to distinguish between two datasets. Can also be used with categorical data. }
\item{\code{\link{MMCM}} (in case of no ties, appropriate distance function has to be specified) }{Multisample Mahalanobis crossmatch (MMCM) test (\cite{Mukherjee et al., 2022}). Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Rosenbaum}} and to the \code{\link{Petrie}} test.}
\item{\code{\link{Petrie}} (in case of no ties, appropriate distance function has to be specified) }{\cite{Petrie (2016)} multi-sample cross-match test. Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Rosenbaum}} and to the \code{\link{MMCM}} test.}
\item{\code{\link{Rosenbaum}} (in case of no ties, appropriate distance function has to be specified)}{\cite{Rosenbaum (2005)} two-sample cross-match test. Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Petrie}} and to the \code{\link{MMCM}} test.}
\item{\code{\link{SC}} (in case of no ties, appropriate distance function has to be specified) }{Graph-based multi-sample test for high-dimensional data proposed by \cite{Song and Chen (2022)}. Uses the within- and between-sample edge counts in a similarity graph to compare two or more numeric datasets.}
\item{\code{\link{SH}} (in case of no ties, appropriate distance function has to be specified) }{Schilling-Henze nearest neighbor test \cite{(Schilling, 1986; Henze, 1988)}. Uses the number of edges connecting points from different samples in a \eqn{K}-nearest neighbor graph on the pooled sample.}
\item{\code{\link{YMRZL}} }{Tree-based test of \cite{Yu et al. (2007)}. Uses the classification error of a decision tree trained to distinguish between two datasets. Can also be used with categorical data.}
\item{\code{\link{ZC}} (in case of no ties, appropriate distance function has to be specified) }{Max-type edge-count test (\cite{Zhang and Chen, 2019}). Enhancement of the Friedman-Rafsky test (original edge-count test, \code{\link{FR}})  that aims at detecting both location and scale alternatives and is more flexible than the generalized edge-count test of Chen and Friedman (2017) (\code{\link{CF}}).}
}
}

\section{Methods for two datasets with both categorical and numeric variables and target variables}{
\describe{
\item{\code{\link{OTDD}} (appropriate distance function has to be specified) }{Optimal transport dataset distance (OTDD) (\cite{Alvarez-Melis and Fusi, 2020}). The distance combines the distance between features and the distance between label distributions. }
}
}

\section{Methods for more than two datasets with both categorical and numeric variables but without target variables}{
\describe{
\item{\code{\link{C2ST}} }{Classifier Two-Sample Test (C2ST) of \cite{Lopez-Paz and Oquab (2017)}. Can be used for multiple samples and categorical data also. Uses the classification accuracy of a classifier that is trained to distinguish between the datasets.}
\item{\code{\link{MMCM}} (in case of no ties, appropriate distance function has to be specified) }{Multisample Mahalanobis crossmatch (MMCM) test (\cite{Mukherjee et al., 2022}). Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Rosenbaum}} and to the \code{\link{Petrie}} test.}
\item{\code{\link{Petrie}} (in case of no ties, appropriate distance function has to be specified) }{\cite{Petrie (2016)} multi-sample cross-match test. Uses the optimal non-bipartite matching for comparing two or more numeric or categorical samples. In the two-sample case equivalent to the \code{\link{Rosenbaum}} and to the \code{\link{MMCM}} test.}
\item{\code{\link{SC}} (in case of no ties, appropriate distance function has to be specified) }{Graph-based multi-sample test for high-dimensional data proposed by \cite{Song and Chen (2022)}. Uses the within- and between-sample edge counts in a similarity graph to compare two or more numeric datasets.}
}
}

\value{
Typically an object of class \code{htest} with the following components:
\item{statistic }{Observed value of the test statistic}
\item{parameter }{Paramter of the null distribution of the test statistic (where applicable)}
\item{p.value }{Permutation or asymptotic p value (where applicable)}
\item{estimate }{Sample estimate (where applicable)}
\item{alternative }{The alternative hypothesis}
\item{method }{Description of the test}
\item{data.name }{The dataset names}

Further components specific to the method might be included. For details see the help pages of the respective methods.
}

\references{
Stolte, M., Kappenberg, F., Rahnenführer, J., Bommert, A. (2024). Methods for quantifying dataset similarity: a review, taxonomy and comparison. Statist. Surv. 18, 163 - 298. \doi{10.1214/24-SS149} 

}

\seealso{
\code{\link{method.table}}, \code{\link{findSimilarityMethod}}
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