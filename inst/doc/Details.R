### R code from vignette source 'Details.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
op <- par(no.readonly = TRUE)
old <- options()
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library("knitr")


###################################################
### code chunk number 2: visualization_CM
###################################################
library(nbpMatching)
set.seed(5202)
data.graph.sim <- data.frame(x = runif(16), y = runif(16), 
                             dataset = rep(c(1, 2), each = 8))
set.seed(5202)
data.graph.dis <- data.frame(x = c(rbeta(8, 2, 5), runif(8)), 
                             y = c(rbeta(8, 2, 5), runif(8)), 
                             dataset = rep(c(1, 2), each = 8))

w.sim <- dist(data.graph.sim[, 1:2], upper = TRUE)
w.dis <- dist(data.graph.dis[, 1:2], upper = TRUE)

nbp.sim <- nonbimatch(distancematrix(as.matrix(w.sim)))
nbp.sim <- cbind(nbp.sim$halves[, 2], nbp.sim$halves[, 4])
nbp.dis <- nonbimatch(distancematrix(as.matrix(w.dis)))
nbp.dis <- cbind(nbp.dis$halves[, 2], nbp.dis$halves[, 4])

plotGraph <- function(data, E, directed = FALSE, col = hcl.colors(2, "Spectral")[1]) {
  x <- data$x
  y <- data$y
  ind <- data$dataset
  plot(x, y, pch = c(21, 19)[ind], cex = 2, xlim = 0:1, ylim = 0:1, xlab = "", 
       ylab = "", las = 1, cex.axis = 2)
  if(directed) {
    for(i in 1:nrow(E)) {
      e <- E[i, ]
      d <- dist(matrix(c(x[e[1]], y[e[1]], x[e[2]], y[e[2]]), byrow = TRUE, ncol = 2))
      f <- 0.014 / d
      arrows(x0 = x[e[1]], 
             y0 = y[e[1]], 
             x1 = x[e[1]] + (1 - f) * (x[e[2]] - x[e[1]]), 
             y1 = y[e[1]] + (1 - f) * (y[e[2]] - y[e[1]]), 
             length = 0.07, lwd = 2, 
             col = c("darkgrey", col)[(ind[e[1]] != ind[e[2]]) + 1], 
             lty = c(1, 2)[(ind[E[, 1]] != ind[E[, 2]]) + 1])
    }
  } else {
    segments(x0 = x[E[, 1]], y0 = y[E[, 1]], 
             x1 = x[E[, 2]], y1 = y[E[, 2]], 
             lwd = 3, 
             col = c("darkgrey", col)[(ind[E[, 1]] != ind[E[, 2]]) + 1], 
             lty = c(1, 2)[(ind[E[, 1]] != ind[E[, 2]]) + 1])
  }
  points(x, y, pch = 19, col = c("white", "black")[ind], cex = 2)
  points(x, y, pch = c(21, 19)[ind], cex = 2)
}
par(mar = c(2.1, 3.1, 1.1, 2.1))
plotGraph(data.graph.sim, nbp.sim, FALSE)
par(op)


###################################################
### code chunk number 3: visualization_CM_dis
###################################################
par(mar = c(2.1, 3.1, 1.1, 2.1))
plotGraph(data.graph.dis, nbp.dis, FALSE)
par(op)


###################################################
### code chunk number 4: visualization_tree1
###################################################
library(DataSimilarity)
set.seed(5202)
X1 <- data.frame(X1 = runif(100), X2 = runif(100))
X1$y <- as.factor(ifelse(X1$X1 < 0.5 & X1$X2 > 0.3, 0, 1))

X2 <- data.frame(X1 = runif(100), X2 = runif(100))
X2$y <- as.factor(ifelse((X2$X1 < 0.5 & X2$X2 > 0.3) | (X2$X2 < 0.3 & X2$X1 > 0.2 ), 0, 1))

library(rpart)
library(rpart.plot)
tree1 <- rpart(y ~ ., data = X1)
tree2 <- rpart(y ~ ., data = X2)
parti1 <- DataSimilarity:::findPartition(tree1, X1, X2)
parti2 <- DataSimilarity:::findPartition(tree2, X1, X2)
intersec.parti <- DataSimilarity:::intersectPartitions(parti1, parti2)
par(xpd = TRUE)
prp(tree1, digits = 2, type = 5, tweak = 1.5)
par(op)


###################################################
### code chunk number 5: visualization_tree2
###################################################
par(xpd = TRUE)
prp(tree2, digits = 2, type = 5, tweak = 1.5)
par(op)


###################################################
### code chunk number 6: visualization_parti1
###################################################
plotParti <- function(parti) {
  plot(NA, xlim = 0:1, ylim = 0:1, xlab = "X1", ylab = "X2", main = "", las = 1, 
       cex.axis = 1.5, cex.lab = 1.5)
  for(i in seq_along(parti)){
    segments(x0 = round(parti[[i]][1, 2], 2), x1 = round(parti[[i]][1, 3], 2), 
             y0 = round(parti[[i]][2, 2], 2), y1 = round(parti[[i]][2, 2], 2))
    segments(x0 = round(parti[[i]][1, 2], 2), x1 = round(parti[[i]][1, 3], 2), 
             y0 = round(parti[[i]][2, 3], 2), y1 = round(parti[[i]][2, 3], 2))
    segments(x0 = round(parti[[i]][1, 3], 2), x1 = round(parti[[i]][1, 3], 2), 
             y0 = round(parti[[i]][2, 2], 2), y1 = round(parti[[i]][2, 3], 2))
    segments(x0 = round(parti[[i]][1, 2], 2), x1 = round(parti[[i]][1, 2], 2), 
             y0 = round(parti[[i]][2, 2], 2), y1 = round(parti[[i]][2, 3], 2))
  }  
}

par(mar = c(4.1, 4.1, 1.1, 2.1))
plotParti(parti1)
par(op)


###################################################
### code chunk number 7: visualization_NKT_parti2
###################################################
par(mar = c(4.1, 4.1, 1.1, 2.1))
plotParti(parti2)
par(op)


###################################################
### code chunk number 8: visualization_intersect
###################################################
par(mar = c(4.1, 4.1, 1.1, 2.1))
plotParti(intersec.parti$parti)
par(op)


###################################################
### code chunk number 9: loadpackage
###################################################
library("DataSimilarity")


###################################################
### code chunk number 10: preparedhfr
###################################################
data(dhfr, package = "caret")
act <- dhfr[dhfr$Y == "active", -1]
inact <- dhfr[dhfr$Y == "inactive", -1]


###################################################
### code chunk number 11: exRosenbaum
###################################################
res.Rosenbaum <- Rosenbaum(act, inact, exact = TRUE)


###################################################
### code chunk number 12: exRosenbaum1 (eval = FALSE)
###################################################
## Rosenbaum(act, inact, exact = TRUE)


###################################################
### code chunk number 13: exRosenbaum2
###################################################
print(res.Rosenbaum)


###################################################
### code chunk number 14: exRosenbaumDS1 (eval = FALSE)
###################################################
## DataSimilarity(act, inact, method = "Rosenbaum", exact = TRUE)


###################################################
### code chunk number 15: exRosenbaumDS2
###################################################
print(res.Rosenbaum)


###################################################
### code chunk number 16: prepareiris
###################################################
data("iris")
setosa <- iris[iris$Species == "setosa", -5]
versicolor <- iris[iris$Species == "versicolor", -5]
virginica <- iris[iris$Species == "virginica", -5]


###################################################
### code chunk number 17: exMMCM
###################################################
MMCM(setosa, versicolor, virginica)


###################################################
### code chunk number 18: exMMCMDS
###################################################
DataSimilarity(setosa, versicolor, virginica, method = "MMCM")


###################################################
### code chunk number 19: preparesegmentationData
###################################################
data(segmentationData, package = "caret")
test <- segmentationData[segmentationData$Case == "Test", -(1:2)]
train <- segmentationData[segmentationData$Case == "Train", -(1:2)]


###################################################
### code chunk number 20: exNKT
###################################################
NKT(train, test, target1 = "Class", target2 = "Class", tune = FALSE)
NKT(train, test, target1 = "Class", target2 = "Class", tune = FALSE, 
    version = 2)
NKT(train, test, target1 = "Class", target2 = "Class", tune = FALSE,
    version = 3)


###################################################
### code chunk number 21: exNKTDS
###################################################
DataSimilarity(train, test, method = "NKT", target1 = "Class",
               target2 = "Class",  tune = FALSE)
DataSimilarity(train, test, method = "NKT", target1 = "Class", 
               target2 = "Class", tune = FALSE, version = 2)
DataSimilarity(train, test, method = "NKT", target1 = "Class",
               target2 = "Class", tune = FALSE, version = 3)


###################################################
### code chunk number 22: preparebanque1
###################################################
data(banque , package = "ade4")
card <- banque[banque$cableue == "oui", -7]
no.card <- banque[banque$cableue == "non", -7]


###################################################
### code chunk number 23: exHMN
###################################################
# HMN.res <- HMN(card, no.card, n.perm = 1000, statistic = "OverallOOB") 
# save(HMN.res, file = "tmpResHMNVignette.RData")
load("tmpResHMNVignette.RData")


###################################################
### code chunk number 24: exHMN1 (eval = FALSE)
###################################################
## HMN(card, no.card, n.perm = 1000, statistic = "OverallOOB") 


###################################################
### code chunk number 25: exHMN2
###################################################
print(HMN.res)


###################################################
### code chunk number 26: exHMNDS1 (eval = FALSE)
###################################################
## DataSimilarity(card, no.card, method = "HMN", n.perm = 1000, 
##                statistic = "OverallOOB") 


###################################################
### code chunk number 27: exHMNDS2
###################################################
print(HMN.res)


###################################################
### code chunk number 28: preparebanque2
###################################################
data(banque, package = "ade4")
agric <- banque[banque$csp == "agric", -1]
artis <- banque[banque$csp == "artis", -1]
cadsu <- banque[banque$csp == "cadsu", -1]
inter <- banque[banque$csp == "inter", -1]
emplo <- banque[banque$csp == "emplo", -1]
ouvri <- banque[banque$csp == "ouvri", -1]
retra <- banque[banque$csp == "retra", -1]
inact <- banque[banque$csp == "inact", -1]
etudi <- banque[banque$csp == "etudi", -1]


###################################################
### code chunk number 29: exC2STKNN
###################################################
C2ST.res <- C2ST(agric, artis, cadsu, inter, emplo, ouvri, retra, inact, etudi)


###################################################
### code chunk number 30: exC2STKNN1 (eval = FALSE)
###################################################
## C2ST(agric, artis, cadsu, inter, emplo, ouvri, retra, inact, etudi)


###################################################
### code chunk number 31: exC2STKNN2
###################################################
print(C2ST.res)


###################################################
### code chunk number 32: exC2STKNNDS1
###################################################
DataSimilarity(agric, artis, cadsu, inter, emplo, ouvri, retra, inact, 
               etudi, method = "C2ST")


###################################################
### code chunk number 33: exC2STMLP
###################################################
C2ST(agric, artis, cadsu, inter, emplo, ouvri, retra, inact, etudi, 
     classifier = "nnet", train.args = list(trace = FALSE))


###################################################
### code chunk number 34: exC2STMLPDS
###################################################
DataSimilarity(agric, artis, cadsu, inter, emplo, ouvri, retra, inact, 
               etudi, method = "C2ST", classifier = "nnet", 
               train.args = list(trace = FALSE))


###################################################
### code chunk number 35: exOTDD
###################################################
# res.OTDD1 <- OTDD(artis, cadsu, target1 = "eparliv", target2 = "eparliv", 
#                   feature.cost = hammingDist) 
# res.OTDD2 <- OTDD(artis, ouvri, target1 = "eparliv", target2 = "eparliv", 
#                   feature.cost = hammingDist) 
# save(res.OTDD1, res.OTDD2, file = "tmpResOTDDVignette.RData")
load("tmpResOTDDVignette.RData")


###################################################
### code chunk number 36: exOTDD1 (eval = FALSE)
###################################################
## OTDD(artis, cadsu, target1 = "eparliv", target2 = "eparliv", 
##      feature.cost = hammingDist) 


###################################################
### code chunk number 37: exOTDD2
###################################################
print(res.OTDD1)


###################################################
### code chunk number 38: exOTDDDS1 (eval = FALSE)
###################################################
## DataSimilarity(artis, cadsu, method = "OTDD", target1 = "eparliv", 
##                target2 = "eparliv", feature.cost = hammingDist) 


###################################################
### code chunk number 39: exOTDDDS2
###################################################
print(res.OTDD1)


###################################################
### code chunk number 40: exOTDD3 (eval = FALSE)
###################################################
## OTDD(artis, ouvri, target1 = "eparliv", target2 = "eparliv", 
##      feature.cost = hammingDist) 


###################################################
### code chunk number 41: exOTDD4
###################################################
print(res.OTDD2)


###################################################
### code chunk number 42: exOTDDDS3 (eval = FALSE)
###################################################
## DataSimilarity(artis, ouvri, method = "OTDD", target1 = "eparliv", 
##                target2 = "eparliv", feature.cost = hammingDist) 


###################################################
### code chunk number 43: exOTDDDS4
###################################################
print(res.OTDD2)


###################################################
### code chunk number 44: methods
###################################################
names(caret::getModelInfo())[sapply(caret::getModelInfo(), function(x) {
  "Classification" %in% x$type
})]


###################################################
### code chunk number 45: reset
###################################################
options(old)
par(op)


