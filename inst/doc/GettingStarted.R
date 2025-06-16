### R code from vignette source 'GettingStarted.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
op <- par(no.readonly = TRUE)
old <- options()
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library("knitr")


###################################################
### code chunk number 2: loadpackage
###################################################
library("DataSimilarity")


###################################################
### code chunk number 3: preparedhfr
###################################################
data(dhfr, package = "caret")
act <- dhfr[dhfr$Y == "active", -1]
inact <- dhfr[dhfr$Y == "inactive", -1]


###################################################
### code chunk number 4: findMethod2Num
###################################################
findSimilarityMethod(Numeric = TRUE)


###################################################
### code chunk number 5: findMethod2NumAllInfo
###################################################
findSimilarityMethod(Numeric = TRUE, only.names = FALSE)


###################################################
### code chunk number 6: exRosenbaumDS
###################################################
DataSimilarity(act, inact, method = "Rosenbaum", exact = TRUE)


###################################################
### code chunk number 7: exRosenbaum
###################################################
res.Rosenbaum <- Rosenbaum(act, inact, exact = TRUE)


###################################################
### code chunk number 8: exRosenbaum1 (eval = FALSE)
###################################################
## Rosenbaum(act, inact, exact = TRUE)


###################################################
### code chunk number 9: exRosenbaum2
###################################################
print(res.Rosenbaum)


###################################################
### code chunk number 10: exRosenbaum3
###################################################
res.Rosenbaum <- Rosenbaum(act, inact, exact = TRUE)
res.Rosenbaum$statistic


###################################################
### code chunk number 11: exRosenbaum4
###################################################
res.Rosenbaum$p.value


###################################################
### code chunk number 12: exRosenbaum5
###################################################
res.Rosenbaum$estimate


###################################################
### code chunk number 13: prepareiris
###################################################
data("iris")
setosa <- iris[iris$Species == "setosa", -5]
versicolor <- iris[iris$Species == "versicolor", -5]
virginica <- iris[iris$Species == "virginica", -5]


###################################################
### code chunk number 14: findMethod3Num
###################################################
findSimilarityMethod(Numeric = TRUE, Multiple.Samples = TRUE)


###################################################
### code chunk number 15: exMMCM
###################################################
DataSimilarity(setosa, versicolor, virginica, method = "MMCM")
MMCM(setosa, versicolor, virginica)


###################################################
### code chunk number 16: preparesegmentationData
###################################################
data(segmentationData, package = "caret")
test <- segmentationData[segmentationData$Case == "Test", -(1:2)]
train <- segmentationData[segmentationData$Case == "Train", -(1:2)]


###################################################
### code chunk number 17: findMethodNumY
###################################################
findSimilarityMethod(Numeric = TRUE, Target.Inclusion = TRUE)


###################################################
### code chunk number 18: exNKT
###################################################
DataSimilarity(train, test, method = "NKT", target1 = "Class", 
               target2 = "Class", tune = FALSE)
NKT(train, test, target1 = "Class", target2 = "Class", tune = FALSE)
DataSimilarity(train, test, method = "NKT", target1 = "Class",
               target2 = "Class", tune = FALSE, version = 2)
NKT(train, test, target1 = "Class", target2 = "Class", tune = FALSE, 
    version = 2)
DataSimilarity(train, test, method = "NKT", target1 = "Class", 
               target2 = "Class", tune = FALSE, version = 3)
NKT(train, test, target1 = "Class", target2 = "Class", tune = FALSE,
    version = 3)


###################################################
### code chunk number 19: preparebanque1
###################################################
data(banque , package = "ade4")
card <- banque[banque$cableue == "oui", -7]
no.card <- banque[banque$cableue == "non", -7]


###################################################
### code chunk number 20: findMethod2Cat
###################################################
findSimilarityMethod(Categorical = TRUE)


###################################################
### code chunk number 21: exHMN
###################################################
# HMN.res <- HMN(card, no.card, n.perm = 1000, statistic = "OverallOOB") 
# save(HMN.res, file = "tmpResHMNVignette.RData")
load("tmpResHMNVignette.RData")


###################################################
### code chunk number 22: exHMNDS (eval = FALSE)
###################################################
## set.seed(1234)
## DataSimilarity(card, no.card, method = "HMN", n.perm = 1000,
##                statistic = "OverallOOB") 


###################################################
### code chunk number 23: exHMNDS2
###################################################
print(HMN.res)


###################################################
### code chunk number 24: exHMN1 (eval = FALSE)
###################################################
## set.seed(1234)
## HMN(card, no.card, n.perm = 1000, statistic = "OverallOOB") 


###################################################
### code chunk number 25: exHMN2
###################################################
print(HMN.res)


###################################################
### code chunk number 26: preparebanque2
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
### code chunk number 27: findMethod7Cat
###################################################
findSimilarityMethod(Categorical = TRUE, Multiple.Samples = TRUE)


###################################################
### code chunk number 28: exC2STKNN
###################################################
C2ST.res <- C2ST(agric, artis, cadsu, inter, emplo, ouvri, retra, inact, etudi)


###################################################
### code chunk number 29: exC2STKNNDS1 (eval = FALSE)
###################################################
## DataSimilarity(agric, artis, cadsu, inter, emplo, ouvri, retra, inact,
##                etudi, method = "C2ST")


###################################################
### code chunk number 30: exC2STKNNDS2
###################################################
print(C2ST.res)


###################################################
### code chunk number 31: exC2STKNN1 (eval = FALSE)
###################################################
## C2ST(agric, artis, cadsu, inter, emplo, ouvri, retra, inact, etudi)


###################################################
### code chunk number 32: exC2STKNN2
###################################################
print(C2ST.res)


###################################################
### code chunk number 33: exC2STMLP
###################################################
DataSimilarity(agric, artis, cadsu, inter, emplo, ouvri, retra, inact,
               etudi, method = "C2ST", classifier = "nnet",
               train.args = list(trace = FALSE))
C2ST(agric, artis, cadsu, inter, emplo, ouvri, retra, inact, etudi, 
     classifier = "nnet", train.args = list(trace = FALSE))


###################################################
### code chunk number 34: findMethodCatY
###################################################
findSimilarityMethod(Categorical = TRUE, Target.Inclusion = TRUE)


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
### code chunk number 36: exOTDDDS1 (eval = FALSE)
###################################################
## DataSimilarity(artis, cadsu, method = "OTDD", target1 = "eparliv", 
##                target2 = "eparliv", feature.cost = hammingDist) 


###################################################
### code chunk number 37: exOTDDDS2
###################################################
print(res.OTDD1)


###################################################
### code chunk number 38: exOTDD1 (eval = FALSE)
###################################################
## OTDD(artis, cadsu, target1 = "eparliv", target2 = "eparliv", 
##      feature.cost = hammingDist) 


###################################################
### code chunk number 39: exOTDD2
###################################################
print(res.OTDD1)


###################################################
### code chunk number 40: exOTDDDS3 (eval = FALSE)
###################################################
## DataSimilarity(artis, ouvri, method = "OTDD", target1 = "eparliv", 
##                target2 = "eparliv", feature.cost = hammingDist) 


###################################################
### code chunk number 41: exOTDDDS4
###################################################
print(res.OTDD2)


###################################################
### code chunk number 42: exOTDD3 (eval = FALSE)
###################################################
## OTDD(artis, ouvri, target1 = "eparliv", target2 = "eparliv", 
##      feature.cost = hammingDist) 


###################################################
### code chunk number 43: exOTDD4
###################################################
print(res.OTDD2)


###################################################
### code chunk number 44: reset
###################################################
options(old)
par(op)


