rm(list = ls())
library(rgl)
library(MASS)
library(stabledist)
library(mclust)
source('Util.R')

# Sample data -------------------------------------------------------------

set.seed(10340)
cf <- list()
cf$nSample <- 1000
cf$nDimention <- 20
cf$nClass <- 3
cf$tDimention <- 2

# Parameter estimation ----------------------------------------------------
# like Hill estimator for alpha

# Clustering --------------------------------------------------------------

d <- sampleDataUniNormal(cf)
# d <- sampleDataMultiNormal(cf)
# d <- sampleDataUniStable(cf, 1.8)
# d <- sampleDataMultiStable(cf, 1.8)

cl <- kmeans(d$data, cf$nClass, nstart = 10)
d$pdata <- randomProjection(d$data, cf$tDimention, alpha = 2)
clp <- kmeans(d$pdata, cf$nClass, nstart = 10)

plot(d$pdata, type = 'p', col = d$class, asp = 1)
points(d$pdata, pch = 3, col = cl$cluster)

# Index ----------------------------------------------------------------

ari_d <- (adjustedRandIndex(d$class, cl$cluster))
ari_p <- (adjustedRandIndex(d$class, clp$cluster))
c_p <- 100 * (ari_d - ari_p)
print(c(ari_d,ari_p, c_p))

