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

# d <- sampleDataUniNormal(cf)
# d <- sampleDataMultiNormal(cf)
d <- sampleDataUniStable(cf, 1.8)
# d <- sampleDataMultiStable(cf, 1.8)

d$data <- randomProjection(d$data, cf$tDimention, alpha = 1.8)
plot(d$data, type = 'p', col = d$class, asp = 1)
cl <- kmeans(d$data, cf$nClass, nstart = 10)
points(d$data, pch = 3, col = cl$cluster)

# Index ----------------------------------------------------------------

print(adjustedRandIndex(d$class, cl$cluster))

