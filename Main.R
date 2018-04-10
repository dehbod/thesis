rm(list = ls())
library(rgl)
library(MASS)
source('Util.R')

# Sample data -------------------------------------------------------------

cf <- list()
cf$nSample <- 100
cf$nDimention <- 2
cf$nClass <- 3

# _Univariate Normal ------------------------------------------------------

sampleDataUniNormal <- function(cf) {

  out <- dataConfiguration(cf)
  
  cf <- out$cf
  dClass <- out$dClass
  dClassMean <- out$dClassMean
  dClassVar <- out$dClassVar
  
  d <- matrix(rnorm(cf$nSample * cf$nDimention), nrow = cf$nSample)

  for(i in 1:cf$nClass) {
    for(j in 1:cf$nDimention) {
      d[(cf$classIndex[i]+1):cf$classIndex[i+1], j] <-
        sqrt(dClassVar[i, j, j]) * d[(cf$classIndex[i]+1):cf$classIndex[i+1], j]
    }
    d[(cf$classIndex[i]+1):cf$classIndex[i+1], ] <- 
      d[(cf$classIndex[i]+1):cf$classIndex[i+1], ] +
      matrix(rep(dClassMean[i,], cf$classIndex[i+1] - cf$classIndex[i]), 
             ncol = cf$nDimention,
             byrow = TRUE)
  }

  permuteIndex <- sample.int(cf$nSample, size = cf$nSample)
  d <- d[permuteIndex, ]
  dClass <- dClass[permuteIndex]

  return(list(data = d, class = dClass))
  
}

# d <- sampleDataUniNormal(cf)
# print(head(data.frame(d = d$data, class = d$class)))
# plot(d$data, type = 'p', col = d$class, asp = 1)
# plot3d(d$data, col = d$class, size = 2, type = 's')

# _Multivariate Normal ----------------------------------------------------

out <- dataConfiguration(cf, multiVariate = TRUE)

i <- 1

d <- mvrnorm(1000, mu = out$dClassMean[i,], Sigma = out$dClassVar[i, , ])

plot(d, asp = 1)



# _Alpha Univariate -------------------------------------------------------


# _Alpha Multivariate -----------------------------------------------------


# Parameter estimation ----------------------------------------------------


# Dimantion Reduction -----------------------------------------------------


# Clustering --------------------------------------------------------------


# Goodness ----------------------------------------------------------------


