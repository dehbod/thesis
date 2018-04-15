rm(list = ls())
library(rgl)
library(MASS)
library(stabledist)
source('Util.R')

# Sample data -------------------------------------------------------------

cf <- list()
cf$nSample <- 1000
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

sampleDataMultiNormal <- function(cf) {
  out <- dataConfiguration(cf, multiVariate = TRUE)
  
  cf <- out$cf
  dClass <- out$dClass
  dClassMean <- out$dClassMean
  dClassVar <- out$dClassVar
  
  d <- NULL
  
  for(i in 1:cf$nClass) {
    if(is.null(d)) {
      d <- mvrnorm(cf$classIndex[i+1] - cf$classIndex[i],
                   mu = out$dClassMean[i,], Sigma = out$dClassVar[i, , ])
    } else {
      d <- rbind(d, mvrnorm(cf$classIndex[i+1] - cf$classIndex[i],
                            mu = out$dClassMean[i,], Sigma = out$dClassVar[i, , ])
      )
    }
  }
  
  
  permuteIndex <- sample.int(cf$nSample, size = cf$nSample)
  d <- d[permuteIndex, ]
  dClass <- dClass[permuteIndex]
  
  return(list(data = d, class = dClass))
  
}

# d <- sampleDataMultiNormal(cf)
# print(head(data.frame(d = d$data, class = d$class)))
# plot(d$data, type = 'p', col = d$class, asp = 1)
# plot3d(d$data, col = d$class, size = 2, type = 's')


# _Alpha Univariate -------------------------------------------------------

sampleDataUniStable <- function(cf, alpha) {
  
  out <- dataConfiguration(cf)
  
  cf <- out$cf
  dClass <- out$dClass
  dClassMean <- out$dClassMean
  dClassVar <- out$dClassVar
  
  d <- matrix(rstable(cf$nSample * cf$nDimention, alpha = alpha, beta = 0),
              nrow = cf$nSample)
  
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

# d <- sampleDataUniStable(cf, 1.8)
# print(head(data.frame(d = d$data, class = d$class)))
# plot(d$data, type = 'p', col = d$class, asp = 1)
# plot3d(d$data, col = d$class, size = 2, type = 's')

# _Alpha Multivariate -----------------------------------------------------


# Parameter estimation ----------------------------------------------------


# Dimantion Reduction -----------------------------------------------------


# Clustering --------------------------------------------------------------


# Goodness ----------------------------------------------------------------


