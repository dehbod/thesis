rm(list = ls())
library(rgl)

# Sample data -------------------------------------------------------------

cf <- list()
cf$nSample <- 100
cf$nDimention <- 2
cf$nClass <- 3

# _Univariate Normal ------------------------------------------------------


sampleDataUniNormal <- function(cf) {
  if(!('classIndex' %in% names(cf))) {
    if(!('classRatio' %in% names(cf))) {
      cf$classRatio <- rep(1/cf$nClass, cf$nClass)
    }
    cf$classIndex <- c(0, round( cumsum(cf$classRatio) * cf$nSample))
  }
  
  d <- matrix(rnorm(cf$nSample * cf$nDimention), nrow = cf$nSample)

  dClass <- integer(0)
  for(i in 1:cf$nClass) {
    dClass <- c(dClass, rep(i, cf$classIndex[i+1]- cf$classIndex[i]))
  }

  dClassMean <- matrix(0, nrow = cf$nClass, ncol = cf$nDimention  ,byrow = TRUE)
  theta <- 2 * pi / cf$nClass
  radious <- 10
  for(i in 1:cf$nClass) {
    dClassMean[i, 1] <- radious * cos(theta * i)
    dClassMean[i, 2] <- radious * sin(theta * i)
  }
  # dClassMean[ ,1] <- 10 * seq(0, cf$nClass-1)

  dClassVar <- array(0, dim = c(cf$nClass, cf$nDimention, cf$nDimention))
  dClassVar[,1,1] <- 1
  dClassVar[,2,2] <- 1

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

d <- sampleDataUniNormal(cf)
print(head(data.frame(d = d$data, class = d$class)))
plot(d$data, type = 'p', col = d$class, asp = 1)
# plot3d(d$data, col = d$class, size = 2, type = 's')

# _Multivariate Normal ----------------------------------------------------


# _Alpha Univariate -------------------------------------------------------


# _Alpha Multivariate -----------------------------------------------------


# Parameter estimation ----------------------------------------------------


# Dimantion Reduction -----------------------------------------------------


# Clustering --------------------------------------------------------------


# Goodness ----------------------------------------------------------------


