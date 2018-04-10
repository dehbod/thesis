dataConfiguration <- function(cf, multiVariate = FALSE) {
  
  if(!('classIndex' %in% names(cf))) {
    if(!('classRatio' %in% names(cf))) {
      cf$classRatio <- rep(1/cf$nClass, cf$nClass)
    }
    cf$classIndex <- c(0, round( cumsum(cf$classRatio) * cf$nSample))
  }
  
  # dClass <- rep(0, cf$nSample)
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
  if(multiVariate) {
    for(i in 1:cf$nClass) {
      correlation <- runif(cf$nDimention^2, min = -0.5, max = 0.5)
      correlation[as.vector(diag(cf$nDimention)) == 1] <- 0.5
      correlation <- matrix(correlation, nrow = 2)
      correlation <- correlation + t(correlation)
      sdev <- rep(1, cf$nDimention)
      msdev <- diag(cf$nDimention) * sdev
      dClassVar[i, , ] <- msdev %*% correlation %*% msdev
    }
  } else {
    dClassVar[,1,1] <- 1
    dClassVar[,2,2] <- 1
  }
  
  return(list(
    cf = cf,
    dClass = dClass,
    dClassMean = dClassMean,
    dClassVar = dClassVar
  ))
  
}
