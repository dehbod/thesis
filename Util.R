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
      dClassVar[i, , ] <- randCovMat(cf$nDimention)
    }
  } else {
    for(i in 1:cf$nClass) {
      dClassVar[i, , ] <- diag(cf$nDimention)
    }
  }
  
  return(list(
    cf = cf,
    dClass = dClass,
    dClassMean = dClassMean,
    dClassVar = dClassVar
  ))
  
}

randCovMat <- function(nd, sdev = NULL) {

  if (is.null(sdev)) {
    sdev <- rep(1, nd)
  }
  out <- diag(nd)
  index <- data.frame(n = 1:((nd*(nd-1)) / 2))
  index$i <- ceiling((-1+sqrt(1+8*index$n)) / 2)
  index$j <- index$n - ((index$i - 1)^2 + (index$i - 1)) / 2
  index$i <- index$i + 1
  index <- index[sample(index$n, nrow(index)), ]
  
  for(i in 1:nrow(index)) {
    candidate <- seq(from = -1, to = 1, by = 0.01)
    candidate <- sample(candidate, size = length(candidate))
    for(k in 1:length(candidate)) {
      out[index$i[i],index$j[i]] <- candidate[k]
      out[index$j[i],index$i[i]] <- out[index$i[i],index$j[i]]
      if(det(out) > 0) break
    } 
  }
  msdev <- diag(nd) * sdev
  out <- msdev %*% out %*% msdev
}
