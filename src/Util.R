# Sample Data Generation Functions ----------------------------------------

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

if(FALSE) {
  d <- sampleDataUniNormal(cf)
  print(head(data.frame(d = d$data, class = d$class)))
  plot(d$data, type = 'p', col = d$class, asp = 1)
  plot3d(d$data, col = d$class, size = 2, type = 's')
  aspect3d(x = 'iso')
}

# _Multivariate Normal ----------------------------------------------------

sampleDataMultiNormal <- function(cf) {
  out <- dataConfiguration(cf, multiVariate = TRUE)
  
  cf <- out$cf
  dClass <- out$dClass
  dClassMean <- out$dClassMean
  dClassVar <- out$dClassVar
  
  d <- matrix(0, nrow = 0, ncol = cf$nDimention)
  
  for(i in 1:cf$nClass) {
    d <- rbind(d, mvrnorm(cf$classIndex[i+1] - cf$classIndex[i],
                          mu = out$dClassMean[i,], Sigma = out$dClassVar[i, , ])
    )
  }
  
  permuteIndex <- sample.int(cf$nSample, size = cf$nSample)
  d <- d[permuteIndex, ]
  dClass <- dClass[permuteIndex]
  
  return(list(data = d, class = dClass))
  
}

if(FALSE) {
  d <- sampleDataMultiNormal(cf)
  print(head(data.frame(d = d$data, class = d$class)))
  plot(d$data, type = 'p', col = d$class, asp = 1)
  plot3d(d$data, col = d$class, size = 2, type = 's')
  aspect3d(x = 'iso')
}

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

if(FALSE) {
  d <- sampleDataUniStable(cf, 1.8)
  print(head(data.frame(d = d$data, class = d$class)))
  plot(d$data, type = 'p', col = d$class, asp = 1)
  plot3d(d$data, col = d$class, size = 2, type = 's')
  aspect3d(x = 'iso')
}

# _Alpha Multivariate -----------------------------------------------------

sampleDataMultiStable <- function(cf, alpha) {
  
  out <- dataConfiguration(cf)
  
  cf <- out$cf
  dClass <- out$dClass
  dClassMean <- out$dClassMean
  dClassVar <- out$dClassVar
  
  cf$nDimention <- 2
  
  theta <- c(0, pi/4, pi/2, 3 * pi / 4)
  weight <- c(1,2,1,1)
  
  d <- 
    matrix(
      rstable(cf$nSample * length(theta), alpha = alpha, beta = 0),
      ncol = length(theta)
    ) %*%
    matrix(
      c(
        (cos(theta) * weight) / sum(abs(cos(theta) * weight)),
        (sin(theta) * weight) / sum(abs(sin(theta) * weight))
      ),
      ncol = cf$nDimention
    )
  
  for(i in 1:cf$nClass) {
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

if(FALSE) {
  d <- sampleDataMultiStable(cf, 1.8)
  print(head(data.frame(d = d$data, class = d$class)))
  plot(d$data, type = 'p', col = d$class, asp = 1)
  plot3d(d$data, col = d$class, size = 2, type = 's')
  aspect3d(x = 'iso')
}




# Dimention Reduction -----------------------------------------------------

randomProjection <- function(d, targetNumDimention, alpha = 2) {
  
  pm <- matrix(rstable(ncol(d) * targetNumDimention, alpha = alpha, beta = 0),
               nrow = ncol(d))
  
  d %*% pm
  
}

sRandomProjection <- function(d, targetNumDimention, s = 2) {
  
  r <- runif(ncol(d) * targetNumDimention, min = 0, max = 2*s)
  r[r <= 1] = -1
  r[r >= (2*s-1)] = 1
  r[r != 1 & r != -1] <- 0
  pm <- matrix(r,
               nrow = ncol(d))
  
  d %*% pm
  
}



# Util Functions ----------------------------------------------------------

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
  radious <- 2
  if(cf$nClass != 2) {
    for(i in 1:cf$nClass) {
      dClassMean[i, 1] <- radious * cos(theta * i)
      dClassMean[i, 2] <- radious * sin(theta * i)
    }
  } else {
    w <- runif(cf$nDimention,min = 0, max = 1)
    w <- w / sqrt(sum(w ^ 2)) * radious
    dClassMean[1, ] <- w
    dClassMean[2, ] <- -w
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

normalizeData <- function(d) {
  d <- apply(d, 2, function(x) {(x-mean(x))/sd(x)})
  d
}

ARIreport <- function(d, tDimention = 2, alpha = 2) {
  
  nClass <- length(unique(d$class))
  
  cl <- kmeans(d$data, nClass, nstart = 20, iter.max = 100)
  ari_d <- (adjustedRandIndex(d$class, cl$cluster))
  
  d$pdata <- randomProjection(d$data, tDimention, alpha = alpha)
  clp <- kmeans(d$pdata, nClass, nstart = 20, iter.max = 100)
  ari_p <- (adjustedRandIndex(d$class, clp$cluster))
  
  c_e <- 100 * (ari_d - ari_p)
  
  data.frame(ari_d = ari_d, ari_p = ari_p, c_e = c_e)
}

ARIreport_s <- function(d, tDimention = 2, s = 2) {
  
  nClass <- length(unique(d$class))
  
  cl <- kmeans(d$data, nClass, nstart = 20, iter.max = 100)
  ari_d <- (adjustedRandIndex(d$class, cl$cluster))
  
  check <- TRUE
  while(check){
    check <- tryCatch({
      d$pdata <- sRandomProjection(d$data, tDimention, s = s)
      clp <- kmeans(d$pdata, nClass, nstart = 20, iter.max = 100)
      FALSE
      }, error = function(e) {print('not enough distinct points'); TRUE})
  }
  
  ari_p <- (adjustedRandIndex(d$class, clp$cluster))
  
  c_e <- 100 * (ari_d - ari_p)
  
  data.frame(ari_d = ari_d, ari_p = ari_p, c_e = c_e)
}

