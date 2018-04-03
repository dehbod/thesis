# Sample data -------------------------------------------------------------

cf <- list()
cf$nSample <- 1000
cf$nDimention <- 2
cf$nClass <- 3
cf$classRatio <- rep(1/cf$nClass, cf$nClass)


# _Univariate Normal ------------------------------------------------------

d <- matrix(rnorm(cf$nSample * cf$nDimention), nrow = cf$nSample)

dClass <- rep(0, cf$nSample)
for(i in 1:cf$nClass) {
  sumRatio <- sum(cf$classRatio[i:cf$nClass])
  base <- which(dClass == 0)
  index <- sample(
    base,
    size = round(length(base) * cf$classRatio[i] / sumRatio )
  )
  dClass[index] <- i
}

dClassMean <- matrix(0, nrow = cf$nClass, ncol = cf$nDimention  ,byrow = TRUE)
theta <- 2 * pi / cf$nClass
radious <- 10
for(i in 1:cf$nClass) {
  dClassMean[i, 1] <- radious * cos(theta * i)
  dClassMean[i, 2] <- radious * sin(theta * i)
}
rm('theta', 'radious')
# dClassMean[ ,1] <- 10 * seq(0, cf$nClass-1)

dClassVar <- array(0, dim = c(cf$nClass, cf$nDimention, cf$nDimention))
dClassVar[,1,1] <- 1
dClassVar[,2,2] <- 1

for(i in 1:cf$nClass) {
  for(j in 1:cf$nDimention) {
    d[dClass == i, j] <- sqrt(dClassVar[i, j, j]) * d[dClass == i, j]
  }
  d[dClass == i, ] <- d[dClass == i, ] + matrix(rep(dClassMean[i,], sum(dClass == i)), ncol = 2, byrow = TRUE)
}

print(head(data.frame(d = d, class = dClass)))
plot(d, type = 'p', col = dClass)

# _Multivariate Normal ----------------------------------------------------


# _Alpha Univariate -------------------------------------------------------


# _Alpha Multivariate -----------------------------------------------------



# Parameter estimation ----------------------------------------------------


# Dimantion Reduction -----------------------------------------------------


# Clustering --------------------------------------------------------------


# Goodness ----------------------------------------------------------------


