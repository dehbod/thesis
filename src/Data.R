rm(list = ls(all.names = TRUE))
library(beepr)

library(MASS)
library(mclust)
library(xlsx)

if(file.exists('data.RData')) {
  load(file = 'data.RData')
} else {

crabs <- crabs #MASS
data("diabetes") #mclust
data("thyroid") #mclust
data("banknote") #mclust

irisUci <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data', header = FALSE)
names(irisUci) <- names(iris)
seeds <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt', header = FALSE)

tmpFile <- tempfile(fileext = '.xls')
download.file('https://archive.ics.uci.edu/ml/machine-learning-databases/00342/Data_Cortex_Nuclear.xls', destfile = tmpFile)
MPE <- read.xlsx(tmpFile, sheetIndex = 1)

rm(tmpFile)

save(list = ls(), file = 'data.RData')

}

beep(4)
