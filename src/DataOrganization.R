library(tictoc)
source('Util.R')
d <- list()
apply(thyroid, 2, function(x) {sum(is.na(x) | is.null(x))})
d$thyroid$data <- thyroid[,2:6]
d$thyroid$class <- as.numeric(thyroid$Diagnosis)
d$thyroid$data <- normalizeData(d$thyroid$data)
d$thyroid$name <- 'Thyroid'

apply(irisUci, 2, function(x) {sum(is.na(x) | is.null(x))})
d$iris$data <- irisUci[,1:4]
d$iris$class <- as.numeric(irisUci$Species)
d$iris$data <- normalizeData(d$iris$data)
d$iris$name <- 'Iris'

apply(diabetes, 2, function(x) {sum(is.na(x) | is.null(x))})
d$diabetes$data <- diabetes[,2:4]
d$diabetes$class <- as.numeric(diabetes$class)
d$diabetes$data <- normalizeData(d$diabetes$data)
d$diabetes$name <- 'Diabetes'

apply(banknote, 2, function(x) {sum(is.na(x) | is.null(x))})
d$banknote$data <- banknote[,2:7]
d$banknote$class <- as.numeric(banknote$Status)
d$banknote$data <- normalizeData(d$banknote$data)
d$banknote$name <- 'Swiss Banknotes'

apply(seeds, 2, function(x) {sum(is.na(x) | is.null(x))})
d$seeds$data <- seeds[,1:7]
d$seeds$class <- as.numeric(seeds[,8])
d$seeds$data <- normalizeData(d$seeds$data)
d$seeds$name <- 'Seeds'

apply(MPE, 2, function(x) {sum(is.na(x) | is.null(x))})
MPEwoNA <- MPE
MPEwoNA[,2:78] <- apply(MPE[,2:78], 2, function(x) { x[is.na(x)] <- mean(x, na.rm = TRUE); x } ) 
apply(MPEwoNA, 2, function(x) {sum(is.na(x) | is.null(x))})
d$MPEwoNA$data <- MPEwoNA[,2:78]
d$MPEwoNA$class <- as.numeric(MPEwoNA$class)
d$MPEwoNA$data <- normalizeData(d$MPEwoNA$data)
d$MPEwoNA$name <- 'Mice Protein Expression'

apply(crabs, 2, function(x) {sum(is.na(x) | is.null(x))})
d$crabs$data <- crabs[,4:8]
d$crabs$data <- cbind(d$crabs$data, as.numeric(crabs[,2]))
d$crabs$class <- as.numeric(crabs[,1])
d$crabs$data <- normalizeData(d$crabs$data)
d$crabs$name <- 'Crabs'

iterations <- 1000
tic()
for(dIndex in names(d)) {
  out <- data.frame(ari_d = numeric(0), ari_p = numeric(0), c_e = numeric(0))
  for(i in 1:iterations) {
    out <- rbind(out , ARIreport(d[[dIndex]], tDimention = 2, alpha = 1))
  }
  d[[dIndex]]$ARIreport <- out
}
toc()

save(file = 'SimData_A1D2_1io.RData', list = c('d'))

beep(4)

# report <- data.frame()
# 
# report <- rbind(report, data.frame(dataset = 'Thyroid',ARIreport(d$thyroid), stringsAsFactors = FALSE))
# report <- rbind(report, c(dataset = 'Iris',ARIreport(d$iris)))
# report <- rbind(report, c(dataset = 'Diabetes',ARIreport(d$diabetes)))
# report <- rbind(report, c(dataset = 'Swiss banknotes',ARIreport(d$banknote)))
# report <- rbind(report, c(dataset = 'Seeds',ARIreport(d$seeds)))
# report <- rbind(report, c(dataset = 'MPE',ARIreport(d$MPEwoNA)))
# report <- rbind(report, c(dataset = 'Crabs',ARIreport(d$crabs)))
# 
# names(report) <- c('Dataset', 'ARI_d','ARI_p','C_e')
# print(report)