rm(list = ls())
library(tictoc)
library(beepr)
source('Util.R')
d <- list()

cf <- list()
cf$nSample <- 800
cf$nDimention <- 200
cf$nClass <- 2
d$Sim200 <- sampleDataUniNormal(cf)
d$Sim200$name <- 'Sim200'

cf <- list()
cf$nSample <- 800
cf$nDimention <- 1000
cf$nClass <- 2
d$Sim1000 <- sampleDataUniNormal(cf)
d$Sim1000$name <- 'Sim1000'

for(alpha in c(1,2)) {
  for(tDimention in c(2,3, 5, 10, 20, 30, 50)) {
    
    iterations <- 1000
    tic()
    for(dIndex in names(d)) {
      out <- data.frame(ari_d = numeric(0), ari_p = numeric(0), c_e = numeric(0))
      for(i in 1:iterations) {
        out <- rbind(out , ARIreport(d[[dIndex]], tDimention = tDimention, alpha = alpha))
      }
      d[[dIndex]]$ARIreport <- out
      print(dIndex)
    }
    toc()
    save(file = paste0('SimData_A',alpha,'D',tDimention,'_Sim_',gsub('( )|(-)|(:)', '', Sys.time()),'.RData'), list = c('d'))
    
  }
}

for(s in c(2)) {
  for(tDimention in c(2,3, 5, 10, 20, 30, 50)) {
    
    iterations <- 1000
    tic()
    for(dIndex in names(d)) {
      out <- data.frame(ari_d = numeric(0), ari_p = numeric(0), c_e = numeric(0))
      for(i in 1:iterations) {
        out <- rbind(out , ARIreport_s(d[[dIndex]], tDimention = tDimention, s = s))
      }
      d[[dIndex]]$ARIreport <- out
      print(dIndex)
    }
    toc()
    save(file = paste0('SimData_S',s,'D',tDimention,'_Sim_',gsub('( )|(-)|(:)', '', Sys.time()),'.RData'), list = c('d'))
    
  }
}


# vs. reports -------------------------------------------------------------

# iterations <- 200
# range <- seq(from = 1, to = 2, by = 0.1)
# tic()
# for(dIndex in names(d)) {
#   out <- data.frame(ari_d = numeric(0), ari_p = numeric(0), c_e = numeric(0))
#   out1 <- data.frame(alpha = numeric(0),
#                      ari_d = numeric(0), sd_ari_d = numeric(0),
#                      ari_p = numeric(0), sd_ari_p = numeric(0),
#                      c_e = numeric(0), sd_c_e = numeric(0))
#   for(j in range){
#     for(i in 1:iterations) {
#       out <- rbind(out , ARIreport(d[[dIndex]], tDimention = 2, alpha = j))
#     }
#     out1 <- rbind(out1, data.frame(alpha = j,
#                                    ari_d  = mean(out[,1]), sd_ari_d = sd(out[,1]),
#                                    ari_p  = mean(out[,2]), sd_ari_p = sd(out[,2]),
#                                    c_e    = mean(out[,3]), sd_c_e = sd(out[,3])
#                                    )
#     )
#   }
#   d[[dIndex]]$ARIvsAlpha <- out1
#   print(dIndex)
# }
# toc()
#
# save(file = 'SimData_ARIvsAlphaD2_1jM.RData', list = c('d'))
#
#
# beep(4)
#
# iterations <- 200
# range <- seq(from = 1.5, to = 2.5, by = 0.1)
# tic()
# for(dIndex in names(d)) {
#   out <- data.frame(ari_d = numeric(0), ari_p = numeric(0), c_e = numeric(0))
#   out1 <- data.frame(alpha = numeric(0), ari_d = numeric(0), ari_p = numeric(0), c_e = numeric(0))
#   for(j in range){
#     for(i in 1:iterations) {
#       out <- rbind(out , ARIreport_s(d[[dIndex]], tDimention = 2, s = j))
#     }
#     out1 <- rbind(out1, data.frame(alpha = j,
#                                    ari_d  = mean(out[,1]), sd_ari_d = sd(out[,1]),
#                                    ari_p  = mean(out[,2]), sd_ari_p = sd(out[,2]),
#                                    c_e    = mean(out[,3]), sd_c_e = sd(out[,3])
#     )
#     )
#   }
#   d[[dIndex]]$ARIvsAlpha <- out1
#   print(dIndex)
# }
# toc()
#
# save(file = 'SimData_ARIvsSD2_1jN.RData', list = c('d'))
#
#
# beep(4)
#
# iterations <- 200
# range <- seq(from = 1, to = 2, by = 0.1)
# tic()
# for(dIndex in names(d)) {
#   out <- data.frame(ari_d = numeric(0), ari_p = numeric(0), c_e = numeric(0))
#   out1 <- data.frame(alpha = numeric(0),
#                      ari_d = numeric(0), sd_ari_d = numeric(0),
#                      ari_p = numeric(0), sd_ari_p = numeric(0),
#                      c_e = numeric(0), sd_c_e = numeric(0))
#   for(j in range){
#     for(i in 1:iterations) {
#       out <- rbind(out , ARIreport(d[[dIndex]], tDimention = 3, alpha = j))
#     }
#     out1 <- rbind(out1, data.frame(alpha = j,
#                                    ari_d  = mean(out[,1]), sd_ari_d = sd(out[,1]),
#                                    ari_p  = mean(out[,2]), sd_ari_p = sd(out[,2]),
#                                    c_e    = mean(out[,3]), sd_c_e = sd(out[,3])
#     )
#     )
#   }
#   d[[dIndex]]$ARIvsAlpha <- out1
#   print(dIndex)
# }
# toc()
#
# save(file = 'SimData_ARIvsAlphaD3_1jP.RData', list = c('d'))
#
#
# beep(4)
#
# iterations <- 200
# range <- seq(from = 1.5, to = 2.5, by = 0.1)
# tic()
# for(dIndex in names(d)) {
#   out <- data.frame(ari_d = numeric(0), ari_p = numeric(0), c_e = numeric(0))
#   out1 <- data.frame(alpha = numeric(0), ari_d = numeric(0), ari_p = numeric(0), c_e = numeric(0))
#   for(j in range){
#     for(i in 1:iterations) {
#       out <- rbind(out , ARIreport_s(d[[dIndex]], tDimention = 3, s = j))
#     }
#     out1 <- rbind(out1, data.frame(alpha = j,
#                                    ari_d  = mean(out[,1]), sd_ari_d = sd(out[,1]),
#                                    ari_p  = mean(out[,2]), sd_ari_p = sd(out[,2]),
#                                    c_e    = mean(out[,3]), sd_c_e = sd(out[,3])
#     )
#     )
#   }
#   d[[dIndex]]$ARIvsAlpha <- out1
#   print(dIndex)
# }
# toc()
#
# save(file = 'SimData_ARIvsSD3_1jQ.RData', list = c('d'))
#
#
# beep(4)


# Old ---------------------------------------------------------------------


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