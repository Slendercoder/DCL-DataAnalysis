#source("Model_Plots.R")
source("MODELpred.R")

###############################################################
# Parameter recovery function
###############################################################


# estimate_biases <- function(df, ronda_max=10) {
#   aux <- df[df['Round'] < ronda_max, ]
#   biases <- table(aux['Category'])
#   suma <- sum(biases)
#   biases <- biases/suma
#   wA <- mean(biases[1])
#   wN <- mean(biases[5])
#   wL <- mean(biases[2], biases[4], biases[7], biases[9])
#   wI <- mean(biases[3], biases[6])
#   return(c(wA, wN, wL, wI))
# }

fitModels2Data <- function(args) {

  Trials <- 100
  parametros <- list(rep(0, 11), rep(0, 11), rep(0, 11))
  devs <- c(100000, 100000, 100000)
  f_MBi <- searchBestFit_MBiases(args, N=Trials, module="nmkb", contador, FALSE)
  f_WSLS <- searchBestFit_WSLS(args, N=Trials, module="nmkb", contador, FALSE)
  f_FRA <- searchBestFit_FRA(args, N=Trials, module="nmkb", contador, FALSE)
  print("--------------")
  tryCatch({
    print(cat("MBiases dev: ",f_MBi$value))
    imprimir(f_MBi$par)
    parametros[[1]] <- c('MBiases', f_MBi$par, rep(0,6))
    devs[1] <- f_MBi$value
  }, error = function(e) {
    print("Optimizer didn't work for MBiases")
  })
  print("--------------")
  tryCatch({
    print(cat("WSLS dev: ",f_WSLS$value))
    imprimir(f_WSLS$par)
    parametros[[2]] <- c('WSLS', f_WSLS$par, rep(0,3))
    devs[2] <- f_WSLS$value
  }, error = function(e) {
    print("Optimizer didn't work for WSLS")
  })
  print("--------------")
  tryCatch({
    print(cat("FRA dev: ",f_FRA$value))
    imprimir(f_FRA$par)
    parametros[[3]] <- c('FRA', f_FRA$par)
    devs[3] <- f_FRA$value
  }, error = function(e) {
    print("Optimizer didn't work for FRA")
  })
  
  data <- as.data.frame(do.call(rbind, parametros))
  names(data) <- c('Model', 'wA', 'wN', 'wL', 'wI',
                   'alpha', 'beta', 'gamma',
                   'delta', 'epsilon', 'zeta')
  data$dev <- devs
  return(data)

} # end fitModels2Data

####################################################

# archivo <- "../Data/Confusion/Simulations/MB7.csv"
# archivo <- "../Data/Confusion/Simulations/WS2.csv"
archivo <- "../Data/Confusion/Simulations/FR7.csv"
# archivo <- "../Data/humans_only_absent.csv"
# archivo <- "../Data/high_performing_human_dyads.csv"
print(paste("Loading and preparing data", archivo, "..."))
df = read.csv(archivo)

# p <- plot_individual_behavior(df)
# p1 <- p[[1]]
# p2 <- p[[2]]
# p3 <- p[[3]]
# pl <- grid.arrange(p1, p2, p3, nrow=1)

df$Region <- df$Category
# b <- estimate_biases(df, 5)
df <- find_joint_region(df)
df$RegionFULL <- unlist(df$RegionFULL)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
print(head(df))
args <- getFreqFRA(df, theta)
args <- get_FRASims_list(args)
print(head(args))
print("Data prepared!")

# start_pars <- c(111)
# parametros <- searchFit_WSLS_NMKB(start_pars, args, 5)
# thetaWS <- parametros
# thetaFR <- parametros

fitdata <- fitModels2Data(args)
write.csv(fitdata, '../Data/parameter_fit_humans.csv', row.names=FALSE)

# thetaWS <- parametros[[2]]
# thetaFR <- parametros[[2]]
# 
# p <- plot_model_on_top_behavior(thetaWS, thetaFR, p1, p2, p3)
# p1 <- p[[1]]
# p2 <- p[[2]]
# p3 <- p[[3]]
# pl <- grid.arrange(p1, p2, p3, nrow=1)

#################################################
# To try individual cases
#################################################
# n <- 100
# i <- args$Region[n]
# iV <- args$RegionFULL[n] 
# s <- args$Score[n]
# j <- args$RJcode[n]
# Fsims <- args$FRASims[n]
# theta <- c(0.001, 0.001, 0.001, 0.001, 0.1, 299.5, 7, 0, 1000, 0.8)
# wAll <- theta[1]
# wNoth <- theta[2]
# wLef <- theta[3]
# wIn <- theta[4]
# alpha <- theta[5]
# beta <- theta[6]
# gamma <- theta[7]
# delta <- theta[8]
# epsilon <- theta[9]
# zeta <- theta[10]
# params <- theta
# MBIASESpred(params)
# WSLSpred(i, s, params)
# FRApred1(i, iv, s, j, Fsims, params)
# MBIASESutil(wAll, wNoth, wLef, wIn)
# WSLSutil(wAll, wNoth, wLef, wIn, 
#          alpha, beta, gamma)
# FRAutil(wAll, wNoth, wLef, wIn, 
#         alpha, beta, gamma, 
#         delta, epsilon, zeta)
# 
# f <- searchFit_MBiases_NMKB(params[1:4], args)
# f <- searchFit_WSLS_NMKB(params[1:7], args)
# warnings()
# f <- searchBestFit_MBiases(args)
# f <- searchBestFit_WSLS(args, 10)

####################################################
# Plotting...
####################################################

# 
# a <- c(0)
# for (k in regiones) {
#   df_Focal <- df[df$Region == 'RS', ]
#   df_Focal <- df_Focal[df_Focal$RegionGo == k, ]
#   a <- c(a, dim(df_Focal)[1])
#   print(dim(df_Focal))
# }
# sum(a)
# 
# # PLOT AT DYADIC LEVEL
# 
# archivo <- "../Data/humans_only_absent.csv"
# print(paste("Loading and preparing data", archivo, "..."))
# df1 = read.csv(archivo)
# df1$Region <- df1$Category
# df1$Exp <- as.character("Humans")
# archivo <- "../Data/FittedMBiases.csv"
# print(paste("Loading and preparing data", archivo, "..."))
# df2 = read.csv(archivo)
# df2$Region <- df2$Category
# df2$Exp <- as.character("MBiases")
# archivo <- "../Data/FittedWSLS.csv"
# print(paste("Loading and preparing data", archivo, "..."))
# df3 = read.csv(archivo)
# df3$Region <- df3$Category
# df3$Exp <- as.character("WSLS")
# archivo <- "../Data/FittedFRA.csv"
# print(paste("Loading and preparing data", archivo, "..."))
# df4 = read.csv(archivo)
# df4$Region <- df4$Category
# df4$Exp <- as.character("FRA")
# 
# q <- plot_behavioral_data_fit(df1, df2, df3, df4)
