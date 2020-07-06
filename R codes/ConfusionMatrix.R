source("MODELpred.R")

###############################################################
# Parameter recovery function
###############################################################

fitModels2Data <- function(args, contador=0) {
  
  Trials <- 1
  
  # f_MBi <- searchBestFit_MBiases(args, N=Trials, module="nmkb", contador)
  # f_WSLS <- searchBestFit_WSLS(args, N=Trials, module="nmkb", contador)
  f_FRA <- searchBestFit_FRA(args, N=Trials, module="nmkb", contador)
  print("--------------")
  tryCatch({
    print(cat("MBiases dev: ",f_MBi$value))
    imprimir(f_MBi$par)
  }, error = function(e) {
    print("Optimizer didn't work for MBiases")
  })
  print("--------------")
  tryCatch({
    print(cat("WSLS dev: ",f_WSLS$value))
    imprimir(f_WSLS$par)
  }, error = function(e) {
    print("Optimizer didn't work for WSLS")
  })
  print("--------------")
  tryCatch({
    print(cat("FRA dev: ",f_FRA$value))
    imprimir(f_FRA$par)
  }, error = function(e) {
    print("Optimizer didn't work for FRA")
  })

  return("Ok")
  
} # end fitModels2Data

####################################

model <- c('MBiases', 'WSLS', 'FRA')
matriz <- data.frame(model)

###############################################################
# Loading and preparing data...
###############################################################

a <- seq(0, 0)
for (contador in a) {
  archivo <- paste("../Data/Confusion/Simulations/MB", contador, ".csv", sep="")
  print(paste("Loading and preparing data", archivo, "..."))
  df = read.csv(archivo)
  df$Region <- df$Category
  df <- find_joint_region(df)
  df$RegionFULL <- unlist(df$RegionFULL)
  df$RegionGo <- factor(df$RegionGo, levels = regiones)
  print(head(df))
  args <- getFreqFRA(df, theta)
  args <- get_FRASims_list(args)
  print(head(args))
  rotulo <- paste('MB', contador, sep="")
  MB <- fitModels2Data(args, rotulo)

  archivo <- paste("../Data/Confusion/Simulations/WS", contador, ".csv", sep="")
  print(paste("Loading and preparing data", archivo, "..."))
  df = read.csv(archivo)
  df$Region <- df$Category
  df <- find_joint_region(df)
  df$RegionFULL <- unlist(df$RegionFULL)
  df$RegionGo <- factor(df$RegionGo, levels = regiones)
  print(head(df))
  args <- getFreqFRA(df, theta)
  args <- get_FRASims_list(args)
  print(head(args))
  rotulo <- paste('WS', contador, sep="")
  WS <- fitModels2Data(args, rotulo)

  archivo <- paste("../Data/Confusion/Simulations/FR", contador, ".csv", sep="")
  print(paste("Loading and preparing data", archivo, "..."))
  df = read.csv(archivo)
  df$Region <- df$Category
  df <- find_joint_region(df)
  df$RegionFULL <- unlist(df$RegionFULL)
  df$RegionGo <- factor(df$RegionGo, levels = regiones)
  print(head(df))
  args <- getFreqFRA(df, theta)
  args <- get_FRASims_list(args)
  print(head(args))
  rotulo <- paste('FR', contador, sep="")
  FR <- fitModels2Data(args, rotulo)

}

