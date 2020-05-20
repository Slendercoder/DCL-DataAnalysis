source("MODELpred.R")

###############################################################
# Parameter recovery function
###############################################################

fitModels2Data <- function(args, contador=0) {
  
  Trials <- 5
  
  cm <- c(1000000, 1000000, 1000000)
  
  f_MBi <- searchBestFit_MBiases(args, N=Trials, module="nmkb", contador)
  f_WSLS <- searchBestFit_WSLS(args, N=Trials, module="nmkb", contador)
  f_FRA <- searchBestFit_FRA(args, N=Trials, module="nmkb", contador)
  print(cat("MBiases dev: ",f_MBi$value))
  imprimir(f_MBi$par)
  cm[1] <- f_MBi$value
  print("--------------")
  print(cat("WSLS dev: ",f_WSLS$value))
  imprimir(f_WSLS$par)
  cm[2] <- f_WSLS$value
  print("--------------")
  print(cat("FRA dev: ",f_FRA$value))
  imprimir(f_FRA$par)
  cm[3] <- f_FRA$value
  
  return(cm)
  
} # end fitModels2Data

####################################

model <- c('MBiases', 'WSLS', 'FRA')
matriz <- data.frame(model)

###############################################################
# Loading and preparing data...
###############################################################

a <- seq(0, 9)
for (contador in a) {
  archivo <- paste("../Data/Confusion/MB", contador, ".csv", sep="")
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
  MB <- fitModels2Data(args, contador)
  matriz <- cbind(matriz, data.frame(MB))
  
  archivo <- paste("../Data/Confusion/WS", contador, ".csv", sep="")
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
  WS <- fitModels2Data(args, contador)
  matriz <- cbind(matriz, data.frame(WS))
  
  archivo <- paste("../Data/Confusion/FR", contador, ".csv", sep="")
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
  FR <- fitModels2Data(args, contador)
  matriz <- cbind(matriz, data.frame(FR))
}

write.csv(matriz, "confusion_matrix.csv", row.names = FALSE)