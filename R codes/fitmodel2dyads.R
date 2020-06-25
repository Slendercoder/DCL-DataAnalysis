source("MODELpred.R")

###############################################################
# Parameter recovery function
###############################################################

fitModels2Data <- function(args, contador=0) {
  
  Trials <- 1
  parametros <- list(rep(0, 10), rep(0, 10), rep(0, 10))
  
  f_MBi <- searchBestFit_MBiases(args, N=Trials, module="nmkb", contador)
  f_WSLS <- searchBestFit_WSLS(args, N=Trials, module="nmkb", contador)
  f_FRA <- searchBestFit_FRA(args, N=Trials, module="nmkb", contador)
  print("--------------")
  tryCatch({
    print(cat("MBiases dev: ",f_MBi$value))
    imprimir(f_MBi$par)
    parametros[1] <- f_MBi$par
  }, error = function(e) {
    print("Optimizer didn't work for MBiases")
  })
  print("--------------")
  tryCatch({
    print(cat("WSLS dev: ",f_WSLS$value))
    imprimir(f_WSLS$par)
    parametros[2] <- f_WSLS$par
  }, error = function(e) {
    print("Optimizer didn't work for WSLS")
  })
  print("--------------")
  tryCatch({
    print(cat("FRA dev: ",f_FRA$value))
    imprimir(f_FRA$par)
    parametros[3] <- f_FRA$par
  }, error = function(e) {
    print("Optimizer didn't work for FRA")
  })
  
  return(parametros)
  
} # end fitModels2Data

####################################

archivo <- "../Data/humans_only_absent.csv"
df = read.csv(archivo)
parejas <- unique(df$Dyad)
modelo <- c('MBiases', 'WSLS', 'FRA')

###############################################################
# Fitting models to dyads...
###############################################################

for (dyad in parejas) {
  archivo <- paste("../Data/Dyads/output-", dyad, ".csv", sep="")
  print(paste("Loading and preparing data", archivo, "..."))
  df = read.csv(archivo)
  df$Region <- df$Category
  juagadores <- unique(df$Player)
  for (jugador in jugadores) {
    print(paste("Fitting player", jugador))
    df1 <- df[df$Player == jugador, ]
    df1 <- find_joint_region(df1)
    df1$RegionFULL <- unlist(df1$RegionFULL)
    df1$RegionGo <- factor(df1$RegionGo, levels = regiones)
    print(head(df1))
    args <- getFreqFRA(df1, theta)
    args <- get_FRASims_list(args)
    print(head(args))
    MB <- fitModels2Data(args, rotulo)
    print(MB)
  }
}
