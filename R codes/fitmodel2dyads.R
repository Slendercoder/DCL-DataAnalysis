source("MODELpred.R")

###############################################################
# Parameter recovery function
###############################################################

fitModels2Data <- function(args, contador=0, label) {
  
  Trials <- 20
  parametros <- list(rep(0, 12), rep(0, 12), rep(0, 12))
  devs <- c(100000, 100000, 100000)
  f_MBi <- searchBestFit_MBiases(args, N=Trials, module="nmkb", contador, FALSE)
  f_WSLS <- searchBestFit_WSLS(args, N=Trials, module="nmkb", contador, FALSE)
  f_FRA <- searchBestFit_FRA(args, N=Trials, module="nmkb", contador, FALSE)
  print("--------------")
  tryCatch({
    print(cat("MBiases dev: ",f_MBi$value))
    imprimir(f_MBi$par)
    parametros[[1]] <- c(label, 'MBiases', f_MBi$par, rep(0,6))
    devs[1] <- f_MBi$value
  }, error = function(e) {
    print("Optimizer didn't work for MBiases")
  })
  print("--------------")
  tryCatch({
    print(cat("WSLS dev: ",f_WSLS$value))
    imprimir(f_WSLS$par)
    parametros[[2]] <- c(label, 'WSLS', f_WSLS$par, rep(0,3))
    devs[2] <- f_WSLS$value
  }, error = function(e) {
    print("Optimizer didn't work for WSLS")
  })
  print("--------------")
  tryCatch({
    print(cat("FRA dev: ",f_FRA$value))
    imprimir(f_FRA$par)
    parametros[[3]] <- c(label, 'FRA', f_FRA$par)
    devs[3] <- f_FRA$value
  }, error = function(e) {
    print("Optimizer didn't work for FRA")
  })
  
  data <- as.data.frame(do.call(rbind, parametros))
  names(data) <- c('Dyad', 'Model', 
                   'wA', 'wN', 'wL', 'wI',
                   'alpha', 'beta', 'gamma',
                   'delta', 'epsilon', 'zeta')
  data$dev <- devs
  return(data)
  
} # end fitModels2Data

####################################

archivo <- "../Data/humans_only_absent.csv"
df = read.csv(archivo)
parejas <- unique(df$Dyad)

###############################################################
# Fitting models to dyads...
###############################################################
data <- as.data.frame(matrix(ncol = 13, nrow = 0))
names(data) <- c('Dyad', 'Model', 
                 'wA', 'wN', 'wL', 'wI',
                 'alpha', 'beta', 'gamma',
                 'delta', 'epsilon', 'zeta', 'dev')
for (dyad in parejas) {
  archivo <- paste("../Data/Dyads/output-", dyad, ".csv", sep="")
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
  MB <- fitModels2Data(args, label=dyad)
  data <- rbind(data, MB)
}
write.csv(data, '../Data/Dyads/dyads-fitted.csv', row.names = FALSE)
