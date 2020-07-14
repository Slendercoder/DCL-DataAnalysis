source("MODELpred-4deltas.R")

###############################################################
# Parameter recovery function
###############################################################

fitModels2Data <- function(args) {

  Trials <- 100
  parametros <- list(rep(0, 14), rep(0, 14), rep(0, 14))
  devs <- c(100000, 100000, 100000)
  f_MBi <- searchBestFit_MBiases(args, N=Trials, module="nmkb", contador, FALSE)
  f_WSLS <- searchBestFit_WSLS(args, N=Trials, module="nmkb", contador, FALSE)
  f_FRA <- searchBestFit_FRA(args, N=Trials, module="nmkb", contador, FALSE)
  print("--------------")
  tryCatch({
    print(cat("MBiases dev: ",f_MBi$value))
    imprimir(f_MBi$par)
    parametros[[1]] <- c('MBiases', f_MBi$par, rep(0,9))
    devs[1] <- f_MBi$value
  }, error = function(e) {
    print("Optimizer didn't work for MBiases")
  })
  print("--------------")
  tryCatch({
    print(cat("WSLS dev: ",f_WSLS$value))
    imprimir(f_WSLS$par)
    parametros[[2]] <- c('WSLS', f_WSLS$par, rep(0,6))
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
                   'delta', 'epsilon', 'zeta', 'delta1', 'delta2', 'delta3')
  data$dev <- devs
  return(data)

} # end fitModels2Data

####################################################

# archivo <- "../Data/Confusion/Simulations/MB7.csv"
# archivo <- "../Data/Confusion/Simulations/WS2.csv"
# archivo <- "../Data/Confusion/Simulations/FR7.csv"
archivo <- "../Data/humans_only_absent.csv"
# archivo <- "../Data/high_performing_human_dyads.csv"
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
print("Data prepared!")

fitdata <- fitModels2Data(args)
write.csv(fitdata, '../Data/parameter_fit_humans.csv', row.names=FALSE)
