source("MODELpred.R")
source("Model_plots.R")

###############################################################
# Parameter recovery function
###############################################################

fitModels2Data <- function(args) {
  
  Trials <- 5
  
  pars <- c(list(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
            list(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
            list(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
  
  f_MBi <- searchBestFit_MBiases(args, N=Trials, module="nmkb")
  f_WSLS <- searchBestFit_WSLS(args, N=Trials, module="nmkb")
  f_FRA <- searchBestFit_FRA(args, N=Trials, module="nmkb")
  print(cat("MBiases dev: ",f_MBi$value))
  imprimir(f_MBi$par)
  pars[1] <- f_MBi$par
  print("--------------")
  print(cat("WSLS dev: ",f_WSLS$value))
  imprimir(f_WSLS$par)
  pars[2] <- f_WSLS$par
  print("--------------")
  print(cat("FRA dev: ",f_FRA$value))
  imprimir(f_FRA$par)
  pars[3] <- f_FRA$par
  
  return(pars)
  
} # end fitModels2Data

####################################################

archivo <- "../Data/humans_only_absent.csv"
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
parametros <- fitModels2Data(args)

####################################################
# Plotting...
####################################################

p <- plot_RSTransitions(df)
