source("MODELpred.R")
# source("Model_Plots.R")

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

###########################################
# print("-------------------")
# params <- c(0.01, 0.01, 0.08, 0.001, 10, 100, 30)
# f <- searchFit_WSLS_NMKB(params, args)
# print(cat("WSLS dev: ",f$value))
# imprimir(f$par)
# print("-------------------")
# params <- c(0.01, 0.01, 0.08, 0.001, 10, 10, 30, 10, 10, 0.7)
# f <- searchFit_FRA_NMKB(params, args)
# print(cat("FRA dev: ",f$value))
# imprimir(f$par)
# print("-------------------")
# f <- searchBestFit_FRA(args, 5)
# print(cat("FRA dev: ",f$value))
# imprimir(f$par)
# print("-------------------")

# WSLS dev:  498.97NULL
# > imprimir(f$par)
# [1]  0.058  0.037  0.008  0.002 11.690  0.039 31.999

####################################################
# Plotting...
####################################################

WS_color <- cbPalette[5]
FR_color <- cbPalette[7]
min_score = 0
legend2 <- get_legend_from_dummy1(WS_color, FR_color)
#theta <- c(0.1, 0.083, 0.05, 0.006, 0, 0, 0, 0, 0, 0)
thetaWS <- c(0.086, 0.043, 0.011, 0.002, 6.596, 499.806, 4.564, 0, 0, 0)
thetaFR <- c(0.080, 0.041, 0.010, 0.002, 496.038, 499.921, 4.407, 469.507, 499.574, 0.758)

# PLOT WSLS AT INDIVIDUAL LEVEL
df <- getRelFreq_Rows(df1)
d1 <- plot_RSTransitions(df)
d1 <- plot_ModelTransitions_RS(thetaFR, d1, FR_color)
d1 <- plot_ModelTransitions_RS(thetaWS, d1, WS_color)

d2 <- plot_FocalTransitions(df)
d2 <- plot_ModelTransitions_Focal(thetaWS, d2, WS_color)
d2 <- plot_ModelTransitions_Focal(thetaFR, d2, FR_color)

grid.arrange(d1, d2, top=legend2, nrow = 1)
# 
# # PLOT FRA AT INDIVIDUAL LEVEL
# archivo <- "../Data/humans_only_absent.csv"
# print(paste("Loading and preparing data", archivo, "..."))
# df = read.csv(archivo)
# df$Region <- df$Category
# df <- find_joint_region(df)
# df <- get_FRASims(df)
# regs <- c('ALL', 'LEFT')
# p <- plot_FRA_regs(df, regs, thetaFR) 
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
