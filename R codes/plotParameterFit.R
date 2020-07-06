source('MODELpred.R')
source("Model_Plots.R")

####################################################
# Loading parameters from estimations...
####################################################
archivo <- "../Data/parameter_fit_humans.csv"
print(paste("Loading and preparing data", archivo, "..."))
parametros = read.csv(archivo)
thetaWS <- unlist(parametros[parametros['Model']=='WSLS', ][2:11])
imprimir(thetaWS)
thetaFR <- unlist(parametros[parametros['Model']=='FRA', ][2:11])
imprimir(thetaFR)

archivo <- "../Data/humans_only_absent.csv"
print(paste("Loading and preparing data", archivo, "..."))
df = read.csv(archivo)

####################################################
# Plotting...
####################################################

df1 <- getRelFreq_Rows(df)
d2 <- plot_FocalTransitions(df1)
d2 <- plot_ModelTransitions_Focal(thetaWS, d2, "#E69F00")
d2 <- plot_ModelTransitions_Focal(thetaFR, d2, "#56B4E9")
d2

df$Region <- df$Category
df <- find_joint_region(df)
df$RegionFULL <- unlist(df$RegionFULL)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
print(head(df))
args <- getFreqFRA(df, theta)
args <- get_FRASims(args)
head(args)
plot_Transitions_FRASim_k(args, 'RIGHT')

a <- c(0)
for (k in regiones) {
  df_Focal <- df[df$Region == 'RS', ]
  df_Focal <- df_Focal[df_Focal$RegionGo == k, ]
  a <- c(a, dim(df_Focal)[1])
  print(dim(df_Focal))
}
sum(a)

# PLOT AT DYADIC LEVEL

archivo <- "../Data/humans_only_absent.csv"
print(paste("Loading and preparing data", archivo, "..."))
df1 = read.csv(archivo)
df1$Region <- df1$Category
df1$Exp <- as.character("Humans")
archivo <- "../Data/FittedMBiases.csv"
print(paste("Loading and preparing data", archivo, "..."))
df2 = read.csv(archivo)
df2$Region <- df2$Category
df2$Exp <- as.character("MBiases")
archivo <- "../Data/FittedWSLS.csv"
print(paste("Loading and preparing data", archivo, "..."))
df3 = read.csv(archivo)
df3$Region <- df3$Category
df3$Exp <- as.character("WSLS")
archivo <- "../Data/FittedFRA.csv"
print(paste("Loading and preparing data", archivo, "..."))
df4 = read.csv(archivo)
df4$Region <- df4$Category
df4$Exp <- as.character("FRA")

q <- plot_behavioral_data_fit(df1, df2, df3, df4)
