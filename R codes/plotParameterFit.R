source("Model_Plots.R")
source('MODELpred-4deltas.R') # Load after Model_Plots

####################################################
# Loading parameters from estimations...
####################################################
archivo <- "../Data/parameter_fit_humans.csv"
print(paste("Loading and preparing data", archivo, "..."))
parametros = read.csv(archivo)
thetaWS <- unlist(parametros[parametros['Model']=='WSLS', ][2:14])
imprimir(thetaWS)
thetaFR <- unlist(parametros[parametros['Model']=='FRA', ][2:14])
imprimir(thetaFR)

# archivo <- "../Data/humans_only_absent.csv"
archivo <- "../Data/high_performing_human_dyads.csv"
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

pA <- plot_Transitions_FRASim_k(args, 'ALL')
pA <- plot_ModelTransition_k_FRA(args, thetaFR, 'ALL')
pN <- plot_Transitions_FRASim_k(args, 'NOTHING')
pN <- plot_ModelTransition_k_FRA(args, thetaFR, 'NOTHING')
pL <- plot_Transitions_FRASim_k(args, 'LEFT')
pL <- plot_ModelTransition_k_FRA(args, thetaFR, 'LEFT')
pR <- plot_Transitions_FRASim_k(args, 'RIGHT')
pR <- plot_ModelTransition_k_FRA(args, thetaFR, 'RIGHT')
pT <- plot_Transitions_FRASim_k(args, 'TOP')
pT <- plot_ModelTransition_k_FRA(args, thetaFR, 'TOP')
pB <- plot_Transitions_FRASim_k(args, 'BOTTOM')
pB <- plot_ModelTransition_k_FRA(args, thetaFR, 'BOTTOM')
pI <- plot_Transitions_FRASim_k(args, 'IN')
pI <- plot_ModelTransition_k_FRA(args, thetaFR, 'IN')
pO <- plot_Transitions_FRASim_k(args, 'OUT')
pO <- plot_ModelTransition_k_FRA(args, thetaFR, 'OUT')

p <- grid.arrange(d2, pA, pN,
             pI, pO,
             pL, pR, pT, pB)
p

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
