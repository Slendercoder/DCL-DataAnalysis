source("WSpred.R")
source("Model_Plots.R")
source("MODELpred.R")
library(dfoptim)
library(bbmle)
library(beepr)

archivo <- "../Data/Confusion/Simulations/WS0.csv"
df = read.csv(archivo, na.strings=c("","NA"))
df$Region <- df$Category
args1 <- getFreq(df)
head(args1)
# f <- searchBestFit_WSLS(args, 5)
# thetaWS1 <- f$par

df = read.csv(archivo, na.strings=c("","NA"))
df$Region <- df$Category
df <- find_joint_region(df)
df$RegionFULL <- unlist(df$RegionFULL)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
args <- getFreqFRA(df, theta)
head(args)
# f <- searchBestFit_WSLS(args, 5)
# thetaWS2 <- f$par

# WS1_color <- cbPalette[5]
# WS2_color <- cbPalette[7]
# min_score = 0
# legend2 <- get_legend_from_dummy1(WS1_color, WS2_color)
# 
# dfP <- getRelFreq_Rows(df)
# d2 <- plot_FocalTransitions(dfP)
# d2 <- plot_ModelTransitions_Focal(thetaWS1, d2, WS1_color)
# d2 <- plot_ModelTransitions_Focal(thetaWS2, d2, WS2_color)
# 
# d2
