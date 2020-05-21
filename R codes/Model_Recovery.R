source("WSpred.R")
library(stats4)
library(bbmle)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(beepr)

#####################################################
# Global variables
#####################################################

# True parameters: 
thetaTRUE <- c(0.0003188711883553644,0.010299533842124295,0.006652270050406081,0.003911378007830106,
               499.51419259640335,500,3.430524866216885)

min_score = 0

True_model_color = "#E69F00"
Recovered_model_color = "#009E73"

legend2 <- get_legend_from_dummy(True_model_color, Recovered_model_color)

###############################################################################
# Loading database with full information
###############################################################################

#df1 = read.csv("../Python Codes/Model_recovery/M5_full.csv", na.strings=c("","NA"))
#df1 = read.csv("../Python Codes/Data_correction/Only_absent/M5_OnlyA.csv", na.strings=c("","NA"))
#df1 = read.csv("../Python Codes/Data_correction/Score_correction/M5_ScoreC.csv")
df1 = read.csv("../Data/Confusion/WS0.csv")
#head(df1)

###############################################################################
# Obtaining estimated values...
###############################################################################

#args <- getFreq(df1)
#fitresWSLS <- searchBestFit(args, 50)
#theta <- fitresWSLS$par
#imprimir(theta)
#beep()
theta <- c(0.000208340533387852, 0.00824876924135664, 0.00650159269113997, 0.00660437614396095,
           19.9911912034854, 500, 3.20606590207864)

###############################################################################
# Obtaining frequencies...
###############################################################################

df <- getRelFreq_Rows(df1)
#dfA <- unique(df)
head(df)

###############################################################################
# Plot simulated data...
###############################################################################

d1 <- plot_RSTransitions(df)
d1 <- plot_ModelTransitions_RS(thetaTRUE, d1, True_model_color)
d1 <- plot_ModelTransitions_RS(theta, d1, Recovered_model_color)

d2 <- plot_FocalTransitions(df)
d2 <- plot_ModelTransitions_Focal(thetaTRUE, d2, True_model_color)
d2 <- plot_ModelTransitions_Focal(theta, d2, Recovered_model_color)

grid.arrange(d1, d2, nrow = 1, 
             top=legend2,
             bottom=para_visualizar(thetaTRUE))

