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
thetaTRUE <- c(0.002288375796043474,0.006382772500187606,0.03722563296082381,0.04469836430994406,
               26.666117907625186,500,27.694808737977397)

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
theta <- c(0.00251417936534334, 0.00377623203998708, 0.0383425764589409, 0.0529675532240082,
           6.99686466692762, 500, 27.5691971782374)

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

