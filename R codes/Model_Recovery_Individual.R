source("WSpred.R")
source("Model_Plots.R")

#####################################################
# Global variables
#####################################################

# True parameters: 
thetaTRUE <- c(0.05, 0.05, 0.05, 0.05, 500, 500, 32)

min_score = 0
max_score = 35

True_model_color = "#E69F00"
Recovered_model_color = "#009E73"

legend2 <- get_legend_from_dummy(True_model_color, Recovered_model_color)

###############################################################################
# Loading database with full information
###############################################################################

#df1 = read.csv("../Python Codes/Simulations/M5_full.csv", na.strings=c("","NA"))
#df1 = read.csv("../Python Codes/Data_correction/Only_absent/M5_OnlyA.csv", na.strings=c("","NA"))
#df1 = read.csv("../Python Codes/Data_correction/Score_correction/M5_ScoreC.csv")
#df1 = read.csv("../Python Codes/Data_correction/Block_estimation/M5_Estimated.csv")
df1 = read.csv("../Python Codes/output.csv")
head(df1)

###############################################################################
# Obtaining estimated values...
###############################################################################

args <- getFreq(df1)
fitresWSLS <- searchBestFit(args, 100)
theta <- fitresWSLS$par
para_visualizar(imprimir(theta))
beep()

###############################################################################
# Obtaining frequencies...
###############################################################################

df <- getRelFreq_Rows(df1)
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

legend_bottom = paste("True:", para_visualizar(thetaTRUE), "Recovered:", para_visualizar(imprimir(theta)))
grid.arrange(d1, d2, nrow = 1, 
             top=legend2,
             bottom=legend_bottom)

