source("WSpred.R")
source("Model_Plots.R")

#min_score = -50
min_score = 0
max_score = 35
alpha <- 0.3
theta <- c(0.05, 0.05, 0.05, 0.05, 500, 500, 32)
archivo <- "../Python Codes/output.csv"
#archivo <- "../Python Codes/Model_recovery/M5_full.csv"
#archivo <- "../Python Codes/Data_correction/Only_absent/M5_onlyA.csv"
#archivo <- "../Python Codes/Data_correction/Score_correction/M5_ScoreC.csv"
#archivo <- "../Python Codes/Data_correction/Block_estimation/M5_Estimated.csv"

plot_4panels(archivo)
