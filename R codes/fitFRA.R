source("FRApred.R")
source("Model_Plots.R")
library(beepr)

###############################################################
# Loading and preparing the data...
###############################################################

#archivo <- "../Python Codes/Simulations/M5_full.csv"
archivo <- "../Python Codes/Simulations/N1_full.csv"
df = read.csv(archivo)
df <- find_joint_region(df)
df <- get_FRASims(df)
df$RegionFULL <- unlist(df$RegionFULL)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
head(df)
args <- getFreqFRA(df, theta)
head(args)
beep()

###############################################################
# Parameter recovery...
###############################################################

args1 <- args[1:50, ]
f <- searchBestFit(args1, 1)

###############################################################
# Plotting...
###############################################################

theta <- c(0.001, 0.001, 0.001, 0.001, 500, 500, 32, 500, 500, 0.7)
params <- para_visualizar(imprimir(theta))
alpha <- 0.3
min_score = 0
max_score = 2
q <- plot_6panels(archivo)
regs <- c('ALL', 'DOWN', 'IN')
p <- plot_FRA_regs(df, regs)
grid.arrange(q, p, ncol=2, widths=c(2/3, 1/3))
