source("FRApred.R")
source("Model_Plots.R")
library(dfoptim)
library(beepr)

#archivo <- "../Python Codes/Simulations/M5_full.csv"
archivo <- "../Python Codes/Simulations/N4_full.csv"

df = read.csv(archivo)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
#finding joint region
df <- find_joint_region(df)
df <- get_FRASims(df)
beep()
head(df)

###############################################################
# Plotting...
###############################################################

alpha <- 0.3
min_score = 0
max_score = 2
theta <- c(0.001, 0.001, 0.001, 0.001, 500, 500, 32, 50, 500, 0.9)
params <- para_visualizar(imprimir(theta))

q <- plot_6panels(archivo)
regs <- c('ALL', 'DOWN', 'IN')
p <- plot_FRA_regs(df, regs)
grid.arrange(q, p, ncol=2, widths=c(2/3, 1/3))
