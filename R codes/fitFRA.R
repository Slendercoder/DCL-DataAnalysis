source("FRApred.R")
source("Model_Plots.R")
library(dfoptim)
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
theta <- c(0.001, 0.001, 0.001, 0.001, 500, 500, 32, 500, 500, 0.7)
params <- para_visualizar(imprimir(theta))

f <- FRAutil(theta, args)

f <- searchFit(theta, args)

###############################################################
# Plotting...
###############################################################

alpha <- 0.3
min_score = 0
max_score = 2
q <- plot_6panels(archivo)
regs <- c('ALL', 'DOWN', 'IN')
p <- plot_FRA_regs(df, regs)
grid.arrange(q, p, ncol=2, widths=c(2/3, 1/3))

# To search for best parameters FRA model
w1 <- 0.1 # bias ALL
w2 <- 0.1 # bias NOTHING
w3 <- 0.1 # bias LEFT
w4 <- 0.1 # bias IN
w5 <- 0.05 # alpha
w6 <- 0.5 # beta
w7 <- 0.5 # gamma
w8 <- 0.5 # delta
w9 <- 0.5 # epsilon
w10 <- 0.5 # zeta
fitresFRA <- nmkb(par=c(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10),
                   fn = function(theta) FRAutil(theta, args, regiones),
                   lower=c(0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0),
                   upper=c(0.1,
                           0.1,
                           0.1,
                           0.1,
                           500,
                           1000,
                           10),
                   control=list(trace=0))

beep()
print(fitresFRA$par) 
print(fitresFRA$value) 
dev <- fitresFRA$value

theta <- c(2.204720e-01, 1.322340e+01, 500, 0.98, 7.163690e-08, 1, 1.718298e+00, 1.2)
dev <- FRAutil(theta, args, regiones)
dev # 2126

aic <- 2*8 + dev
aic # 2142

2443 - 2246
2435 - 2230
