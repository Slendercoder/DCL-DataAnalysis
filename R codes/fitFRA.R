source("FRApred.R")
source("Model_Plots.R")
library(dfoptim)
library(beepr)

#archivo <- "../Python Codes/Simulations/M5_full.csv"
archivo <- "../Python Codes/Simulations/N1_full.csv"

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
theta <- c(0.001, 0.001, 0.001, 0.001, 500, 500, 32, 500, 500, 0.7)
params <- para_visualizar(imprimir(theta))

q <- plot_6panels(archivo)
regs <- c('ALL', 'DOWN', 'IN')
p <- plot_FRA_regs(df, regs)
grid.arrange(q, p, ncol=2, widths=c(2/3, 1/3))

args <- getFreq(df)
beep()
#head(args)
#dim(args)

# To search for best parameters FRA model
w1 <- 0.1 # bias FOCAL
w2 <- 10 # win stay 
w3 <- 0.05 # delta
w4 <- 0.5 # zeta
fitresFRA <- nmkb(par=c(w1, w2, w3, w4),
                   fn = function(theta) FRAutil(c(theta[1],
                                                 theta[2], 
                                                 10, 
                                                 31, 
                                                 theta[3], 
                                                 1, 
                                                 theta[4], 
                                                 1.2), args, regiones),
                   lower=c(0,
                           0,
                           0,
                           0),
                   upper=c(1,
                           200,
                           15,
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
