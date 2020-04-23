source("FRApred.R")
library(beepr)

###############################################################
# Loading and preparing data...
###############################################################

print("Loading and preparing the data...")
#archivo <- "../Python Codes/Simulations/M5_full.csv"
#archivo <- "../Python Codes/Simulations/N1_full.csv"
#archivo <- "../Python Codes/Dyads/output-435-261.csv"

#archivo <- "N1_full.csv"
archivo <- "output-435-261.csv"
df = read.csv(archivo)
df <- find_joint_region(df)
df$RegionFULL <- unlist(df$RegionFULL)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
print(head(df))
args <- getFreqFRA(df, theta)
args <- get_FRASims_list(args)
print(head(args))
beep()

###############################################################
# Parameter recovery...
###############################################################

f <- searchBestFit(args, N=5, module="mle2")
f <- searchBestFit(args, N=5, module="nmkb")
beep()

###############################################################
# Plotting...
###############################################################

#source("Model_Plots.R")
#df <- get_FRASims(df)
#df$RegionFULL <- unlist(df$RegionFULL)
#df$RegionGo <- factor(df$RegionGo, levels = regiones)
#min_score <- 0
#max_score <- 32
#theta <- f$par
#regs <- c('ALL', 'DOWN', 'IN')
#p <- plot_FRA_regs(df, regs)
