source("FRApred.R")
library(beepr)

###############################################################
# Loading and preparing the data...
###############################################################

#archivo <- "../Python Codes/Simulations/M5_full.csv"
#archivo <- "../Python Codes/Simulations/N1_full.csv"
#archivo <- "../Python Codes/Dyads/output-435-261.csv"
archivo <- "output-435-261.csv"
df = read.csv(archivo)
df <- find_joint_region(df)
df$RegionFULL <- unlist(df$RegionFULL)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
print(head(df))
args <- getFreqFRA(df, theta)

#aux <- get_FRASims(args)
#aux <- aux %>% 
#  dplyr::mutate(FRASims = as.list(data.frame(c(FRASimALL, 
#                                FRASimNOTHING, 
#                                FRASimDOWN, 
#                                FRASimUP, 
#                                FRASimLEFT, 
#                                FRASimRIGHT, 
#                                FRASimIN, 
#                                FRASimOUT)))
#                )
#args$FRAsims <- get_FRASims_list(args)
print(head(args))
beep()

###############################################################
# Parameter recovery...
###############################################################

#args <- args[1:2, ]
f <- searchBestFit(args, 1)
print(f)
beep()
