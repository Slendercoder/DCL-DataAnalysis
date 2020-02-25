source("WSpred.R")
library(dfoptim)
library(bbmle)
library(beepr)

#df1 = read.csv("../Python Codes/Dyads/output-356-137.csv", na.strings=c("","NA"))
df1 = read.csv("../Python Codes/Dyads/output-356-137PL2.csv", na.strings=c("","NA"))
#df1 = read.csv("../Python Codes/Dyads/output-356-137PL1.csv", na.strings=c("","NA"))
#df1 = read.csv("../Python Codes/humans.csv", na.strings=c("","NA"))
head(df1)

args <- getFreq(df1)

fitresWSLS <- searchBestFit(args, 10)

beep()
print(fitresWSLS$value) 
imprimir(fitresWSLS$par)
cat('wALL', fitresWSLS$par[1], 
    'wNOTHING', fitresWSLS$par[2],
    '\nwLEFT', fitresWSLS$par[3],
    'wIN', fitresWSLS$par[4],
    '\nalpha', fitresWSLS$par[5],
    'beta', fitresWSLS$par[6],
    'gamma', fitresWSLS$par[7]) 

dev <-fitresWSLS$value 
dev

aic <- 2*4 + dev
aic # 2297

theta <- c(0.03, 1, 500, 98, 0, 0, 0, 0)
dev <- WSutil(theta, args)
