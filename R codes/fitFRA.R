source("getFrequencies.R")
source("FRApred.R")
library(dfoptim)
library(beepr)

df1 = read.csv("../Python Codes/output.csv")
head(df1)
getFreqFromGameFRA(df1)

data = read.csv("frequencies.csv")
head(data)

args <- getArgs(data, regiones)
args <- args[order(-args$s, args$i),] 
beep()
head(args)
dim(args)

# To search for best parameters FRA model
w1 <- 0.001 # bias FOCAL
w2 <- 5 # win stay 
w3 <- 0.5 # delta
w4 <- 1 # zeta
fitresFRA <- nmkb(par=c(w2, w3, w4),
                   fn = function(theta) FRAutil(c(0, 
                                                 theta[1], 
                                                 500, 
                                                 0.98, 
                                                 theta[2], 
                                                 1, 
                                                 theta[3], 
                                                 1.2), args, regiones),
                   lower=c(0,
                           0,
                           0),
                   upper=c(10,
                           10,
                           10),
                   control=list(trace=0))

beep()
print(fitresFRA$par) 
print(fitresFRA$value) 
dev <- fitresFRA$value

theta <- c(0.018, 13, 500, 0.98, 0, 1, 1.76, 1.2)
dev <- FRAutil(theta, args, regiones)
dev # 2227

aic <- 2*8 + dev
aic # 2246

2443 - 2246
2435 - 2230
