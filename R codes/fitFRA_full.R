source("getFrequencies.R")
source("FRApred_full.R")
library(dfoptim)
library(beepr)

df1 = read.csv("../Python Codes/humans.csv")
head(df1)
getFreqFromGameFRA(df1)

data = read.csv("frequencies.csv")
head(data)

args <- getArgs(data, regiones)
args <- args[args$sumFreq > 5, ]
beep()
args <- args[order(-args$s, args$i),] 
head(args)
#dim(args)

# To search for best parameters FRA model
wAll <- 0.012 # bias All
wNoth <- 0.012 # bias Nothing
wLef <- 0.012 # bias Left, Right, Top, Bottom
wIn <- 0.012 # bias In, Out
w2 <- 10 # win stay 
w3 <- 100 # steepness of sigmoid 
w4 <- 30 # threshold of sigmoid 
w5 <- 1 # delta
w6 <- 1 # exponential sim to complement
w7 <- 1 # zeta
w8 <- 1 # exponential sim to region
fitresFRA <- nmkb(par=c(wAll, wNoth, wLef, wIn, w2, w3, w4, w5, w6, w7, w8),
                   fn = function(theta) FRAutil(c(theta[1],
                                                 theta[2],
                                                 theta[3],
                                                 theta[4],
                                                 theta[5],
                                                 theta[6], 
                                                 theta[7], 
                                                 theta[8], 
                                                 theta[9], 
                                                 theta[10], 
                                                 theta[11]), args, regiones),
                   lower=c(0.001,
                           0.001,
                           0.001,
                           0.001,
                           8,
                           8,
                           8,
                           0.9,
                           0.5,
                           0.9,
                           0.5),
                   upper=c(0.15,
                           0.15,
                           0.15,
                           0.15,
                           200,
                           500,
                           32,
                           15,
                           2,
                           10,
                           2),
                   control=list(trace=0))

beep()
print(fitresFRA$par) 
print(fitresFRA$value) 
dev <- fitresFRA$value

theta <- c(wAll, wNoth, wLef, wIn, w2, w3, w4, w5, w6, w7, w8)
dev <- FRAutil(theta, args, regiones)
dev # 2126

aic <- 2*8 + dev
aic # 2142

2443 - 2246
2435 - 2230
