source("getFrequencies.R")
source("FRApred_full.R")
library(dfoptim)
library(beepr)

#df1 = read.csv("../Python Codes/Dyads/output-435-261PL1.csv", na.strings=c("","NA"))
#df1 = read.csv("../Python Codes/Dyads/output-435-261PL2.csv", na.strings=c("","NA"))
#df1 = read.csv("../Python Codes/Dyads/output-356-137.csv", na.strings=c("","NA"))
df1 = read.csv("../Python Codes/humans.csv")
head(df1)
getFreqFromGameFRA(df1)

data = read.csv("frequencies.csv")
head(data)

#-------------------------------------------------
# To separate per player
#-------------------------------------------------
#player = '435-261PL1'
player = '435-261PL2'
#player = '356-137PL1'
#player = '356-137PL2'
data = data[data$Player == player, ]
#-------------------------------------------------

args <- getArgs(data, regiones)
#args <- args[args$sumFreq > 5, ]
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
                           0,
                           0,
                           0,
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
                           100,
                           100,
                           100),
                   control=list(trace=0))

beep()
print(fitresFRA$value) 
imprimir(fitresFRA$par)
cat('wALL', fitresFRA$par[1], 
    'wNOTHING', fitresFRA$par[2],
    '\nwLEFT', fitresFRA$par[3],
    'wIN', fitresFRA$par[4],
    '\nalpha', fitresFRA$par[5],
    'beta', fitresFRA$par[6],
    'gamma', fitresFRA$par[7],
    '\ndelta', fitresFRA$par[8],
    'epsilon', fitresFRA$par[9],
    '\nzeta', fitresFRA$par[10],
    'eta', fitresFRA$par[11]
) 


theta <- c(wAll, wNoth, wLef, wIn, w2, w3, w4, w5, w6, w7, w8)
dev <- FRAutil(theta, args, regiones)
dev # 2126

aic <- 2*8 + dev
aic # 2142

2443 - 2246
2435 - 2230
