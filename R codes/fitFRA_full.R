#source("getFrequencies.R")
#source("FRApred.R")
source("FRApred_scoreLevel.R")
#source("FRApred_full.R")
library(dfoptim)
#library(beepr)
#library(dplyr)

df1 = read.csv("freqs4FRA-humans.csv")
#df1 = read.csv("../Python Codes/freqs4FRA-humans.csv")
#df1 = read.csv("../Python Codes/freqs4FRA-simulated.csv")
# Finding scoreLevel
df1$scoreLevel <- lapply(df1$Score, function(x) {
  if (x == 32) {
    return (32)
  } else if (x > 16) {
    return (30) 
  } else {
    return (20)
  }
})
head(df1)

args <- getArgs(df1, regiones)
#beep()
head(args)
#dim(args)

# To search for best parameters FRA model
wAll <- 0.012 # bias All
wNoth <- 0.012 # bias Nothing
wLef <- 0.012 # bias Left, Right, Top, Bottom
wIn <- 0.012 # bias In, Out
w2 <- 10 # win stay 
w3 <- 100 # steepness of WSLS sigmoid 
w4 <- 30 # threshold of WSLS sigmoid 
w5 <- 1 # FRA
w6 <- 1 # steepness of FRA sigmoid 
w7 <- 1 # threshold of FRA sigmoid
fitresFRA <- nmkb(par=c(wAll, wNoth, wLef, wIn, w2, w3, w4, w5, w6, w7),
                   fn = function(theta) FRAutil(c(theta[1],
                                                 theta[2],
                                                 theta[3],
                                                 theta[4],
                                                 theta[5],
                                                 theta[6], 
                                                 theta[7], 
                                                 theta[8], 
                                                 theta[9], 
                                                 theta[10]), args, regiones),
                   lower=c(0.001,
                           0.001,
                           0.001,
                           0.001,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0),
                   upper=c(0.15,
                           0.15,
                           0.15,
                           0.15,
                           200,
                           500,
                           32,
                           200,
                           500,
                           1.5),
                   control=list(trace=0))

#beep()
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
    '\nzeta', fitresFRA$par[10]
) 


#theta <- c(wAll, wNoth, wLef, wIn, w2, w3, w4, w5, w6, w7)
#dev <- FRAutil(theta, args, regiones)
#dev # 2126

#aic <- 2*8 + dev
#aic # 2142
