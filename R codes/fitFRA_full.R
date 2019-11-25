source("getFrequencies.R")
source("FRApred_full.R")
library(dfoptim)
library(beepr)

#df1 = read.csv("../Python Codes/humans.csv")
#head(df1)
#getFreqFromGameFRA(df1)

#data = read.csv("frequencies.csv")
#head(data)

#args <- getArgs(data, regiones)
#args <- args[order(-args$s, args$i),] 
#write.csv(args, file = "args.csv", row.names = FALSE)
#print('Args written in args.csv!')

args = read.csv("args.csv")
#beep()
#head(args)
#dim(args)

# To search for best parameters FRA model
wAll <- 0.12 # bias All
wNoth <- 0.12 # bias Nothing
wLef <- 0.12 # bias Left, Right, Top, Bottom
wIn <- 0.12 # bias In, Out
w2 <- 10 # win stay 
w3 <- 10 # steepness of sigmoid 
w4 <- 10 # threshold of sigmoid 
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
                   lower=c(0.1,
                           0.1,
                           0.1,
                           0.1,
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
                           30,
                           30,
                           15,
                           2,
                           10,
                           2),
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
