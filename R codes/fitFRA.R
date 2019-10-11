source("getFrequencies.R")
source("FRApred.R")
library(dfoptim)
library(beepr)

#df1 = read.csv("../Python Codes/humans.csv")
df1 = read.csv("../Python Codes/tofitFRA.csv")
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
w2 <- 100 # win stay 
w3 <- 0.5 # delta
w4 <- 0.5 # zeta
fitresFRA <- nmkb(par=c(w1, w2, w3, w4),
                   fn = function(theta) FRAutil(c(theta[1],
                                                 theta[2], 
                                                 500, 
                                                 0.98, 
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
