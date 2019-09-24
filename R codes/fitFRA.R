source("FRApred.R")
library(dfoptim)
library(beepr)

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
                   upper=c(0.075,
                           10,
                           10,
                           10),
                   control=list(trace=0))

beep()
print(fitresFRA$par) 
print(fitresFRA$value) 


theta <- c(0.018, 5.56, 500, 0.98, 0.006, 1, 2, 1.2)
dev <- FRAutil(theta, args, regiones)
dev # ???
aic <- 2*8 + dev
aic # ???
