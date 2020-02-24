source("WSpred_full.R")
library(dfoptim)
library(bbmle)
library(beepr)

#df1 = read.csv("../Python Codes/Dyads/output-356-137.csv", na.strings=c("","NA"))
df1 = read.csv("../Python Codes/Dyads/output-356-137PL2.csv", na.strings=c("","NA"))
#df1 = read.csv("../Python Codes/Dyads/output-356-137PL1.csv", na.strings=c("","NA"))
#df1 = read.csv("../Python Codes/humans.csv", na.strings=c("","NA"))
head(df1)

args <- getFreq(df1)

theta <- c(0.01, 0.01, 0.01, 0.01, 500, 500, 32)

dev <- WSutil(theta, args)

## To search for best parameters WSLS model with mle2
#fitresWSLS <- mle2(minuslogl=WSutil1,
#                   start=list(a=0.1,b=1),
#                   lower=c(a=0,b=.01),
#                   upper=c(a=1.5,b=200),
#                   method="L-BFGS-B")
#
#print(summary(fitresWSLS))

# To search for best parameters WSLS model with optim
wAll <- 0.01 # w
wNoth <- 0.01 # w
wLef <- 0.01 # w
wIn <- 0.01 # w
alpha <- 40 # win stay 
beta <- 10 # steepness 
gamma <- 30.5 # threshold 
fitresWSLS <- nmkb(par=c(wAll, wNoth, wLef, wIn, alpha, beta, gamma),
               fn = function(theta) WSutil(c(theta, 0, 0, 0, 0), args, regiones),
               lower=c(0,
                       0,
                       0,
                       0,
                       30,
                       0,
                       30),
               upper=c(0.15,
                       0.15,
                       0.15,
                       0.15,
                       200,
                       500,
                       32),
               control=list(trace=0))

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
dev <- WSutil(theta, args, regiones)
