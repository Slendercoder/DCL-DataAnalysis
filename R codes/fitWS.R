source("WSpred.R")
library(dfoptim)
library(bbmle)
library(beepr)

archivo <- "../Data/humans_only_absent.csv"
df1 = read.csv(archivo, na.strings=c("","NA"))
df1 <- df1[complete.cases(df1), ]
df1$Region <- df1$Category
df1 <- df1[c('Dyad', 'Player', 'Region', 'Score', 'RegionGo')]
head(df1)

args <- getArgs(df1)
args <- args[order(-args$s, args$i),] 
args <- args[c('pair', 'freq', 'sumFreq')]
head(args)

# To search for best parameters WSLS model with mle2
fitresWSLS <- mle2(minuslogl=WSutil1,
                   start=list(a=0.1,b=1),
                   lower=c(a=0,b=.01),
                   upper=c(a=1.5,b=200),
                   method="L-BFGS-B")

print(summary(fitresWSLS))


# To search for best parameters WSLS model with optim
# Keep constant beta and gamma
w1 <- 0.1 # w
w2 <- 10 # win stay 
fitresWSLS <- nmkb(par=c(w1, w2),
               fn = function(theta) WSutil(c(theta, 10, 31, 0, 0, 0, 0), args, regiones),
               lower=c(0,
                       0),
               upper=c(1.5,
                       200),
               control=list(trace=0))

#beep()
print(fitresWSLS$par) 
print(fitresWSLS$value) 

# Allow beta and gamma to be free
w1 <- 0.1 # w
w2 <- 10 # win stay 
w3 <- 10 # steepness of sigmoid
w4 <- 10 # threshold of sigmoid
fitresWSLS <- nmkb(par=c(w1, w2, w3, w4),
                   fn = function(theta) WSutil(c(theta, 0, 0, 0, 0), args, regiones),
                   lower=c(0,
                           0,
                           0,
                           0),
                   upper=c(1.5,
                           20,
                           32,
                           200),
                   control=list(trace=0))

#beep()
print(fitresWSLS$par) 
print(fitresWSLS$value) 



dev <-fitresWSLS$value 
dev # 2289

aic <- 2*4 + dev
aic # 2297

theta <- c(0.03, 1, 500, 98, 0, 0, 0, 0)
dev <- WSutil(theta, args, regiones)
