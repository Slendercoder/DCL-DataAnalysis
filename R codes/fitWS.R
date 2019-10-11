source("WSpred.R")
source("getFrequencies.R")
library(dfoptim)
library(beepr)

df1 = read.csv("../Python Codes/tofitWSLS-ALL.csv")
#df1 = read.csv("../Python Codes/humans.csv")
head(df1)
getFreqFromGameWS(df1)

data = read.csv('frequencies.csv')
#data = read.csv("../Python Codes/fileFreqs.csv")
head(data)

args <- getArgs(data, regiones)
args <- args[order(-args$s, args$i),] 
head(args)
dim(args)

# To search for best parameters WSLS model
w1 <- 0.1 # w
w2 <- 100 # win stay 
fitresWSLS <- nmkb(par=c(w1, w2),
               fn = function(theta) WSutil(c(theta, 500, 0.98, 0, 0, 0, 0), args, regiones),
               lower=c(0,
                       1),
               upper=c(1,
                       200),
               control=list(trace=0))

beep()
print(fitresWSLS$par) 
print(fitresWSLS$value) 

dev <-fitresWSLS$value 
dev # 2289

aic <- 2*4 + dev
aic # 2297

theta <- c(0.03, 1, 500, 98, 0, 0, 0, 0)
dev <- WSutil(theta, args, regiones)
