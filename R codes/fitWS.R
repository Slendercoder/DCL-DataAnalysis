source("WSpred.R")
library(dfoptim)
library(beepr)

data = read.csv('../Python Codes/frequencies_test.csv')
data = read.csv('frequencies.csv')
data = read.csv("../Python Codes/fileFreqs.csv")
#head(data)

args <- getArgs(data, regiones)
#args <- args[order(args$s),] 
head(args)

# To search for best parameters WSLS model
w1 <- 0.035 # bias FOCAL
w2 <- 110 # win stay 
fitresWSLS <- nmkb(par=c(w1, w2),
               fn = function(theta) WSutil(c(theta, 500, 0.98, 0, 0, 0, 0), args, regiones),
               lower=c(0,
                       0),
               upper=c(0.075,
                       200),
               control=list(trace=0))

beep()
print(fitresWSLS$par) 
print(fitresWSLS$value) 

dev <-fitresWSLS$value 
#theta <- c(0.03, 150, 500, 0.98, 0, 0, 0, 0)
#dev <- WSutil(theta, args, regions) 
#dev # 4758
aic <- 2*4 + dev
aic # 4766


# To search for best parameters WSLS model
w1 <- 0.035 # bias FOCAL
w2 <- 110 # win stay 
w3 <- 400 # beta 
w4 <- 0.98 # gamma 
fitresWSLS <- nmkb(par=c(w1, w2, w3, w4),
                   fn = function(theta) WSutil(c(theta, 0, 0, 0, 0), args, regions),
                   lower=c(0,
                           0,
                           0,
                           0),
                   upper=c(0.075,
                           150,
                           500,
                           1),
                   control=list(trace=0))

beep()
print(fitresWSLS$par) 
print(fitresWSLS$value) 
