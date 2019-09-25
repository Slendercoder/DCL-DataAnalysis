source("WSpred.R")
source("getFrequencies.R")
library(dfoptim)
library(beepr)

#df1 = read.csv("../Python Codes/output.csv")
df1 = read.csv("../Python Codes/humans.csv")
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
w1 <- 0.001 # b_all
w2 <- 0.001 # b_rest
w3 <- 0.001 # b_in_out
w4 <- 10 # win stay 
fitresWSLS <- nmkb(par=c(w1, w2, w3, w4),
               fn = function(theta) WSutil(c(theta, 500, 0.98, 0, 0, 0, 0), args, regiones),
               lower=c(0,
                       0,
                       0,
                       0),
               upper=c(1,
                       0.0125,
                       0.0125,
                       100),
               control=list(trace=0))

beep()
print(fitresWSLS$par) 
print(fitresWSLS$value) 

dev <-fitresWSLS$value 
aic <- 2*4 + dev
aic # 2443

# To search for best parameters WSLS model
w1 <- 0.035 # bias FOCAL
w2 <- 50 # win stay 
w3 <- 50 # beta
w4 <- 0.98 # gamma
fitresWSLS <- nmkb(par=c(w1, w2, w3, w4),
                   fn = function(theta) WSutil(c(theta, 0, 0, 0, 0), args, regiones),
                   lower=c(0,
                           0,
                           0,
                           0),
                   upper=c(0.075,
                           200,
                           100,
                           1),
                   control=list(trace=0))

beep()
print(fitresWSLS$par) 
print(fitresWSLS$value) 

dev <-fitresWSLS$value 
aic <- 2*4 + dev
aic # 4766

#############################

theta <- c(0.001, 0.001, 0.001, 90, 500, 0.98, 0, 0, 0, 0)

b_all <- theta[1]
b_rest <- theta[2]
b_in_out <- theta[3]
alpha <- theta[4]
beta <- theta[5]
gamma <- theta[6]
delta <- theta[7]
epsilon <- theta[8]
zeta <- theta[9]
eta <- theta[10]

args$probs <- lapply(args$pair, function(x) {
  i <- as.character(x[[1]][1])
  s <- as.numeric(x[[2]][1])
  return(WSpred(i, s, b_all, b_rest, b_in_out, alpha, beta, gamma, delta, epsilon, zeta, eta, regiones))
})

args$dev <- mapply(function(x,y) log(dmultinom(x, prob = y)), args$freq, args$probs)
