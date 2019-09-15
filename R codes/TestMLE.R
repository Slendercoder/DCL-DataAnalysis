source("WSpred.R")
source("getFrequencies_test.R")

runOptim2 <- function(size) {
  
#  size <- 200
#  f <- paste('../Python codes/output_', as.character(size), ".csv", sep="")
#  data = read.csv(f)
#  head(data)
  
  f <- paste("frequencies_test_", as.character(size), ".csv", sep="")
#  df1 <- data
#  getFreqs(df1, f)
  a <- read.csv(f)
  head(a)
  
  args <- getArgs(a)
  head(args)
  
  # To search for best parameters WSLS model
  w1 <- 0.03 # bias FOCAL
  w2 <- 110 # win stay 
  fitresWSLS <- nmkb(par=c(w1, w2),
                     fn = function(theta) WSutil(c(theta, 500, 0.98, 0, 0, 0, 0), args, regions),
                     lower=c(0,
                             0),
                     upper=c(0.075,
                             150),
                     control=list(trace=0))
  
  pars <- fitresWSLS$par
  errorPercBias <- abs(pars[1] - 0.05)/0.05
  errorPercAlpha <- abs(pars[2] - 150)/150
  dev <- fitresWSLS$value
  
  return(c(size, pars, errorPercBias, errorPercAlpha, dev))
}

a <- runOptim2(50)
b <- runOptim2(100)
c <- runOptim2(200)
d <- runOptim2(300)
a
b
c
d
