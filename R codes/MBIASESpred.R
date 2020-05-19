library(dplyr)
library(dfoptim)
library(bbmle)
library(beepr)
library(ggplot2)
library(gridExtra)
library(Rmisc)

####################################################################################
# Global variables
####################################################################################

regiones <- c('RS',
              'ALL', 
              'NOTHING', 
              'BOTTOM', 
              'TOP', 
              'LEFT', 
              'RIGHT', 
              'IN', 
              'OUT')

lowerEps2=.00001
highEps2 =.99999

lower_limits=c(0,0,0,0)
upper_limits=c(0.1,0.1,0.1,0.1)

####################################################################################
# Functions
####################################################################################

specify_decimal3 <- function(x) trimws(format(round(x, 3), nsmall=3))
imprimir <- function(x) print(as.numeric(unlist(lapply(x, specify_decimal3))))
para_visualizar <- function(theta) {
  a <- as.character(theta)
  params <- ''
  for (letra in a) {
    params <- paste(params, letra, ', ', sep = "")
  }
  return(params)
}

MBIASESpred <- function(theta){
  
  wAll <- theta[1]
  wNoth <- theta[2]
  wLef <- theta[3]
  wIn <- theta[4]

  aux <- c(wAll, wNoth, wLef, wLef, wLef, wLef, wIn, wIn)
  #  aux <- rep(w, 8)
  # The probability of region 'RS' is 1 - the sum of the other probabilities
  if (sum(aux) > 1) {
    print('Oops, incorrect biases')
    aux <- aux/sum(aux)
  }
  bias <- c(1 - sum(aux), aux)
  return(bias)
}

# A function to get deviance from MBiases
MBIASESutil <- function(theta, freqs){
  # Input: theta, parameter vector of length 7
  #        args, the dataframe with frequencies
  # Output: Deviance of MBIASESpred
  
  if (any(is.na(theta))) {
    print('Incorrect parameters: ')
    print(theta)
    return(10000)
  }
  
  # Calculate the probabilities based on WSpred
  # print('Calculating probabilities')
  probs <- MBIASESpred(theta)

  # Calculate deviance
  #  print('Calculating deviances')
  dev <- log(dmultinom(freqs, prob = probs))

  if (is.infinite(dev)) {
    return(10000)
  }
  
  return(-2*dev)
}

random_params <- function(x1, x2, x3, x4) {
  
  x1 <- unlist(x1)
  x1 <- c(1, x1)
#  print(x1)
  a1 <- do.call(runif, as.list(x1))

  x2 <- unlist(x2)
  x2 <- c(1, x2)
#  print(x2)
  a2 <- do.call(runif, as.list(x2))
  
  x3 <- unlist(x3)
  x3 <- c(1, x3)
#  print(x3)
  a3 <- do.call(runif, as.list(x3))
  
  x4 <- unlist(x4)
  x4 <- c(1, x4)
#  print(x4)
  a4 <- do.call(runif, as.list(x4))
  
  return(c(a1, a2, a3, a4))
   
}

searchFit <- function(params, freqs) {
  
  fitresMBiases <- nmkb(par=params,
                     fn = function(t) MBIASESutil(t, freqs),
                     lower=lower_limits,
                     upper=upper_limits,
                     control=list(trace=0))

  return(fitresMBiases)  

}

searchBestFit <- function(args, N) {
  
  best <- 100000
  
  for (n in rep(0, N)) {
    params <- random_params(list(lower_limits[1], upper_limits[1]), 
                            list(lower_limits[2], upper_limits[2]), 
                            list(lower_limits[3], upper_limits[3]), 
                            list(lower_limits[4], upper_limits[4]))
    
    bestFit <- searchFit(params, args)
    if (bestFit$value < best) {
      fitMBiases <- bestFit
      best <- bestFit$value
    }
  }
  
  return(fitMBiases)
  
}

Nombre_Region <- function(x) {
  if (x == '0' || x== '9') {
    return('RS')
  } else if (x == '1') {
    return('ALL')
  } else if (x == '2') {
      return('NOTHING')
  } else if (x == '3') {
    return('DOWN')
  } else if (x == '4') {
    return('UP')
  } else if (x == '5') {
    return('LEFT')
  } else if (x == '6') {
    return('RIGHT')
  } else if (x == '7') {
    return('IN')
  } else if (x == '8') {
    return('OUT')
  }
}

WSprob <- function(i, s, k, theta){
  
  probs <- WSpred(i, s, theta)
  
  probab <- probs[which(regiones == k)]
  
  return(probab)
}
