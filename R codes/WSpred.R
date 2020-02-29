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
              'DOWN', 
              'UP', 
              'LEFT', 
              'RIGHT', 
              'IN', 
              'OUT')

lowerEps2=.00001
highEps2 =.99999

lower_limits=c(0,0,0,0,0,400,0)
upper_limits=c(0.1,0.1,0.1,0.1,500,1000,32)

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

sigmoid <- function(x, beta, gamma) {
  # Returns the value of the sigmoid function 1/(1+exp(b(x-c)))
  
  return(1 / (1 + exp(-beta * (x - gamma))))
}

obtainFreqVector <- function(x) {
  a <- data.frame(table(x))
  return(list(a$Freq))
  
}

getFreq <- function(df, theta) {
  
  df <- df[complete.cases(df), ]
  df$Region <- df$Category
  df <- df[c('Region', 'Score', 'RegionGo')]
  df$RegionGo <- factor(df$RegionGo, levels = regiones)
  df <- df %>%
    dplyr::group_by(Region, Score) %>%
    dplyr::summarize(Freqs = obtainFreqVector(RegionGo))
  
#  print(df$Region)
#  df <- df[complete.cases(df), ]
  df$freqs <- lapply(df$Freqs, function(x) {
    x1 <- x[1]
    x2 <- x[2]
    x3 <- x[3]
    x4 <- x[4]
    x5 <- x[5]
    x6 <- x[6]
    x7 <- x[7]
    x8 <- x[8]
    x9 <- x[9]
    return (c(x1, x2, x3, x4, x5, x6, x7, x8, x9))
  })
  
  return (df[c('Region', 'Score', 'freqs')])
} 

getRelFreq_Rows <- function(df) {
  # Obtains the relative frequencies for transition from region i and score s to region k
  # Input: k, the region the player is going to
  #        df, the dataframe from which the observations are obtained
  # Output: Relative frequency
  df <- df[complete.cases(df), ]
  df$Region <- df$Category
  df <- df[df$RegionGo != "", ]
  df <- df %>% select('Region', 'Score', 'RegionGo')
  df <- df %>%
    dplyr::group_by(Region, Score, RegionGo) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Region, Score) %>%
    dplyr::mutate(n1 = sum(n),
                  Freqs = n/n1)  
  return(df[c('Region', 'Score', 'RegionGo', 'Freqs')])
}

getRelFreq_k <- function(k, df) {
  # Obtains the relative frequencies for transition from region i and score s to region k
  # Input: k, the region the player is going to
  #        df, the dataframe from which the observations are obtained
  # Output: Relative frequency
  df <- df[df2$RegionGo != "", ]
  df <- df %>% select('Region', 'Score', 'RegionGo')
  df <- df %>%
    dplyr::group_by(Region, Score, RegionGo) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Region, Score) %>%
    dplyr::mutate(n1 = sum(n),
                  Freqs = n/n1)  
  
  df <- df[df$RegionGo == k, ]
  return(df)
}

WSpred <- function(i, s, theta){
  
  wAll <- theta[1]
  wNoth <- theta[2]
  wLef <- theta[3]
  wIn <- theta[4]
  alpha <- theta[5]
  beta <- theta[6]
  gamma <- theta[7]

  aux <- c(wAll, wNoth, wLef, wLef, wLef, wLef, wIn, wIn)
  #  aux <- rep(w, 8)
  # The probability of region 'RS' is 1 - the sum of the other probabilities
  if (sum(aux) > 1) {
    print('Oops, incorrect biases')
    aux <- aux/sum(aux)
  }
  bias <- c(1 - sum(aux), aux)
  #  print("bias")
  #  imprimir(bias)
  
#  n <- (s + 128) / 160 # Normalizing score
  n <- s
    
  # Find the attractivenes:
  attractiveness <- bias # Start from bias
  
  # Add attractiveness to current region according to score
  if (i != 'RS') {
    index <- which(regiones == i)
    attractiveness[index] <- attractiveness[index] + alpha * sigmoid(n, beta, gamma) 
  }
  
  probs <- attractiveness / sum(attractiveness)
  probs <- replace(probs,probs<lowerEps2,lowerEps2)
  probs <- replace(probs,probs>highEps2,highEps2)
  
  return(probs)
}

# A function to get deviance from WSLS and FRA models
WSutil <- function(theta, args){
  # Input: theta, parameter vector of length 7
  #        args, the dataframe with frequencies
  # Output: Deviance of WSLSpred
  
  if (any(is.na(theta))) {
    print('Incorrect parameters: ')
    print(theta)
    return(10000)
  }
  
  # Calculate the probabilities based on WSpred
  # print('Calculating probabilities')
  args$probs <- mapply(function(i,s) {
    return(list(WSpred(i, s, theta)))
    # return(WSpredSL(i, s, w, alpha, beta, gamma, delta, epsilon, zeta, eta, regiones))
  },
  args$Region, args$Score)

  # Calculate deviance
  #  print('Calculating deviances')
  args$dev <- mapply(function(x,y) {
    x <- unlist(x)
    y <- unlist(y)
    log(dmultinom(x, prob = y))
    }, args$freqs, args$probs)
  
#  print(args$dev)
  
  if (any(is.infinite(args$dev) | is.na(args$dev))) {
#    print('Incorrect dev: ')
#    new_DF <- args[is.infinite(args$dev), ]
#    print(new_DF)
#    print(theta)
#    print(head(args$probs))
#    print(head(args$freq))
#    print(head(args$dev))
    return(10000)
  }
  
  return(-2*sum(args$dev))
}

random_params <- function(x1, x2, x3, x4, x5, x6, x7) {
  
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
  
  x5 <- unlist(x5)
  x5 <- c(1, x5)
#  print(x5)
  a5 <- do.call(runif, as.list(x5))
  
  x6 <- unlist(x6)
  x6 <- c(1, x6)
#  print(x6)
  a6 <- do.call(runif, as.list(x6))
  
  x7 <- unlist(x7)
  x7 <- c(1, x7)
#  print(x7)
  a7 <- do.call(runif, as.list(x7))
  
  return(c(a1, a2, a3, a4, a5, a6, a7))
   
}

searchFit <- function(params, args) {
  
  fitresWSLS <- nmkb(par=params,
                     fn = function(t) WSutil(t, args),
                     lower=lower_limits,
                     upper=upper_limits,
                     control=list(trace=0))

  return(fitresWSLS)  

}

searchBestFit <- function(args, N) {
  
  best <- 100000
  
  for (n in rep(0, N)) {
    params <- random_params(list(lower_limits[1], upper_limits[1]), 
                            list(lower_limits[2], upper_limits[2]), 
                            list(lower_limits[3], upper_limits[3]), 
                            list(lower_limits[4], upper_limits[4]), 
                            list(lower_limits[5], upper_limits[5]), 
                            list(lower_limits[6], upper_limits[6]), 
                            list(lower_limits[7], upper_limits[7]))
    
    bestFit <- searchFit(params, args)
    if (bestFit$value < best) {
      fitWS <- bestFit
      best <- bestFit$value
    }
  }
  
  return(fitWS)
  
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
