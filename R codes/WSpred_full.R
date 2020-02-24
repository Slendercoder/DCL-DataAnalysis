library(dplyr)

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

####################################################################################
# Functions
####################################################################################

specify_decimal3 <- function(x) trimws(format(round(x, 3), nsmall=3))
imprimir <- function(x) print(as.numeric(unlist(lapply(x, specify_decimal3))))

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
  
  return (df) #[c('Region', 'Score', 'freqs')])
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
  # Input: theta, parameter vector of length 11
  #        data, the dataframe from which frequencies are obtained
  # Output: Deviance of WSLSpred for all regions and scores
  
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
    }, args$Freqs, args$probs)
  
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

WSutil1 <- function(a, b){

  theta <- c(a, b, 10, 31, 0, 0, 0, 0)
  return(WSutil(theta, args, regiones))

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
