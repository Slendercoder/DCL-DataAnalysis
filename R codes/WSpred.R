library(dplyr)

specify_decimal3 <- function(x) trimws(format(round(x, 3), nsmall=3))
imprimir <- function(x) print(as.numeric(unlist(lapply(x, specify_decimal3))))

sigmoid <- function(x, beta, gamma) {
  # Returns the value of the sigmoid function 1/(1+exp(b(x-c)))
  
  return(1 / (1 + exp(-beta * (x - gamma))))
}

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

getFreq <- function(i, s, df, regiones) {
  # Obtains the frequencies vector for each starting region region i and score s 
  # Input: i, which is the region the player is in
  #        s, which is the player's score obtained on the previous round
  #        df, the dataframe from which the observations are obtained
  #        regions, which is the vector with the regions names
  # Output: Frequency vector of length 9
  
  df$RegionGo <- factor(df$RegionGo, levels = regiones, ordered = TRUE)
  
  regs <- df[which(df$Region == i), ]
  regsGo <- regs$RegionGo[which(regs$Score == s)]
  #  print(regsGo)
  auxDF <- t(as.data.frame(table(regsGo)))
  #  print(auxDF)
  colnames(auxDF) <- as.character(unlist(auxDF[1, ])) # the first row will be the header
  auxDF <- auxDF[-1, ]          # removing the first row.
  auxDF <- auxDF[regiones]
  return(as.numeric(auxDF[1:9]))
}

getArgs <- function(data) {
  # Prepare dataFrame with frequencies
#  regions <- unique(data$Region)
  scores <- unique(data$Score)
  
  # Create all combinations of regions, scores and joints
  args <- as.data.frame(expand.grid(i = regiones, s = scores))
  head(args)
  args$pair <- apply(args, 1, function(x) list(as.character(x[1]), as.numeric(x[2])))
  #length(args$pair)
  #  of the fifth row, args$pair[5][[1]][1] is the region, args$pair[5][[1]][2] is the score
  
  # Get the frequencies for each pair
  args$freq <- lapply(args$pair, function(x) {
    i <- as.character(x[[1]][1])
    s <- as.numeric(x[[2]][1])
    return(getFreq(i,s,data, regiones))
  })
  
  # Get the sum of frequencies for each pair
  args$sumFreq <- lapply(args$freq, function(x) {
    return(sum(x))
  })
  
  args <- args[args$sumFreq > 0, ]
  
  return(args)
}

WSpred <- function(i, s, w, alpha, beta, gamma, delta, epsilon, zeta, eta, regiones){
  aux <- c(0.1, 0.15, 0.1, 0.1, 0.09, 0.09, 0.01, 0.01)*w
#  aux <- rep(w, 8)
  # The probability of region 'RS' is 1 - the sum of the other probabilities
  if (sum(aux) > 1) {
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

WSpredSL <- function(i, s, w, alpha, beta, gamma, delta, epsilon, zeta, eta, regiones){
  aux <- c(0.1, 0.15, 0.1, 0.1, 0.09, 0.09, 0.01, 0.01)*w
  #  aux <- rep(w, 8)
  # The probability of region 'RS' is 1 - the sum of the other probabilities
  if (sum(aux) > 1) {
    aux <- aux/sum(aux)
  }
  bias <- c(1 - sum(aux), aux)
  #  print("bias")
  #  imprimir(bias)
  #  n <- (s + 128) / 160 # Normalizing score
  if(s == 1){
    n <- 10
  } else {
    n <- 32
  }
#  n <- s
  
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
WSutil <- function(theta, args, regiones){
  # Input: theta, parameter vector of length 11
  #        data, the dataframe from which frequencies are obtained
  # Output: Deviance of WSLSpred for all regions and scores
  
  if (any(is.na(theta))) {
    print('Incorrect parameters: ')
    print(theta)
    return(10000)
  }
  
  w <- theta[1]
  alpha <- theta[2]
  beta <- theta[3]
  gamma <- theta[4]
  delta <- theta[5]
  epsilon <- theta[6]
  zeta <- theta[7]
  eta <- theta[8]
  
  # Calculate the probabilities based on WSpred
  # print('Calculating probabilities')
  args$probs <- lapply(args$pair, function(x) {
    i <- as.character(x[[1]][1])
    s <- as.numeric(x[[2]][1])
    return(WSpred(i, s, w, alpha, beta, gamma, delta, epsilon, zeta, eta, regiones))
    # return(WSpredSL(i, s, w, alpha, beta, gamma, delta, epsilon, zeta, eta, regiones))
  })

  # Calculate deviance
  #  print('Calculating deviances')
  args$dev <- mapply(function(x,y) log(dmultinom(x, prob = y)), args$freq, args$probs)
  
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
