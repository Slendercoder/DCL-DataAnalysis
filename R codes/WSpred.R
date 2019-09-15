source('ClassifyRegions.R')

specify_decimal3 <- function(x) trimws(format(round(x, 3), nsmall=3))
imprimir <- function(x) print(as.numeric(unlist(lapply(x, specify_decimal3))))

sigmoid <- function(x, beta, gamma) {
  # Returns the value of the sigmoid function 1/(1+exp(b(x-c)))
  
  return(1 / (1 + exp(-beta * (x - gamma))))
}

WSpred <- function(i, s, w, alpha, beta, gamma, delta, epsilon, zeta, eta, regions){
  # Returns the transition probability vector
  # Each position in the vector represents a region
  # and the value represents the probability of going to that region
  # Input: i, which is the region the player is in
  #        s, which is the player's score on the round
  #        w, which is the prior probability of choosing a focal region
  #        alpha, which is a parameter for how much Win makes the player Stay
  #        beta, which is a parameter for the wideness of the sigmoid
  #        gamma, which is a parameter for the position of the threshold in the sigmoid
  #        delta, for how much the similarity to complement augments attractiveness
  #        epsilon, which is a parameter for the exponential similarity
  #        zeta, which is a parameter for the stubbornness
  #        eta, which is a parameter for the similarity to complementary focal region
  
#  if (any(is.na(c(w, alpha, beta, gamma, delta, epsilon, zeta, eta)))) {
#    print('Incorrect parameters on FRAWSpred: ')
#    print(c(w, alpha, beta, gamma, delta, epsilon, zeta, eta))
#  }

  # First we calculate the prior probabilities
  # w[1] is the probability of region 'ALL' and 'NOTHING'
  # w[2] is the probability of regions 'DOWN', 'UP', 'LEFT', 'RIGHT'
  # w[3] is the probability of regions 'IN', 'OUT'
  aux <- rep(w, 8)
  # The probability of region 'RS' is 1 - the sum of the other probabilities
  if (sum(aux) > 1) {
    aux <- aux/sum(aux)
  }
  bias <- c(1 - sum(aux), aux)
#  print("bias")
#  imprimir(bias)
  
  n <- (s + 128) / 160 # Normalizing score
  
  # Find the attractivenes:
  attractiveness <- bias # Start from bias

  # Add attractiveness to current region according to score
  index <- which(regions == i)
#  print('i')
#  print(regionsCoded[index - 1])
  # adding win stay
  attractiveness[index] <- attractiveness[index] + alpha * sigmoid(n, beta, gamma) 

#  print('Attractiveness with WS:')
#  imprimir(attractiveness)
  
#  print(attractiveness)
  
#  negative <- which(attractiveness < 0)
#  if (length(negative) > 0) {
#    print('Error: attractiveness negative')
#    print(attractiveness)
#    for (k in negative) {
#      attractiveness[k] <- 0.00001
#    }
#  }
  
  probs <- attractiveness / sum(attractiveness)
  return(probs)
}

# i <- 'LEFT'
# s <- 16
# p <- WSpred(i, s, 0.03, 150, 500, 0.98, 120, 1, 2, 1.2, regions)
# imprimir(p)

regions <- c('RS',
             'ALL', 
             'NOTHING', 
             'DOWN', 
             'UP', 
             'LEFT', 
             'RIGHT', 
             'IN', 
             'OUT')

getFreq <- function(i, s, df) {
  # Obtains the requencies vector for each score s and overlapping region j
  # Input: i, which is the region the player is in
  #        s, which is the player's scoreLevel on the round
  #        df, the dataframe from which the observations are obtained
  # Output: Frequency vector of length 9
  
  regs <- df[which(df$Region == i), ]
  scores <- regs$RegionGo[which(regs$Score == s)]
  auxDF <- t(as.data.frame(table(scores)))
  colnames(auxDF) <- as.character(unlist(auxDF[1, ])) # the first row will be the header
  auxDF <- auxDF[-1, ]          # removing the first row.
  auxDF <- auxDF[c('RS',
                   'ALL', 
                   'NOTHING', 
                   'DOWN', 
                   'UP', 
                   'LEFT', 
                   'RIGHT', 
                   'IN', 
                   'OUT')]
  return(as.numeric(auxDF[1:9]))
}

getArgs <- function(data) {
  # Prepare dataFrame with frequencies
  regions <- unique(data$Region)
  scores <- unique(data$Score)
  
  # Create all combinations of regions, scores and joints
  args <- as.data.frame(expand.grid(i = regions, s = scores))
  head(args)
  args$pair <- apply(args, 1, function(x) list(as.character(x[1]), as.numeric(x[2])))
  #length(args$pair)
  #  of the fifth row, args$pair[5][[1]][1] is the region, args$pair[5][[1]][2] is the score
  
  # Get the frequencies for each triple
  args$freq <- lapply(args$pair, function(x) {
    i <- as.character(x[[1]][1])
    s <- as.numeric(x[[2]][1])
    return(getFreq(i,s,data))
  })
  
  # Calculate the probabilities based on WSpred
  # print('Calculating probabilities')
  #args$probs <- lapply(args$pair, function(x) {
  #  i <- as.character(x[[1]][1])
  #  s <- as.numeric(x[[2]][1])
  #  return(imprimir(WSpred(i, s, 0.05, 150, 500, 0.98, 0, 0, 0, 0, regions)))
  #  return(imprimir(WSpred(i, s, 0.054, 3.5, 500, 0.98, 0, 0, 0, 0, regions)))
  #})
  
  
  #getFreq('RS', 2, 'rBCHIJQRS', data)
  #head(args)
  return(args)
}

# A function to get deviance from WSLS and FRA models
WSutil <- function(theta, args, regions){
  # Input: theta, parameter vector of length 11
  #        data, the dataframe from which frequencies are obtained
  # Output: Deviance of WSLSpred for all regions and scores
  
  #  if (any(is.na(theta))) {
  #    print('Incorrect parameters: ')
  #    print(theta)
  #    return(10000)
  #  }
  
  w <- theta[1]
  alpha <- theta[2]
  beta <- theta[3]
  gamma <- theta[4]
  delta <- theta[5]
  epsilon <- theta[6]
  zeta <- theta[7]
  eta <- theta[8]
  
  # Calculate the probabilities based on FRAWSpred
  # print('Calculating probabilities')
  args$probs <- lapply(args$pair, function(x) {
    i <- as.character(x[[1]][1])
    s <- as.numeric(x[[2]][1])
    return(WSpred(i, s, w, alpha, beta, gamma, delta, epsilon, zeta, eta, regions))
  })
  #  print(args$probs[1:6])
  
  #  if (any(is.na(args$probs))) {
  #    print('Incorrect probabilities: ')
  #    head(args$probs)
  #    return(10000)
  #  }
  
  #  args$zeros <- lapply(args$probs, function(x) {if (any(x == 0)) {return(0)} else {return(1)}})
  #  zeroProbs <- args$pair[which(args$zeros == 0)]
  #  if (length(zeroProbs) > 0) {
  #    print('Some probabilities equal 0!')
  #    print('Parameters: ')
  #    print(theta)
  #    print('Freqs')
  #    print(zeroProbs)
  #    return(10000)
  #  }
  
  # Calculate deviance
  #  print('Calculating deviances')
  args$dev <- mapply(function(x,y) log(dmultinom(x, prob = y)), args$freq, args$probs)
  
  #  if (any(is.infinite(args$dev) | is.na(args$dev))) {
  #    print('Incorrect dev: ')
  #    head(args$dev)
  #    return(10000)
  #  }
  
  return(-2*sum(args$dev))
}
