source('ClassifyRegions.R')

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

getArgs <- function(data, regiones) {
  # Prepare dataFrame with frequencies
  scores <- unique(data$Score)
  joints <- unique(data$RJcode)
  
  # Create all combinations of regions, scores and joints
  print('Creating triples...')
  args <- as.data.frame(expand.grid(i = regiones, s = scores, j = joints))
  head(args)
  args$pair <- apply(args, 1, function(x) list(as.character(x[1]), as.numeric(x[2]), as.character(x[3])))
  print('Number of rows:')
  print(length(args$pair))
  #  of the fifth row, args$pair[5][[1]][1] is the region, args$pair[5][[1]][2] is the score
  
  # Get the frequencies for each triple
  print('Finding frequencies...')
  args$freq <- lapply(args$pair, function(x) {
    i <- as.character(x[[1]][1])
    s <- as.numeric(x[[2]][1])
    j <- as.character(x[[3]][1])
    return(getFreq(i,s,j,data))
  })

  # Get the sum of frequencies for each pair
  print('Finding sum of frequencies...')
  args$sumFreq <- lapply(args$freq, function(x) {
    return(sum(x))
  })
  
  args <- args[args$sumFreq > 0, ]

  print('Done!')
  return(args)
  
}

FRApred <- function(i, s, j, wRS, wAll, wNoth, wLef, wIn, alpha, beta, gamma, delta, epsilon, zeta, eta, regiones){
  # Returns the transition probability vector
  # Each position in the vector represents a region
  # and the value represents the probability of going to that region
  # Input: i, which is the region the player is in
  #        j, which is the overlapping region
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
  # w[1] is the probability of 'RS'
  # w[2] is the probability of region 'ALL'
  # w[3] is the probability of region 'NOTHING'
  # w[4] is the probability of regions 'DOWN', 'UP', 'LEFT', 'RIGHT'
  # w[5] is the probability of regions 'IN', 'OUT'
#  aux <- rep(w, 8)
  aux <- c(wRS, wAll, wNoth, wLef, wLef, wLef, wLef, wIn, wIn)
#  print("bias")
#  imprimir(bias)
  
  # Find the attractivenes:
  attractiveness <- bias # Start from bias

  # Add win stay only to focal regions
  index <- which(regiones == i)
#  print('i')
#  print(regionsCoded[index - 1])
  if (i != 'RS') {
    attractiveness[index] <- attractiveness[index] + alpha * sigmoid(s, beta, gamma) 
  }
#  print('Attractiveness with WS:')
#  imprimir(attractiveness)

  # Consider distance from i to each focal region k and augment attractiveness
  index <- which(regiones == i) # WARNING: REGIONS COLLAPSED!
  if (index != 1) {
    iV <- regionsCoded[index - 1]
#    print("iV")
#    print(iV)
    iV <- code2Vector(iV)
    kVector <- lapply(regiones[2:9], function(k) {
      index1 <- which(regiones == k)
      kCoded <- regionsCoded[index1 - 1] # regionsCoded does not have 'RS'
      kV <- code2Vector(kCoded)
    })
    similarities <- lapply(kVector, function(x) {
      simil(iV, x, epsilon) 
    })
    similarities <- as.numeric(unlist(similarities))
    similarities <- c(0, similarities)
    attractiveness <- attractiveness + zeta * similarities
  }
#  print('Similarity to region:')
#  imprimir(similarities)
#  print('Attractiveness to focal regions:')
#  imprimir(attractiveness)

  # Consider distance from j to complementary k and augment attractiveness
  jV <- code2Vector(j)
  #  print("jV")
  #  print(jV)
  kVcompVector <- lapply(regiones[3:9], function(k) {
    index <- which(regiones == k)
    kCoded <- regionsCoded[index - 1] # regionsCoded does not have 'RS'
    #    print('kCoded')
    #    print(kCoded)
    kVComp <- 1 - code2Vector(kCoded)
  })
  similarity2Complement <- lapply(kVcompVector, function(x) {
    simil(jV, x, eta) 
  })
  similarity2Complement <- as.numeric(unlist(similarity2Complement))
  similarity2Complement <- c(0, 0, similarity2Complement)
  #  print('Similarity to complement:')
  #  imprimir(similarity2Complement)
  
  attractiveness <- attractiveness + delta * similarity2Complement
  #  print('Attractiveness to complement:')
  #  imprimir(attractiveness)

  probs <- attractiveness / sum(attractiveness)
  return(probs)
}

# A function to get deviance from WSLS and FRA models
FRAutil <- function(theta, args, regiones){
  # Input: theta, parameter vector of length 11
  #        data, the dataframe from which frequencies are obtained
  # Output: Deviance of WSLSpred for all regions and scores
  
  #  if (any(is.na(theta))) {
  #    print('Incorrect parameters: ')
  #    print(theta)
  #    return(10000)
  #  }
  
  wAll <- theta[1]
  wNoth <- theta[2]
  wLef <- theta[3]
  wIn <- theta[4]
  alpha <- theta[5]
  beta <- theta[6]
  gamma <- theta[7]
  delta <- theta[8]
  epsilon <- theta[9]
  zeta <- theta[10]
  eta <- theta[11]
  
  # Calculate the probabilities based on FRAWSpred
  #  print('Calculating probabilities')
  args$probs <- lapply(args$pair, function(x) {
    i <- as.character(x[[1]][1])
    s <- as.numeric(x[[2]][1])
    j <- as.character(x[[3]][1])
    return(FRApred(i, s, j, wAll, wNoth, wLef, wIn, alpha, beta, gamma, delta, epsilon, zeta, eta, regiones))
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

#i <- 'LEFT'
#s <- 16
#j <- 'GHIJOPQRWXYZ4567'
#p <- FRAWSpred(i,s, j, 0.03, 150, 500, 0.98, 120, 1, 2, 1.2, regions)
#imprimir(p)
