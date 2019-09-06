source('ClassifyRegions.R')

specify_decimal3 <- function(x) trimws(format(round(x, 3), nsmall=3))
imprimir <- function(x) print(as.numeric(unlist(lapply(x, specify_decimal3))))

sigmoid <- function(x, beta, gamma) {
  # Returns the value of the sigmoid function 1/(1+exp(b(x-c)))
  
  return(1 / (1 + exp(-beta * (x - gamma))))
}

FRAWSpred <- function(i, s, j, w, alpha, beta, gamma, delta, epsilon, zeta, eta, regions){
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
  
  # Consider distance from j to complementary k and augment attractiveness
  jV <- code2Vector(j)
#  print("jV")
#  print(jV)
  kVcompVector <- lapply(regions[3:9], function(k) {
    index <- which(regions == k)
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

  # Consider distance from i to each focal region k and augment attractiveness
  index <- which(regions == i) # WARNING: REGIONS COLLAPSED!
  if (index != 1) {
    iV <- regionsCoded[index - 1]
#    print("iV")
#    print(iV)
    iV <- code2Vector(iV)
    kVector <- lapply(regions[2:9], function(k) {
      index1 <- which(regions == k)
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
#  print('Final attractiveness:')
#  imprimir(attractiveness)
  
  
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

#i <- 'LEFT'
#s <- 16
#j <- 'GHIJOPQRWXYZ4567'
#p <- FRAWSpred(i,s, j, 0.03, 150, 500, 0.98, 120, 1, 2, 1.2, regions)
#imprimir(p)
