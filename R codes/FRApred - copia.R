
###########################
# Global variables
###########################

letras <- 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;:'
letras <- strsplit(letras, split = '')
letras <- letras[[1]]

regiones <- c('RS',
             'ALL', 
             'NOTHING', 
             'DOWN', 
             'UP', 
             'LEFT', 
             'RIGHT', 
             'IN', 
             'OUT')

regionsCoded <- c('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;:', # ALL
                  '', # NOTHING
                  'GHIJKLMNOPQRSTUVWXYZ0123456789;:', # DOWN
                  'abcdefghijklmnopqrstuvwxyzABCDEF', # UP
                  'abcdijklqrstyzABGHIJOPQRWXYZ4567', # LEFT
                  'efghmnopuvwxCDEFKLMNSTUV012389;:', # RIGHT
                  'jklmnorstuvwzABCDEHIJKLMPQRSTUXYZ012', # IN
                  'abcdefghipqxyFGNOVW3456789;:') # OUT

###########################
# Define functions
###########################

classifyCode <- function(cadena) {
  # Returns the code of a given lettercode
  # Input: cadena,  lettercode of a region
  # Output: name of the region

  index <- which(regionsCoded == cadena)
#  print(index)
  if (length(index) == 0) {
    return ('RS')
  } else {
  return(regiones[index + 1])
  }
} # end classifyCode

code2Vector <- function(cadena) {
  # Returns the vector of 0s and 1s from a given code
  # Input: cadena, a string with the code in characters
  # Output: vector of 0s and 1s of lenght 64
  
  v <- rep(0, length(letras))
  cadena <- strsplit(cadena, split = '')
  cadena <- cadena[[1]]
  for (i in 1:length(cadena)) {
    index <- which(letras == cadena[i])[1]
    v[index] <- 1
  }
  return(v)
} # end code2Vector

specify_decimal3 <- function(x) trimws(format(round(x, 3), nsmall=3))
imprimir <- function(x) print(as.numeric(unlist(lapply(x, specify_decimal3))))

sigmoid <- function(x, beta, gamma) {
  # Returns the value of the sigmoid function 1/(1+exp(b(x-c)))
  
  return(1 / (1 + exp(-beta * (x - gamma))))
} # end sigmoid

getFreq <- function(i, iV, s, j, df) {
  # Obtains the frequencies vector for a given region, score s and overlapping region j
  # Input: i, the name of the region the player is in
  #        iV (lettercode), the region the player is in
  #        s, which is the player's scoreLevel on the round
  #        j (lettercode), the overlapping region from players' strategies
  #        df, the dataframe from which the observations are obtained
  # Output: Frequency vector of length 9
  
  #  print(regiones)
  df$RegionGo <- factor(df$RegionGo, levels = regiones, ordered = TRUE)
  #  print(levels(df$RegionGo))
  
  regs <- df[which(df$RegionCoded == iV), ]
#  print('Considering region', i)
  scores <- regs[which(regs$Score == s), ]
#  print('Considering score', s)
  regsGo <- scores$RegionGo[which(scores$JointRegion == j)]
  auxDF <- t(as.data.frame(table(regsGo)))
  colnames(auxDF) <- as.character(unlist(auxDF[1, ])) # the first row will be the header
  auxDF <- auxDF[-1, ]          # removing the first row.
  auxDF <- auxDF[regiones]
  return(as.numeric(auxDF[1:9]))
} # end getFreq

getArgs <- function(data, regiones) {
  # Obtains the dataframe with the frequencies vector 
  # Input: data, dataFrame from which to
  #        regiones, the vector with the regions names
  # Output: dataFrame

  # Prepare dataFrame with frequencies
  regs <- unique(data$RegionCoded)
  scores <- unique(data$Score)
  joints <- unique(data$JointRegion)
#  print('joints')
#  print(joints)
  
  # Create all combinations of regions, scores and joints
  cat('\nCreating cuadruples ', 
      length(regs), 'x',
      length(scores), 'x',
      length(joints), '\n')
  args <- as.data.frame(expand.grid(iV = regs, s = scores, j = joints))
  args$i <- lapply(args$iV, classifyCode)
  print(args[1:5, ])
  args$cuadruple <- apply(args, 1, function(x) list(classifyCode(as.character(x[1])),
                                                 as.character(x[1]), 
                                                 as.numeric(x[2]), 
                                                 as.character(x[3])))
  print('Number of rows:')
  print(length(args$cuadruple))

  # Get the frequencies for each cuadruple
  print('Finding frequencies (please be patient!)...')
  args$freq <- lapply(args$cuadruple, function(x) {
    i <- as.character(x[[1]][1])
    iV <- as.character(x[[2]][1])
    s <- as.numeric(x[[3]][1])
    j <- as.character(x[[4]][1])
#    cat('\ni', i, 'iV', iV, 's', s, 'j', j)
    return(getFreq(i, iV, s, j, data))
  })

  # Get the sum of frequencies for each pair
  print('Finding sum of frequencies...')
  args$sumFreq <- lapply(args$freq, function(x) {
    return(sum(x))
  })
  
  args <- args[args$sumFreq > 0, ]
  print('Done!')
  return(args)
} # end getArgs

sim_consist <- function(v1, v2){
  # Returns the similarity based on consistency
  # v1 and v2 are two 64-bit coded regions
  
  joint <- v1 * v2
  union <- (v1 + v2) / (v1 + v2)
  union[is.na(union)] <- 0
  j = sum(joint)
  u = sum(union)
  if (u != 0) {
    return (j/u)
  } else {
    return (1)
  }
} # end sim_consist

FRApred <- function(i, iV, s, j, 
                    wALL, wNOTHING, wLEFT, wIN,
                    alpha, beta, gamma, 
                    delta, epsilon, zeta, 
                    regiones){
  # Returns the transition probability vector
  # Each position in the vector represents a region
  # and the value represents the probability of going to that region
  # Input: i, the name of the region the player is in
  #        iV (lettercode), the region the player is in
  #        s, the player's score on the round
  #        j  (lettercode), the overlapping region
  #        wALL, the prior probability of choosing ALL region
  #        wNOTHING, the prior probability of choosing NOTHING region
  #        wLEFT, the prior probability of choosing LEFT, RIGHT, TOP, BOTTOM regions
  #        wIN, the prior probability of choosing IN, OUT regions
  #        alpha, how much Win makes the player Stay
  #        beta, the wideness of the WSLS sigmoid
  #        gamma, the position of the threshold in the WSLS sigmoid
  #        delta, how much FRASim attracts the player
  #        epsilon, the wideness of the FRA sigmoid
  #        zeta, the position of the threshold in the FRA sigmoid

#  if (any(is.na(c(w, alpha, beta, gamma, delta, epsilon, zeta, eta)))) {
#    print('Incorrect parameters on FRAWSpred: ')
#    print(c(w, alpha, beta, gamma, delta, epsilon, zeta, eta))
#  }

  # First we calculate the prior probabilities
  aux <- c(wALL, wNOTHING, wLEFT, wLEFT, wLEFT, wLEFT, wIN, wIN)
  # The probability of region 'RS' is 1 - the sum of the other probabilities
  if (sum(aux) > 1) {
    aux <- aux/sum(aux)
  }
  bias <- c(1 - sum(aux), aux)
#  print("bias")
#  imprimir(bias)
  
  # Start from biases
  attractiveness <- bias

  # Add WinStay
  index <- which(regiones == i)
  # print('i')
  # adding win stay only to focal regions
  if (i != 'RS') {
    attractiveness[index] <- attractiveness[index] + alpha * sigmoid(s, beta, gamma) 
  }
#  print('Attractiveness with WS:')
#  imprimir(attractiveness)

  # Calculating similarity to region
  # Consider distance from i to each focal region k and augment attractiveness
  kVector <- lapply(regiones[2:9], function(k) {
    index1 <- which(regiones == k)
    kCoded <- regionsCoded[index1 - 1] # regionsCoded does not have 'RS'
    kV <- code2Vector(kCoded)
  })
  similarities <- lapply(kVector, function(x) {
    sim_consist(code2Vector(iV), x) 
  })
  simil1 <- as.numeric(unlist(similarities))
  simil1 <- c(0, simil1)
#  print('Similarity to region:')
#  imprimir(simil1)

  # Consider distance from j to complementary k
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
  simil2 <- lapply(kVcompVector, function(x) {
    sim_consist(jV, x) 
  })
  simil2 <- as.numeric(unlist(simil2))
  simil2 <- c(0, 0, simil2)
#  print('Similarity to complement:')
#  imprimir(simil2)
  
  simils <- simil1 + simil2 
#  print('FRA Similarity:')
#  imprimir(simils)
  
  attractiveness <- attractiveness + delta * sigmoid(simils, epsilon, zeta) 
#  print('Attractiveness with FRA similarity:')
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
} # end FRApred

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
  
  # Calculate the probabilities based on FRAWSpred
  #  print('Calculating probabilities')
  args$probs <- lapply(args$cuadruple, function(x) {
    i <- as.character(x[[1]][1])
    iV <- as.character(x[[2]][1])
    s <- as.numeric(x[[3]][1])
    j <- as.character(x[[4]][1])
    return(FRApred(i, iV, s, j,
                   wAll, wNoth, wLef, wIn,
                   alpha, beta, gamma, 
                   delta, epsilon, zeta, 
                   regiones))
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
} # end FRAutil

#df1 = read.csv("../Python Codes/freqs4FRA.csv")
#head(df1)

#args <- getArgs(df1, regions)
#head(args)

#i <- as.character(df1[2:2, 4])
#iV <- as.character(df1[2:2, 5])
#s <- as.numeric(df1[2:2, 7])
#j <- as.character(df1[2:2, 8])
#p <- FRApred(i, iV, s, j, 0.1, 0.1, 0.1, 0.1, 200, 500, 32, 200, 500, 0.7, regiones)
#imprimir(p)
#theta <- c(0.1, 0.1, 0.1, 0.1, 200, 500, 32, 200, 500, 0.7)
#dev <- FRAutil(theta, args, regiones)
#dev
