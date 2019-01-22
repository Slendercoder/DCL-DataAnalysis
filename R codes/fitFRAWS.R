source("FRAWSpred.R")
library(dfoptim)

# USING scoreLevel
getFreq <- function(i, s, j, df) {
  # Obtains the requencies vector for each score s and overlapping region j
  # Input: i, which is the region the player is in
  #        s, which is the player's scoreLevel on the round
  #        j, the overlapping region from players' strategies
  #        df, the dataframe from which the observations are obtained
  # Output: Frequency vector of length 9
  
  regs <- df[which(df$Region == i), ]
  scores <- regs[which(regs$scoreLevel == s), ]
  regsGo <- scores$RegionGo[which(scores$RJcode == j)]
  auxDF <- t(as.data.frame(table(regsGo)))
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

data = read.csv("frequencies.csv")
head(data)

# Prepare dataFrame with frequencies
regions <- unique(data$Region)
scores <- unique(data$scoreLevel)
joints <- unique(data$RJcode)

# Create all combinations of regions, scores and joints
args <- as.data.frame(expand.grid(i = regions, s = scores, j = joints))
args$pair <- apply(args, 1, function(x) list(as.character(x[1]), as.numeric(x[2]), as.character(x[3])))
#length(args$pair)
#  of the fifth row, args$pair[5][[1]][1] is the region, args$pair[5][[1]][2] is the score

# Get the frequencies for each triple
args$freq <- lapply(args$pair, function(x) {
  i <- as.character(x[[1]][1])
  s <- as.numeric(x[[2]][1])
  j <- as.character(x[[3]][1])
  return(getFreq(i,s,j,data))
})

#getFreq('RS', 2, 'rBCHIJQRS', data)
#head(args)

# A function to get deviance from WSLS and FRA models
FRAWSutil <- function(theta, args, regions){
  # Input: theta, parameter vector of length 5
  #        data, the dataframe from which frequencies are obtained
  # Output: Deviance of WSLSpred for all regions and scores
  
#  if (any(is.na(theta))) {
#    print('Incorrect parameters: ')
#    print(theta)
#    return(10000)
#  }
  
  w <- theta[1:4]
  alpha <- theta[5]
  beta <- theta[6]
  gamma <- theta[7]
  delta <- theta[8]
  epsilon <- theta[9]
  zeta <- theta[10]
  
  # Calculate the probabilities based on FRAWSpred
#  print('Calculating probabilities')
  args$probs <- lapply(args$pair, function(x) {
    i <- as.character(x[[1]][1])
    sl <- as.numeric(x[[2]][1])
    # Converting score level into score
    if (sl == 1) {s <- 0}
    else if (sl == 2) {s <- 20}
    else {s <- 32}
    j <- as.character(x[[3]][1])
    return(FRAWSpred(i, s, j, w, alpha, beta, gamma, delta, epsilon, zeta, regions))
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

regions <- c('RS',
             'ALL', 
             'NOTHING', 
             'DOWN', 
             'UP', 
             'LEFT', 
             'RIGHT', 
             'IN', 
             'OUT')


# To search for best parameters WSLS model
w1 <- 0.11 # bias 'ALL'
w2 <- 0.05 # bias 'NOTHING'
w3 <- 0.05 # bias 'DOWN', 'UP', 'LEFT', 'RIGHT'
w4 <- 0.01 # bias 'IN', 'OUT'
w5 <- 49 # win stay 
w6 <- 399 # steepness sigmoid
w7 <- 0.92 # thresshold sigmoid
w8 <- 0.000001 # attraction similarity
w9 <- 0.000001 # exponential similarity
w10 <- 0.000001 # stubborness
margen1 <- 0.000001
margen2 <- 0.01
margen3 <- 0.05
margen4 <- 5
fitresWSLS <- nmkb(par=c(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10),
               fn = function(theta) FRAWSutil(theta, args, regions),
               lower=c(w1 - margen3,
                       w2 - margen3,
                       w3 - margen3,
                       w4 - margen2,
                       w5 - margen4,
                       w6 - margen4,
                       w7 - margen2,
                       w8 - margen1,
                       w9 - margen1,
                       w10 - margen1),
               upper=c(w1 + margen3,
                       w2 + margen3,
                       w3 + margen3,
                       w4 + margen2,
                       w5 + margen4,
                       w6 + margen4,
                       w7 + margen2,
                       w8 + margen1,
                       w9 + margen1,
                       w10 + margen1),
               control=list(trace=0))

print(fitresWSLS$par) 
print(fitresWSLS$value) 

theta <- c(0.14, 0.0674, 0.0123, 0.0009, 39, 405, 0.933, 0, 0, 0)
FRAWSutil(theta, args, regions) # 3011


# To search for best parameters FRA model
w1 <- 0.07 # bias 'ALL'
w2 <- 0.03 # bias 'NOTHING'
w3 <- 0.000001 # bias 'DOWN', 'UP', 'LEFT', 'RIGHT'
w4 <- 0.000001 # bias 'IN', 'OUT'
w5 <- 50 # win stay 
w6 <- 400 # steepness sigmoid
w7 <- 0.97 # thresshold sigmoid
w8 <- 3 # attraction similarity
w9 <- 0.96 # exponential similarity
w10 <- 5 # stubborness
margen1 <- 0.000001
margen2 <- 0.02
margen3 <- 0.05
margen4 <- 0.1
margen5 <- 2
fitresFRA <- nmkb(par=c(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10),
               fn = function(theta) FRAWSutil(theta, args, regions),
               lower=c(w1 - margen2,
                       w2 - margen2,
                       0,
                       0,
                       w5 - margen5,
                       w6 - margen5,
                       w7 - margen2,
                       w8 - margen5,
                       w9 - margen2,
                       w10 - margen5),
               upper=c(w1 + margen2,
                       w2 + margen2,
                       w3 + margen1,
                       w4 + margen1,
                       w5 + margen5,
                       w6 + margen5,
                       w7 + margen2,
                       w8 + margen5,
                       w9 + margen2,
                       w10 + margen5),
               control=list(trace=0))

print(fitresFRA$par) 
print(fitresFRA$value) 

theta <- c(0.077, 0.048, 0, 0, 48, 402, 0.99, 1.57, 0.94, 3)
FRAWSutil(theta, args, regions) # 2709
