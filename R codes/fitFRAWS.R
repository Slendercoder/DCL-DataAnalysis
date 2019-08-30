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
head(args)
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
#  print('Calculating probabilities')
  args$probs <- lapply(args$pair, function(x) {
    i <- as.character(x[[1]][1])
    sl <- as.numeric(x[[2]][1])
    # Converting score level into score
    if (sl == 1) {s <- 0}
    else if (sl == 2) {s <- 20}
    else {s <- 32}
    j <- as.character(x[[3]][1])
    return(FRAWSpred(i, s, j, w, alpha, beta, gamma, delta, epsilon, zeta, eta, regions))
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
w1 <- 0.05 # bias FOCAL
w2 <- 150 # win stay 
fitresWSLS <- nmkb(par=c(w1, w2),
               fn = function(theta) FRAWSutil(c(theta, 500, 0.98, 0, 0, 0, 0), args, regions),
               lower=c(w1 - 0.05,
                       w2 - 10),
               upper=c(w1 + 0.05,
                       w2 + 10),
               control=list(trace=0))

print(fitresWSLS$par) 
print(fitresWSLS$value) 

theta <- c(0.05, 150, 500, 0.98, 0, 0, 0, 0)
FRAWSutil(theta, args, regions) # 3011


# To search for best parameters FRA model
w1 <- 0.00001 # bias FOCAL
w2 <- 150 # win stay 
w3 <- 3 # attraction similarity to complement
w4 <- 5 # stubborness
w5 <- 1 # eta
fitresFRA <- nmkb(par=c(w1, w2, w3, w4, w5),
               fn = function(theta) FRAWSutil(c(theta[1], 
                                                theta[2],
                                                500,
                                                0.98,
                                                theta[3],
                                                0.3,
                                                theta[4],
                                                theta[5]), 
                                              args, regions),
               lower=c(0,
                       w2 - 10,
                       w3 - 2,
                       w4 - 5,
                       w5 - 0.04),
               upper=c(w1 + 0.02,
                       w2 + 10,
                       w3 + 2,
                       w4 + 5,
                       w5 + 0.04),
               control=list(trace=0))

print(fitresFRA$par) 
print(fitresFRA$value) 

theta <- c(0.076, 0.05, 0, 0, 48, 398, 0.99, 1.53, 0.94, 3, 1.1)
FRAWSutil(theta, args, regions) # 2709
