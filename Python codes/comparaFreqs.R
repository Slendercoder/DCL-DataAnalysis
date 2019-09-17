imprimir <- function(x) print(as.numeric(unlist(lapply(x, specify_decimal3))))

regiones <- c('RS',
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
  #        j, the overlapping region from players' strategies
  #        df, the dataframe from which the observations are obtained
  # Output: Frequency vector of length 9
  
  regs <- df[which(df$Region == i), ]
  regsGo <- regs$RegionGo[which(regs$Score == s)]
  regsGo <- as.factor(regsGo)
  levels(regsGo) <- regiones
#  print(regsGo)
  auxDF <- t(as.data.frame(table(regsGo)))
#  print(auxDF)
  colnames(auxDF) <- as.character(unlist(auxDF[1, ])) # the first row will be the header
  auxDF <- auxDF[-1, ]          # removing the first row.
  auxDF <- auxDF[regiones]
  return(as.numeric(auxDF[1:9]))
}

WSpred <- function(i, s, w, alpha, beta, gamma, delta, epsilon, zeta, eta, regions){
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
  attractiveness[index] <- attractiveness[index] + alpha * sigmoid(n, beta, gamma) 
  
  probs <- attractiveness / sum(attractiveness)
  return(probs)
}

data = read.csv("fileFreqs.csv")
head(data)

regions <- unique(data$Region)
scores <- unique(data$Score)

# Create all combinations of regions, scores and joints
args <- as.data.frame(expand.grid(i = regions, s = scores))
args$pair <- apply(args, 1, function(x) list(as.character(x[1]), as.numeric(x[2])))
head(args)

# Get the frequencies for each triple
args$freq <- lapply(args$pair, function(x) {
  i <- as.character(x[[1]][1])
  s <- as.numeric(x[[2]][1])
  a <- getFreq(i,s,data)
  return(a)
})

head(args)

# Get relative frequencies
args$sumFreq <- lapply(args$freq, function(x) {
  a <- sum(x)
  return(a)
})

args$relFreq <- mapply(function(x, y) {
  if (y==0) {
    return(list(rep(0, 9)))
  } else {
    return(list(imprimir(1/y*x)))
  }
}, args$freq, args$sumFreq)

args$probs <- lapply(args$pair, function(x) {
  i <- as.character(x[[1]][1])
  s <- as.numeric(x[[2]][1])
  return(imprimir(WSpred(i, s, 0.05, 150, 500, 0.98, 0, 0, 0, 0, regiones)))
})

args <- args[c('pair', 'relFreq', 'probs')]

head(args)
