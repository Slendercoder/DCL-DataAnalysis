letras <- 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;:'
letras <- strsplit(letras, split = '')
letras <- letras[[1]]

regions <- c('RS',
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

code2Vector <- function(cadena) {
  # Returns the vector of 0s and 1s from a given code
  # Input: cadena, which is a string with the code in characters
  # Output: vector of 0s and 1s of lenght 64
  
  v <- rep(0, length(letras))
  cadena <- strsplit(cadena, split = '')
  cadena <- cadena[[1]]
  for (i in 1:length(cadena)) {
    index <- which(letras == cadena[i])[1]
    v[index] <- 1
  }
  return(v)
}

simil <- function(k, i, o) {
  # Returns similarity between regions k and i
  # Input: k, which is a region coded as a vector of 0s and 1s of length 64
  #        i, which is a region coded as a vector of 0s and 1s of length 64
  #        o, which is a parameter for the exponential
  # Output: number representing the similarity between k and i
  
  distance <- sqrt(sum((k - i) * (k - i)))
  return(exp(- o * distance))
}

classifyRegion <- function(r, o, d) {
  # Returns the code of the region that is closer to r
  # Input: r, region coded as a vector of 0s and 1s of length 64
  #        o, which is a parameter for the similarity function
  #        d, which is a parameter for threshold
  # Output: string, which is a region code
  
  similarities <- rep(0, 8)
  contador <- 0
  
  for (k in regionsCoded) {
    kV <- code2Vector(k)
    contador <- contador + 1
    similarities[contador] <- simil(r, kV, o)
  }
#  print('similarities')
#  print(similarities)
  if (any(similarities > d)) {
    probs <- similarities/sum(similarities)
#    print('probs')
#    print(probs)
    chosen <- regions[2:9][which(probs == max(probs))]
    return(chosen)
  } else {
    return('RS')
  }
}

#j <- 'abcdijklqrstyzABGHIJOPQRWXYZ'
#jV <- code2Vector(j)
#classifyRegion(jV, 0.3, 0.55)
