library(stats4)
library(bbmle)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(beepr)

#####################################################
# Definitions
#####################################################

# Trims a number to three decimals
specify_decimal3 <- function(x) trimws(format(round(x, 3), nsmall=3))

# Friendly prints a numeric vector
imprimir <- function(x) print(as.numeric(unlist(lapply(x, specify_decimal3))))

# Returns the value of the sigmoid function 1/(1+exp(b(x-c)))
sigmoid <- function(x, beta, gamma) {1 / (1 + exp(-beta * (x - gamma)))}

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

getRelFreq <- function(i, s, k, df) {
  # Obtains the relative frequencies for transition from region i and score s to region k
  # Input: i, which is the region the player is currently in
  #        s, which is the player's score obtained on the previous round
  #        k, which is the region the player is going to
  #        df, the dataframe from which the observations are obtained
  # Output: Relative frequency
  
  regs <- df[df$Region == i, ]
  regs <- regs[regs$Score == s, ]
  n <- dim(regs)[1]
  regs <- regs[regs$RegionGo == k, ]
  f <- dim(regs)[1]
  return(f/n)

  # auxDF <- t(as.data.frame(table(regsGo)))
  #  print(auxDF)
  # colnames(auxDF) <- as.character(unlist(auxDF[1, ])) # the first row will be the header
  # auxDF <- auxDF[-1, ]          # removing the first row.
  # auxDF <- auxDF[regiones]
  # return(as.numeric(auxDF[1:9]))
}

WSprob <- function(i, s, k, theta, regiones){
  
#  w <- theta[1]
#  alpha <- theta[2]
#  beta <- theta[3]
#  gamma <- theta[4]
#  delta <- theta[5]
#  epsilon <- theta[6]
#  zeta <- theta[7]
#  eta <- theta[8]
#  aux <- c(0.1, 0.15, 0.1, 0.1, 0.09, 0.09, 0.01, 0.01)*w

  wALL <- theta[1]
  wNOTHING <- theta[2]
  wLEFT <- theta[3]
  wIN <- theta[4]
  alpha <- theta[5]
  beta <- theta[6]
  gamma <- theta[7]
  delta <- theta[8]
  epsilon <- theta[9]
  zeta <- theta[10]
  eta <- theta[11]
  aux <- c(wALL, wNOTHING, wLEFT, wLEFT, wLEFT, wLEFT, wIN, wIN)
  
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
#  probs <- replace(probs,probs<lowerEps2,lowerEps2)
#  probs <- replace(probs,probs>highEps2,highEps2)
  
  probab <- probs[which(regiones == k)]
  
  return(probab)
}

#####################################################
# Global variables
#####################################################

# TrueVal: 
thetaTRUE <- c(0.1, 0.1, 0.05, 0.05, 150, 10, 31, 0, 0, 0, 0)
theta <- c(0.089, 0.099, 0.044, 0.042, 122.228, 160.825, 31.060, 0, 0, 0, 0) # Estimated only absent

regiones <- c('RS',
              'ALL', 
              'NOTHING', 
              'DOWN', 
              'UP', 
              'LEFT', 
              'RIGHT', 
              'IN', 
              'OUT')

###############################################################################
# Loading simulation
###############################################################################

df1 = read.csv("../Python Codes/WSLS2BRecovered.csv", na.strings=c("","NA"))
df1 <- df1[complete.cases(df1), ]
df1$Region <- df1$Category
df1 <- df1[c('Dyad', 'Player', 'Region', 'Score', 'RegionGo')]
df1$Region <- factor(df1$Region, levels = regiones, ordered = TRUE)
df1$RegionGo <- factor(df1$RegionGo, levels = regiones, ordered = TRUE)
df1 <- df1[order(df1$Region, df1$Score), ] 
head(df1[, 3:5])

###############################################################################
# Obtaining frequencies...
###############################################################################

dfA <- df1[, 3:5]
dfA$Freqs <- apply(dfA, 1, function(x) {
  i <- as.character(x[[1]][1])
  s <- as.numeric(x[[2]][1])
  k <- as.character(x[[3]][1])
  #cat('\ni', i, 's', s, 'k', k)
  return(getRelFreq(i, s, k, df1))
})
dfA <- unique(dfA)
head(dfA)
beep()

###############################################################################
# Graph model recovery...
###############################################################################

df_RS2 <- dfA[dfA$Region == 'RS', ]
df_RS2 <- df_RS2[c('Score', 'RegionGo', 'Freqs')]
df_RS2 <- df_RS2[df_RS2$RegionGo != 'NOTHING', ]
df_RS2 <- df_RS2[df_RS2$RegionGo != 'DOWN', ]
df_RS2 <- df_RS2[df_RS2$RegionGo != 'UP', ]
df_RS2 <- df_RS2[df_RS2$RegionGo != 'LEFT', ]
df_RS2 <- df_RS2[df_RS2$RegionGo != 'RIGHT', ]
df_RS2 <- df_RS2[df_RS2$RegionGo != 'IN', ]
df_RS2 <- df_RS2[df_RS2$RegionGo != 'OUT', ]
head(df_RS2)

#min_score = min(df_ALL$Score)
#min_score
min_score = 0

xs <- seq(-128,32,length.out=161)
# Model
fitRS1 <- sapply(xs, WSprob, i='RS', k='RS', theta=thetaTRUE, regiones=regiones)
fitALL1 <- sapply(xs, WSprob, i='RS', k='ALL', theta=thetaTRUE, regiones=regiones)
# Model fitted from only absent
fitRS2 <- sapply(xs, WSprob, i='RS', k='RS', theta=theta, regiones=regiones)
fitALL2 <- sapply(xs, WSprob, i='RS', k='ALL', theta=theta, regiones=regiones)
dfB <- data.frame(xs, fitRS1, fitALL1, fitRS2, fitALL2)
head(dfB)

gRS2 <- ggplot() +
  geom_point(aes(x = Score, y = Freqs, shape = RegionGo), df_RS2, alpha = 1) +
  scale_shape_manual(values = c("RS" = 1, 
                                "ALL" = 2),
                     name="Jumps to") +  
  scale_x_continuous(limits = c(min_score, 35)) + 
  xlab("Score") +
  ylab("") +
  #  ylab("Rel. Freq./Probability") +
  ggtitle("RS") +
  theme_bw()

tamanho <- 0.9

gRS2 <- gRS2 +
  geom_line(aes(x = xs, y = fitRS1, color="Original transition to RS"), dfB, size = tamanho) + 
  geom_line(aes(x = xs, y = fitALL1, color = "Original transition to ALL"), dfB, size = tamanho) + 
  geom_line(aes(x = xs, y = fitRS2, color = "Recovered transition to RS"), dfB, size = tamanho) + 
  geom_line(aes(x = xs, y = fitALL2, color = "Recovered transition to ALL"), dfB, size = tamanho) + 
  scale_color_manual(values=c("Original transition to ALL"="#E69F00",
                              "Recovered transition to ALL"="#F0E442",
                              "Original transition to RS"="#009E73",
                              "Recovered transition to RS"="#0072B2"),
                     name="Models")  +
  theme_bw()

gRS2


df_ALL2 <- dfA[dfA$Region == 'ALL', ]
df_ALL2 <- df_ALL2[c('Score', 'RegionGo', 'Freqs')]
df_ALL2 <- df_ALL2[df_ALL2$RegionGo != 'NOTHING', ]
df_ALL2 <- df_ALL2[df_ALL2$RegionGo != 'DOWN', ]
df_ALL2 <- df_ALL2[df_ALL2$RegionGo != 'UP', ]
df_ALL2 <- df_ALL2[df_ALL2$RegionGo != 'LEFT', ]
df_ALL2 <- df_ALL2[df_ALL2$RegionGo != 'RIGHT', ]
df_ALL2 <- df_ALL2[df_ALL2$RegionGo != 'IN', ]
df_ALL2 <- df_ALL2[df_ALL2$RegionGo != 'OUT', ]
head(df_ALL2)

#min_score = min(df_ALL$Score)
#min_score
min_score = 0

xs <- seq(-128,32,length.out=161)
# Model
fitRS1 <- sapply(xs, WSprob, i='ALL', k='RS', theta=thetaTRUE, regiones=regiones)
fitALL1 <- sapply(xs, WSprob, i='ALL', k='ALL', theta=thetaTRUE, regiones=regiones)
# Model fitted from only absent
fitRS2 <- sapply(xs, WSprob, i='ALL', k='RS', theta=theta, regiones=regiones)
fitALL2 <- sapply(xs, WSprob, i='ALL', k='ALL', theta=theta, regiones=regiones)
dfB <- data.frame(xs, fitRS1, fitALL1, fitRS2, fitALL2)
head(dfB)

gALL2 <- ggplot() +
  geom_point(aes(x = Score, y = Freqs, shape = RegionGo), df_ALL2, alpha = 1) +
  scale_shape_manual(values = c("RS" = 1, 
                                "ALL" = 2),
                     name="Jumps to") +  
  scale_x_continuous(limits = c(min_score, 35)) + 
  xlab("Score") +
  ylab("") +
  #  ylab("Rel. Freq./Probability") +
  ggtitle("ALL") +
  theme_bw()

tamanho <- 0.9

gALL2 <- gALL2 +
  geom_line(aes(x = xs, y = fitALL1, color = "Original transition to ALL"), dfB, size = tamanho) + 
  geom_line(aes(x = xs, y = fitRS1, color="Original transition to RS"), dfB, size = tamanho) + 
  geom_line(aes(x = xs, y = fitRS2, color = "Recovered transition to RS"), dfB, size = tamanho) + 
  geom_line(aes(x = xs, y = fitALL2, color = "Recovered transition to ALL"), dfB, size = tamanho) + 
  scale_color_manual(values=c("Original transition to ALL"="#E69F00",
                              "Recovered transition to ALL"="#F0E442",
                              "Original transition to RS"="#009E73",
                              "Recovered transition to RS"="#0072B2"),
                     name="Models")  +
  theme_bw()

gALL2

legend <- get_legend(gRS2)
gRS2 <- gRS2 + theme(legend.position="none")
gALL2 <- gALL2 + theme(legend.position="none")

grid.arrange(gRS2, gALL2,
             nrow = 1,
             right=legend, top="Model recovery - transitions")
