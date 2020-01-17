library(stats4)
library(bbmle)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(beepr)

#####################################################
# Definitions
#####################################################

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
} # end get_legend

Nombre_Region <- function(x) {
  if (x == '0' || x== '9') {
    return('RS')
  } else if (x == '1') {
    return('RIGHT')
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
} # end Nombre_Region

getRelFreqScore <- function(i, s, k, df) {
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
#  if (n > 5) {
#    regs <- regs[regs$RegionGo == k, ]
#    f <- dim(regs)[1]
#    return(f/n)
#  } else {
#    return(NA)
#  }

  # auxDF <- t(as.data.frame(table(regsGo)))
  #  print(auxDF)
  # colnames(auxDF) <- as.character(unlist(auxDF[1, ])) # the first row will be the header
  # auxDF <- auxDF[-1, ]          # removing the first row.
  # auxDF <- auxDF[regiones]
  # return(as.numeric(auxDF[1:9]))

} # end getRelFreqScore

getRelFreqFRA <- function(i, s, k, df) {
  # Obtains the relative frequencies for transition from region i and score s to region k
  # Input: i, which is the region the player is currently in
  #        s, which is the player's FRAsim obtained on the previous round
  #        k, which is the region the player is going to
  #        df, the dataframe from which the observations are obtained
  # Output: Relative frequency
  
  regs <- df[df$Region == i, ]
  cat('\nStarting from i =', i, '\n')
  print(regs)
  regs <- regs[regs$FRASim == s, ]
  cat('\nwith FRAsim s =', s ,'\n')
  print(regs)
  n <- dim(regs)[1]
  regs <- regs[regs$RegionGo == k, ]
  f <- dim(regs)[1]
  cat('\ni', i, 's', s, 'k', k, 'n', n, 'f', f, 'ret', f/n)
  return(f/n)

} # end getRelFreqFRA

sigmoid <- function(x, beta, gamma) {
  # Returns the value of the sigmoid function 1/(1+exp(b(x-c)))
  
  return(1 / (1 + exp(-beta * (x - gamma))))
} # end sigmoid

ModelProb <- function(i, s, k, theta, regiones){
  
  wRIGHT <- theta[1]
  wNOTHING <- theta[2]
  wLEFT <- theta[3]
  wIN <- theta[4]
  alpha <- theta[5]
  beta <- theta[6]
  gamma <- theta[7]
  aux <- c(wRIGHT, wNOTHING, wLEFT, wLEFT, wLEFT, wLEFT, wIN, wIN)
  
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
} # end ModelProb

#####################################################
# Global variables
#####################################################

# Estimated parameters:
#theta <- c(0.094, 0.078, 0.017, 0.005, 10.619, 495.681, 29.002, 0, 0, 0, 0)
#theta <- c(0.093, 0.062, 0.015, 0.000, 17.845, 255.959, 19.014, 0, 0, 0, 0)
#theta <- c(0.042, 0.037, 0.015, 0.001, 30.000, 0.032, 32.000, 0, 0, 0, 0)
#theta <- c(0.14, 0.0674, 0.0123, 0.0009, 39, 405, 20, 0, 0, 0, 0)
theta <- c(0.1, 0.1, 0.1, 0.1, 200, 500, 32) # WSLS
theta <- c(0.1, 0.1, 0.1, 0.1, 200, 500, 0.7) # FRA

regiones <- c('RS',
              'RIGHT', 
              'NOTHING', 
              'DOWN', 
              'UP', 
              'LEFT', 
              'RIGHT', 
              'IN', 
              'OUT')

###############################################################################
# Loading database
###############################################################################

#df1 = read.csv("../Python Codes/humans.csv", na.strings=c("","NA"))
df1 = read.csv("../Python Codes/fraFreqs.csv", na.strings=c("","NA"))
df1$Region <- df1$Category
#df1$FRASim <- as.numeric(df1$FRASim)
df1 <- df1[complete.cases(df1), ]
#df1 <- df1[c('Region', 'Score', 'RegionGo')]
df1 <- df1[c('Region', 'FRASim', 'RegionGo')]
head(df1)

###############################################################################
# Obtaining frequencies...
###############################################################################

dfA <- df1[1:2, ]

#########################################
# Using Score
#########################################
dfA$Freqs <- apply(dfA, 1, function(x) {
  i <- as.character(x[[1]][1])
  s <- as.numeric(x[[2]][1])
  k <- as.character(x[[3]][1])
  #cat('\ni', i, 's', s, 'k', k)
  return(getRelFreqScore(i, s, k, dfA))
})
#########################################

#########################################
# Using FRA similarity
#########################################
dfA$Freqs <- sapply(dfA, 1, function(x) {
  i <- x[[1]][1]
  s <- x[[2]][1]
  k <- x[[3]][1]
  cat('\ni', i, 's', s, 'k', k, '\n')
  print(x)
  return(getRelFreqFRA(i, s, k, dfA))
})

s <- dfA$FRASim[1]

s <- 0.1315789
typeof(s)
dfA[dfA$FRASim == s, ]

#########################################

dfA <- unique(dfA)
dfA <- dfA[complete.cases(dfA), ]
head(dfA)
df <- dfA
beep()

###############################################################################
# Plot data...
###############################################################################

#min_score = -50
min_score = -20
#min_score = 0

df_RS <- df[df$Region == 'RS', ]
head(df_RS)

gRS <- ggplot() +
  geom_point(aes(x = Score, y = Freqs, color=RegionGo), df_RS, alpha = 0.4, size=1.5) +
  scale_x_continuous(limits = c(min_score, 35)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  xlab("Score") +
  #  ylab("") +
  ylab("Rel. Freq./Probability") +
  ggtitle("Transitions from RS") +
  theme_bw()

gRS

df_RS <- df[df$Region == 'RS', ]
df_RS <- df_RS[df_RS$RegionGo != 'ALL', ]
df_RS <- df_RS[df_RS$RegionGo != 'NOTHING', ]
df_RS <- df_RS[df_RS$RegionGo != 'DOWN', ]
df_RS <- df_RS[df_RS$RegionGo != 'UP', ]
df_RS <- df_RS[df_RS$RegionGo != 'LEFT', ]
df_RS <- df_RS[df_RS$RegionGo != 'RIGHT', ]
df_RS <- df_RS[df_RS$RegionGo != 'IN', ]
df_RS <- df_RS[df_RS$RegionGo != 'OUT', ]
head(df_RS)

gRS2RS <- ggplot() +
  geom_point(aes(x = Score, y = Freqs), df_RS, alpha = 0.4, size=1.5) +
  scale_x_continuous(limits = c(min_score, 35)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  xlab("Score") +
#  ylab("") +
  ylab("Rel. Freq./Probability") +
  ggtitle("Transition from RS to RS") +
  theme_bw()

gRS2RS

df_RS <- df[df$Region == 'RS', ]
df_RS <- df_RS[df_RS$RegionGo != 'RS', ]
df_RS <- df_RS[df_RS$RegionGo != 'ALL', ]
df_RS <- df_RS[df_RS$RegionGo != 'NOTHING', ]
df_RS <- df_RS[df_RS$RegionGo != 'DOWN', ]
df_RS <- df_RS[df_RS$RegionGo != 'UP', ]
df_RS <- df_RS[df_RS$RegionGo != 'LEFT', ]
df_RS <- df_RS[df_RS$RegionGo != 'IN', ]
df_RS <- df_RS[df_RS$RegionGo != 'OUT', ]
head(df_RS)

gRS2RIGHT <- ggplot() +
  geom_point(aes(x = Score, y = Freqs), df_RS, alpha = 0.4, size=1.5) +
  scale_x_continuous(limits = c(min_score, 35)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  xlab("Score") +
  ylab("") +
  #  ylab("Rel. Freq./Probability") +
  ggtitle("Transition from RS to RIGHT") +
  theme_bw()

gRS2RIGHT

df_RIGHT <- df[df$Region == 'RIGHT', ]
df_RIGHT <- df_RIGHT[df_RIGHT$RegionGo != 'RS', ]
df_RIGHT <- df_RIGHT[df_RIGHT$RegionGo != 'ALL', ]
df_RIGHT <- df_RIGHT[df_RIGHT$RegionGo != 'NOTHING', ]
df_RIGHT <- df_RIGHT[df_RIGHT$RegionGo != 'DOWN', ]
df_RIGHT <- df_RIGHT[df_RIGHT$RegionGo != 'UP', ]
df_RIGHT <- df_RIGHT[df_RIGHT$RegionGo != 'LEFT', ]
df_RIGHT <- df_RIGHT[df_RIGHT$RegionGo != 'IN', ]
df_RIGHT <- df_RIGHT[df_RIGHT$RegionGo != 'OUT', ]
head(df_RIGHT)

gRIGHT2RIGHT <- ggplot() +
  geom_point(aes(x = Score, y = Freqs), df_RIGHT, alpha = 0.4, size=1.5) +
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  xlab("Score") +
  ylab("") +
  #  ylab("Rel. Freq./Probability") +
  ggtitle("Transition from RIGHT to RIGHT") +
  theme_bw()

gRIGHT2RIGHT

grid.arrange(gRS2RS, gRS2RIGHT, gRIGHT2RIGHT,
             nrow = 2, 
             top="Human data")

#################################################
# Including models on top of data plots
#################################################

#xs <- seq(-128,32,length.out=161)
xs <- seq(min_score,32,length.out=(32-min_score + 1)*1000)

# Transition from RS to RS
# Model fitted 
fitEST <- sapply(xs, ModelProb, i='RS', k='RS', theta=theta, regiones=regiones)
dfmodels <- data.frame(xs, fitEST)
head(dfmodels)

# Size of models' lines
tamanho <- 0.5

# Dummy plot to get legend
dummyplot <- ggplot() +
  geom_line(aes(x = xs, y = fitEST, color = "WSLS"), dfmodels, size = tamanho) + 
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  scale_color_manual(values=c("WSLS"="#E69F00",
                                "Recovered"="#009E73"),
                       name="Transition")  +
  theme_bw() +
  theme(legend.position="bottom")

legend2 <- get_legend(dummyplot)

gRS2RS <- gRS2RS +
  geom_line(aes(x = xs, y = fitEST), dfmodels, size = tamanho, color="#E69F00") + 
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  theme_bw() +
  theme(legend.position="none")

gRS2RS

# Transition from RS to RIGHT
# Model fitted
fitEST <- sapply(xs, ModelProb, i='RS', k='RIGHT', theta=theta, regiones=regiones)
dfmodels <- data.frame(xs, fitEST)
head(dfmodels)

gRS2RIGHT <- gRS2RIGHT +
  geom_line(aes(x = xs, y = fitEST), dfmodels, size = tamanho, color="#E69F00") + 
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  theme_bw() +
  theme(legend.position="none")

gRS2RIGHT

# Transition from RIGHT to RIGHT
# Model fitted
fitEST <- sapply(xs, ModelProb, i='RIGHT', k='RIGHT', theta=theta, regiones=regiones)
dfmodels <- data.frame(xs, fitEST)
head(dfmodels)

gRIGHT2RIGHT <- gRIGHT2RIGHT +
  geom_line(aes(x = xs, y = fitEST), dfmodels, size = tamanho, color="#E69F00") + 
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  theme_bw() +
  theme(legend.position="none")

gRIGHT2RIGHT

grid.arrange(gRS2RS, gRS2RIGHT, gRIGHT2RIGHT,
             nrow = 2, 
             right=legend, 
             bottom=legend2,
             top="Model of transitions")
 