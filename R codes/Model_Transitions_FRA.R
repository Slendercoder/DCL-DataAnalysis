library(stats4)
library(bbmle)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(beepr)

#####################################################
# Definitions
#####################################################

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

getRelFreq <- function(i, s, j, k, df) {
  # Obtains the relative frequencies for transition from region i and score s to region k
  # Input: i, which is the region the player is currently in
  #        s, which is the player's score obtained on the previous round
  #        j, which is the overlapping region
  #        k, which is the region the player is going to
  #        df, the dataframe from which the observations are obtained
  # Output: Relative frequency
  
  regs <- df[df$Region == i, ]
  regs <- regs[regs$Score == s, ]
  n <- dim(regs)[1]
  if (n > 5) {
    regs <- regs[regs$RegionGo == k, ]
    f <- dim(regs)[1]
    return(f/n)
  } else {
    return(NA)
  }
  
  # auxDF <- t(as.data.frame(table(regsGo)))
  #  print(auxDF)
  # colnames(auxDF) <- as.character(unlist(auxDF[1, ])) # the first row will be the header
  # auxDF <- auxDF[-1, ]          # removing the first row.
  # auxDF <- auxDF[regiones]
  # return(as.numeric(auxDF[1:9]))
}

sigmoid <- function(x, beta, gamma) {
  # Returns the value of the sigmoid function 1/(1+exp(b(x-c)))
  
  return(1 / (1 + exp(-beta * (x - gamma))))
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
  #  if (i != 'RS') {
  index <- which(regiones == i)
  attractiveness[index] <- attractiveness[index] + alpha * sigmoid(n, beta, gamma) 
  #  }
  
  probs <- attractiveness / sum(attractiveness)
  #  probs <- replace(probs,probs<lowerEps2,lowerEps2)
  #  probs <- replace(probs,probs>highEps2,highEps2)
  
  probab <- probs[which(regiones == k)]
  
  return(probab)
}

#####################################################
# Global variables
#####################################################

# Estimated parameters:
theta <- c(0.094, 0.078, 0.017, 0.005, 10.619, 495.681, 29.002, 0, 0, 0, 0)

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
# Loading database
###############################################################################

df1 = read.csv("../Python Codes/humans.csv", na.strings=c("","NA"))
df1$Region <- df1$Category
df1 <- df1[complete.cases(df1), ]
df1 <- df1[c('Dyad', 'Player', 'Is_there', 'Region', 'Score', 'RegionGo')]
head(df1[, 3:6])

###############################################################################
# Obtaining frequencies...
###############################################################################

dfA <- df1[, 3:6]
dfA$Freqs <- apply(dfA, 1, function(x) {
  i <- as.character(x[[2]][1])
  s <- as.numeric(x[[3]][1])
  k <- as.character(x[[4]][1])
  #cat('\ni', i, 's', s, 'k', k)
  return(getRelFreq(i, s, k, df1))
})
dfA <- unique(dfA)
dfA <- dfA[complete.cases(dfA), ]
head(dfA)
df <- dfA
beep()

###############################################################################
# Plot data...
###############################################################################

#min_score = -50
min_score = 0

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
df_RS <- df_RS[df_RS$RegionGo != 'NOTHING', ]
df_RS <- df_RS[df_RS$RegionGo != 'DOWN', ]
df_RS <- df_RS[df_RS$RegionGo != 'UP', ]
df_RS <- df_RS[df_RS$RegionGo != 'LEFT', ]
df_RS <- df_RS[df_RS$RegionGo != 'RIGHT', ]
df_RS <- df_RS[df_RS$RegionGo != 'IN', ]
df_RS <- df_RS[df_RS$RegionGo != 'OUT', ]
head(df_RS)

gRS2ALL <- ggplot() +
  geom_point(aes(x = Score, y = Freqs), df_RS, alpha = 0.4, size=1.5) +
  scale_x_continuous(limits = c(min_score, 35)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  xlab("Score") +
  ylab("") +
  #  ylab("Rel. Freq./Probability") +
  ggtitle("Transition from RS to ALL") +
  theme_bw()

gRS2ALL

df_ALL <- df[df$Region == 'ALL', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'ALL', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'NOTHING', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'DOWN', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'UP', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'LEFT', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'RIGHT', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'IN', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'OUT', ]
head(df_ALL)

gALL2RS <- ggplot() +
  geom_point(aes(x = Score, y = Freqs), df_ALL, alpha = 0.4, size=1.5) +
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  xlab("Score") +
  #  ylab("") +
  ylab("Rel. Freq./Probability") +
  ggtitle("Transition from ALL to RS") +
  theme_bw()

gALL2RS

df_ALL <- df[df$Region == 'ALL', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'RS', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'NOTHING', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'DOWN', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'UP', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'LEFT', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'RIGHT', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'IN', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'OUT', ]
head(df_ALL)

gALL2ALL <- ggplot() +
  geom_point(aes(x = Score, y = Freqs), df_ALL, alpha = 0.4, size=1.5) +
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  xlab("Score") +
  ylab("") +
  #  ylab("Rel. Freq./Probability") +
  ggtitle("Transition from ALL to ALL") +
  theme_bw()

gALL2ALL

grid.arrange(gRS2RS, gRS2ALL, gALL2RS, gALL2ALL,
             nrow = 2, 
             top="Human data")