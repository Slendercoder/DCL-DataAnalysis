#library(stats4)
#library(bbmle)
#library(dplyr)
library(ggplot2)
library(gridExtra)
library(beepr)

#####################################################
# Definitions
#####################################################

specify_decimal3 <- function(x) trimws(format(round(x, 3), nsmall=3))
imprimir <- function(x) print(as.numeric(unlist(lapply(x, specify_decimal3))))

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
} # end Nombre_Region

getRelFreqFRA <- function(i, s, k, df) {
  # Obtains the relative frequencies for transition from region i and score s to region k
  # Input: i, which is the region the player is currently in
  #        s, which is the player's FRAsim obtained on the previous round
  #        k, which is the region the player is going to
  #        df, the dataframe from which the observations are obtained
  # Output: Relative frequency
  
  s <- as.numeric(s)
  
  regs <- df[df$Region == i, ]
#  cat('\nStarting from i =', i, '\n')
#  print(regs)
  regs <- regs[regs$FRASim == s, ]
#  cat('\nwith FRAsim s =', s ,'\n')
#  print(regs)
  n <- dim(regs)[1]
  regs <- regs[regs$RegionGo == k, ]
  f <- dim(regs)[1]
#  cat('\ni', i, 's', s, 'k', k, 'n', n, 'f', f, 'ret', f/n)
  return(f/n)

} # end getRelFreqFRA

sigmoid <- function(x, beta, gamma) {
  # Returns the value of the sigmoid function 1/(1+exp(b(x-c)))
  
  return(1 / (1 + exp(-beta * (x - gamma))))
} # end sigmoid

ModelProb <- function(regionFrom, regionGo, s, k, theta, regiones){
  
  # FRA model returns probability of going from regionFrom to regionGo
  # given FRA similarity to region k
  
  wALL <- theta[1]
  wNOTHING <- theta[2]
  wLEFT <- theta[3]
  wIN <- theta[4]
  alpha <- theta[5]
  beta <- theta[6]
  gamma <- theta[7]
  aux <- c(wALL, wNOTHING, wLEFT, wLEFT, wLEFT, wLEFT, wIN, wIN)
  
  # The probability of region 'RS' is 1 - the sum of the other probabilities
  if (sum(aux) > 1) {
    aux <- aux/sum(aux)
  }
  bias <- c(1 - sum(aux), aux)
  # print('bias')
  # imprimir(bias)

  # Find the attractivenes:
  focal <- 0
  index <- which(regiones == regionGo)
  attractiveness <- bias[index]
  if (regionFrom == 'RS') { 
    if (regionGo != 'RS') {
      if(k == regionGo) {
        attractiveness <- attractiveness + alpha * sigmoid(s, beta, gamma)
      }
    }
  } else {
    if(regionFrom == regionGo) {
      attractiveness <- attractiveness + alpha
      focal <- alpha
    }
    if(k == regionGo) {
      attractiveness <- attractiveness + alpha * sigmoid(s, beta, gamma)
      focal <- alpha
    }
  }
  
  probab <- attractiveness / (1 + alpha * sigmoid(s, beta, gamma) + focal)

  return(probab)
} # end ModelProb

#a <- ModelProb('RS', 'RS', 0.1, 'RIGHT', theta, regiones)
#a

#####################################################
# Global variables
#####################################################

# Estimated parameters:
theta <- c(0.035, 0.048, 0.003, 0.001, 176.88, 23.042, 1.152) # FRA

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

df1 = read.csv("../Python Codes/fraFreqs-humans.csv", na.strings=c("","NA"))
#df1 = read.csv("../Python Codes/fraFreqs-simulated.csv", na.strings=c("","NA"))
df1$Region <- df1$Category
df1 <- df1[complete.cases(df1), ]
df1 <- df1[c('Region', 'FRASim', 'RegionGo')]
head(df1)

###############################################################################
# Obtaining frequencies...
###############################################################################

dfA <- df1

dfA$Freqs <- apply(dfA, 1, function(x) {
  i <- x[[1]][1]
  s <- x[[2]][1]
  k <- x[[3]][1]
#  cat('\ni', i, 's', s, 'k', k, '\n')
#  print(x)
  return(getRelFreqFRA(i, s, k, dfA))
})

dfA <- unique(dfA)
dfA <- dfA[complete.cases(dfA), ]
head(dfA)
df <- dfA
beep()

###############################################################################
# Plot data...
###############################################################################

df_RS <- df[df$Region == 'RS', ]
#df_RS <- df_RS[df_RS$RegionGo != 'RS', ]
df_RS <- df_RS[df_RS$RegionGo != 'ALL', ]
df_RS <- df_RS[df_RS$RegionGo != 'NOTHING', ]
df_RS <- df_RS[df_RS$RegionGo != 'DOWN', ]
df_RS <- df_RS[df_RS$RegionGo != 'UP', ]
df_RS <- df_RS[df_RS$RegionGo != 'LEFT', ]
#df_RS <- df_RS[df_RS$RegionGo != 'RIGHT', ]
df_RS <- df_RS[df_RS$RegionGo != 'IN', ]
df_RS <- df_RS[df_RS$RegionGo != 'OUT', ]
head(df_RS)

gRS <- ggplot() +
  geom_point(aes(x = FRASim, y = Freqs, color=RegionGo), df_RS, alpha = 0.5, size=1.5) +
  scale_x_continuous(limits = c(0, 1.01)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  xlab("FRASim to RIGHT") +
  #  ylab("") +
  ylab("Rel. Freq./Probability") +
  ggtitle("Transitions from RS") +
  scale_colour_manual(values = c("RS" = "#999999", "RIGHT" = "#E69F00")) + 
  theme_bw()

gRS

xs <- seq(0,1.2,length.out=1000)

# Transition from RS to RS
# Model fitted 
fitEST <- sapply(xs, ModelProb, regionFrom='RS', regionGo='RS', k='RIGHT', theta=theta, regiones=regiones)
dfmodels <- data.frame(xs, fitEST)
head(dfmodels)

# Size of models' lines
tamanho <- 0.5

gRS2RS <- gRS +
  geom_line(aes(x = xs, y = fitEST), dfmodels, size = tamanho, color="#999999") + 
  scale_x_continuous(limits = c(0, 1.01)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  theme_bw()

gRS2RS

# Transition from RS to RIGHT
# Model fitted
fitEST <- sapply(xs, ModelProb, regionFrom='RS', regionGo='RIGHT', k='RIGHT', theta=theta, regiones=regiones)
dfmodels <- data.frame(xs, fitEST)
head(dfmodels)

gRS2RIGHT <- gRS2RS +
  geom_line(aes(x = xs, y = fitEST), dfmodels, size = tamanho, color="#E69F00") + 
  scale_x_continuous(limits = c(0, 1.01)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  theme_bw()

gRS2RIGHT

grid.arrange(gRS2RIGHT,
             nrow = 1, 
             right=legend, 
             bottom="Points: data     Lines: Model",
             top="FRA model of transitions")
