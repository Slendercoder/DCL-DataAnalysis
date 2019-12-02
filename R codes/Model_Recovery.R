library(stats4)
library(bbmle)
library(dplyr)
library(ggplot2)
library(gridExtra)

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
  
  n <- (s + 128) / 160 # Normalizing score
#  n <- s
  
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
# Instructions
#####################################################

# TrueVal: 
thetaTRUE <- c(0.0125, 0.0125, 0.0125, 0.0125, 150, 405, 0.98, 0, 0, 0, 0)

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
# To use for data estimated from full information
###############################################################################

# Estimated from fullWSLS2BRecovered.csv
theta <- c(0.009, 0.009, 0.008, 0.007, 126.462, 444.185, 0.978, 0, 0, 0, 0) # Estimated full information
df1 = read.csv("../Python Codes/fullWSLS2BRecovered.csv", na.strings=c("","NA"))
df1$Region <- sapply(df1$Strategy, Nombre_Region)

df1 <- df1 %>% 
  group_by(Player) %>%
  mutate(RegionGo = lead(Region)) %>%
  as.data.frame()

#df1 <- df1[complete.cases(df1), ]
df1 <- df1[c('Dyad', 'Player', 'Region', 'Score', 'RegionGo')]
head(df1[, 3:5])

###############################################################################
# To use for data estimated from only absent
###############################################################################

# Estimated from WSLS2BRecovered.csv
theta <- c(0.012, 0.010, 0.010, 0.009, 184.559, 187.651, 0.999, 0, 0, 0, 0) # Estimated only absent
#theta <- c(0.012, 0.010, 0.011, 0.009, 177.922, 190.004, 1, 0, 0, 0, 0) # Estimated only absent
df1 = read.csv("../Python Codes/WSLS2BRecovered.csv", na.strings=c("","NA"))
#theta <- c(0.010, 0.019, 0.009, 0.010, 18.962, 124.242, 1.000, 0, 0, 0, 0) # Estimated only absent
#df1 = read.csv("../Python Codes/WSLS2BRecovered_32.csv", na.strings=c("","NA"))
df1 <- df1[complete.cases(df1), ]
df1$Region <- df1$Category
df1 <- df1[c('Dyad', 'Player', 'Region', 'Score', 'RegionGo')]
df1$Region <- factor(df1$Region, levels = regiones, ordered = TRUE)
df1$RegionGo <- factor(df1$RegionGo, levels = regiones, ordered = TRUE)
df1 <- df1[order(df1$Region, df1$Score), ] 
head(df1[, 3:5])

###############################################################################
# Graph model recovery...
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

df_RS <- dfA[dfA$Region == 'RS', ]
df_RS <- df_RS[c('Score', 'RegionGo', 'Freqs')]
#df_RS <- df_RS[df_RS$RegionGo != 'ALL', ]
df_RS <- df_RS[df_RS$RegionGo != 'NOTHING', ]
df_RS <- df_RS[df_RS$RegionGo != 'DOWN', ]
df_RS <- df_RS[df_RS$RegionGo != 'UP', ]
df_RS <- df_RS[df_RS$RegionGo != 'LEFT', ]
df_RS <- df_RS[df_RS$RegionGo != 'RIGHT', ]
df_RS <- df_RS[df_RS$RegionGo != 'IN', ]
df_RS <- df_RS[df_RS$RegionGo != 'OUT', ]
head(df_RS)

#min_score = min(df_RS$Score)
#min_score
min_score = 0

xs <- seq(-128,32,length.out=161)
fitRS <- sapply(xs, WSprob, i='RS', k='RS', theta=theta, regiones=regiones)
fitALL <- sapply(xs, WSprob, i='RS', k='ALL', theta=theta, regiones=regiones)
fitNOTHING <- sapply(xs, WSprob, i='RS', k='NOTHING', theta=theta, regiones=regiones)
fitLEFT <- sapply(xs, WSprob, i='RS', k='LEFT', theta=theta, regiones=regiones)
dfB <- data.frame(xs, fitRS, fitALL, fitNOTHING, fitLEFT)

gRS <- ggplot() +
  geom_point(aes(x = Score, y = Freqs, colour = RegionGo), df_RS, alpha = 0.5) +
  scale_colour_manual(values = c("RS" = "#999999", 
                                 "ALL" = "#E69F00", 
                                 "NOTHING" = "#56B4E9",
                                 "LEFT" = "#F0E442")) +  
  geom_line(aes(x = xs, y = fitRS), dfB, color="#999999") + 
  geom_line(aes(x = xs, y = fitALL), dfB, color = "#E69F00") + 
  scale_x_continuous(limits = c(min_score, 35)) + 
  labs(color = "Jump to") +
  xlab("Score") +
  ylab("Rel. Freq./Probability") +
  ggtitle("RS") +
  theme_bw()

gRS

df_ALL <- dfA[dfA$Region == 'ALL', ]
df_ALL <- df_ALL[c('Score', 'RegionGo', 'Freqs')]
df_ALL <- df_ALL[df_ALL$RegionGo != 'NOTHING', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'DOWN', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'UP', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'LEFT', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'RIGHT', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'IN', ]
df_ALL <- df_ALL[df_ALL$RegionGo != 'OUT', ]
head(df_ALL)

#min_score = min(df_ALL$Score)
#min_score
min_score = 0

xs <- seq(-128,32,length.out=161)
fitRS <- sapply(xs, WSprob, i='ALL', k='RS', theta=theta, regiones=regiones)
fitALL <- sapply(xs, WSprob, i='ALL', k='ALL', theta=theta, regiones=regiones)
#fitNOTHING <- sapply(xs, WSprob, i='ALL', k='NOTHING', theta=theta, regiones=regiones)
#fitLEFT <- sapply(xs, WSprob, i='ALL', k='LEFT', theta=theta, regiones=regiones)
dfB <- data.frame(xs, fitRS, fitALL, fitNOTHING, fitLEFT)
head(dfB)

gALL<- ggplot() +
  geom_point(aes(x = Score, y = Freqs, colour = RegionGo), df_ALL, alpha = 0.5) +
  scale_colour_manual(values = c("RS" = "#999999", 
                                 "ALL" = "#E69F00", 
                                 "NOTHING" = "#56B4E9",
                                 "LEFT" = "#F0E442")) +  
  geom_line(aes(x = xs, y = fitRS), dfB, color="#999999") + 
  geom_line(aes(x = xs, y = fitALL), dfB, color = "#E69F00") + 
#  geom_line(aes(x = xs, y = fitNOTHING), dfB, color = "#56B4E9") + 
#  geom_line(aes(x = xs, y = fitLEFT), dfB, color = "#F0E442") + 
  scale_x_continuous(limits = c(min_score, 35)) + 
  labs(color = "Jump to") +
  xlab("Score") +
  ylab("") +
#  ylab("Rel. Freq./Probability") +
  ggtitle("ALL") +
  theme_bw()

gALL

df_NOTHING <- dfA[dfA$Region == 'NOTHING', ]
df_NOTHING <- df_NOTHING[c('Score', 'RegionGo', 'Freqs')]
df_NOTHING <- df_NOTHING[df_NOTHING$RegionGo != 'ALL', ]
df_NOTHING <- df_NOTHING[df_NOTHING$RegionGo != 'DOWN', ]
df_NOTHING <- df_NOTHING[df_NOTHING$RegionGo != 'UP', ]
df_NOTHING <- df_NOTHING[df_NOTHING$RegionGo != 'LEFT', ]
df_NOTHING <- df_NOTHING[df_NOTHING$RegionGo != 'RIGHT', ]
df_NOTHING <- df_NOTHING[df_NOTHING$RegionGo != 'IN', ]
df_NOTHING <- df_NOTHING[df_NOTHING$RegionGo != 'OUT', ]
head(df_NOTHING)

#min_score = min(df_NOTHING$Score)
#min_score
min_score = 0


xs <- seq(-128,32,length.out=161)
fitRS <- sapply(xs, WSprob, i='NOTHING', k='RS', theta=theta, regiones=regiones)
fitNOTHING <- sapply(xs, WSprob, i='NOTHING', k='NOTHING', theta=theta, regiones=regiones)
dfB <- data.frame(xs, fitRS, fitNOTHING)
head(dfB)

gNOTHING <- ggplot() +
  geom_point(aes(x = Score, y = Freqs, colour = RegionGo), df_NOTHING, alpha = 0.5) +
  scale_colour_manual(values = c("RS" = "#999999", 
                                 "ALL" = "#E69F00", 
                                 "NOTHING" = "#56B4E9",
                                 "LEFT" = "#F0E442")) +  
  geom_line(aes(x = xs, y = fitRS), dfB, color="#999999") + 
  geom_line(aes(x = xs, y = fitNOTHING), dfB, color = "#56B4E9") + 
  scale_x_continuous(limits = c(min_score, 35)) + 
  labs(color = "Jump to") +
  xlab("Score") +
  ylab("") +
#  ylab("Rel. Freq./Probability") +
  ggtitle("NOTHING") +
  theme_bw()

gNOTHING

###############################################################################
# To use for data estimated from only absent
###############################################################################

tituloTOP = "True params:"
tituloTOP = paste(tituloTOP,
                  ' wALL =', as.character(thetaTRUE[1]),
                  ' wNOTHING =', as.character(thetaTRUE[2]),
                  ' wLEFT =', as.character(thetaTRUE[3]),
                  ' wIN =', as.character(thetaTRUE[4]),
                  ' alpha =', as.character(thetaTRUE[5]),
                  ' beta =', as.character(thetaTRUE[6]),
                  ' gamma =', as.character(thetaTRUE[7]))

tituloTOP = paste(tituloTOP, "\nEstimated:")
tituloTOP = paste(tituloTOP,
                  ' wALL =', as.character(theta[1]),
                  ' wNOTHING =', as.character(theta[2]),
                  ' wLEFT =', as.character(theta[3]),
                  ' wIN =', as.character(theta[4]),
                  ' alpha =', as.character(theta[5]),
                  ' beta =', as.character(theta[6]),
                  ' gamma =', as.character(theta[7]))
tituloBOTTOM = "Model recovered from only absent"

gOA <- grid.arrange(gRS, gALL, gNOTHING, ncol = 3, top=tituloTOP, bottom=tituloBOTTOM)
#gOA <- grid.arrange(gRS, gALL, gNOTHING, gLEFT, ncol = 2, top=tituloTOP, bottom=tituloBOTTOM)

###############################################################################
# To use for data estimated from full information
###############################################################################

tituloTOP = "True params:"
tituloTOP = paste(tituloTOP,
                  ' wALL =', as.character(thetaTRUE[1]),
                  ' wNOTHING =', as.character(thetaTRUE[2]),
                  ' wLEFT =', as.character(thetaTRUE[3]),
                  ' wIN =', as.character(thetaTRUE[4]),
                  ' alpha =', as.character(thetaTRUE[5]),
                  ' beta =', as.character(thetaTRUE[6]),
                  ' gamma =', as.character(thetaTRUE[7]))

tituloTOP = paste(tituloTOP, "\nEstimated:")
tituloTOP = paste(tituloTOP,
               ' wALL =', as.character(theta[1]),
               ' wNOTHING =', as.character(theta[2]),
               ' wLEFT =', as.character(theta[3]),
               ' wIN =', as.character(theta[4]),
               ' alpha =', as.character(theta[5]),
               ' beta =', as.character(theta[6]),
               ' gamma =', as.character(theta[7]))
tituloBOTTOM = "Model recovered from full information\n"
gFULL <- grid.arrange(gRS, gALL, gNOTHING, ncol = 3, top=tituloTOP, bottom=tituloBOTTOM)

####
g <- grid.arrange(gFULL, gOA, nrow = 2)


#####################################################
#####################################################

#This is an exercise in model recovery.  We generate choice data according to a logistic function and then see how well we can recover the parameters that
#generated the data in the first place
#The actual randomly generated, observed data are shown in red.  The actual model that generated the data is shown in red
#The maximum likelihood fit model is shown in blue

length=10 # number of data points - for example, the number of stimulus intensities
numTrials=100
lowerEps1=1/numTrials
highEps1 =(numTrials-1)/numTrials
lowerEps2=.000000000000000000001
highEps2 =.999999999999999999999
actualA=-4
actualB=4 #if actualB is too high then recovery sucks because probabilities are 0 or 1
prediction=c()
prob=c()
logisticfun <- function(x, a,b) {1/(1+exp(-(x-a)*b))} #this is the function that will be fit
xs=seq(-10,10,length.out=length) #creates a sequence of 100 numbers from -5 to 5
#add a bit of normal noise to data to make it more realistic and harder to recover the model
#note that a specific number of trials needs to be created
#Imagine that data = the probability of responding "yes" given a certain level of intensity of signal
data=round(numTrials*(logisticfun(xs,actualA,actualB) + rnorm(length, 0, .001))) # create data according to a logistic function with specific parameters to see if we can recover the parameters.  Add noise with mean of 0 and sd of 0.15
data <- replace (data, data > numTrials,numTrials) #it is too weird to have choice probabilities greater than 1 or less than 0 even if coming from a human
data <- replace (data, data < 0,0) # and don't want probability to actually be zero because then it won't match anything >0 at all
plot(xs,data,col="red")
points(xs,numTrials*logisticfun(xs,actualA,actualB),type="l",col="red")

logisticfunc <- function(a,b) 
{
  #cat(a,b,"\n") # if you want to see how parameter values change during optimization
  prediction<<-logisticfun(xs,a,b) #by using <<- we assign global variable to new value
  #prediction<<-prediction+rnorm(length, 0, .03) # could add some noise to prediction
  #prediction<<-replace(prediction,prediction>(numTrials-1)/numTrials,(numTrials-1)/numTrials) #It's very important that we don't make predictions if probabilities are too close to 0 or 1
  #prediction<<-replace(prediction,prediction<1/numTrials,1/numTrials) # if probabilities are close to the extremes, then anything not extreme is infinitely unlikely
  prediction<<-replace(prediction,prediction<lowerEps1,lowerEps1)
  prediction<<-replace(prediction,prediction>highEps1,highEps1)
  #In giving values for extreme probabilities, split the difference between the extreme and the next observable result
  # trying to maximize the negative of the sum of the logs of the probabilities of the data given the model, by fitting a and b to the model
  # If the model predicts saying "yes" with probability p then the probability of producing an observed number of "yes"es out of numTrials
  #trials is found by consulting a binomial density distribution.  The most probable outcome of a model that predicts p "Yes"es is
  #data/numTrials, but depending on how many trials were run, there can be quite a bit scatter around this.
  
  prob<<- dbinom(data,numTrials,prediction)
  prob<<- replace(prob,prob<lowerEps2,lowerEps2)
  prob<<- replace(prob,prob>highEps2,highEps2)
  #cat(prob)
  -sum(log(prob))
} #finds the negative sum of the log of the products of the predicted and observed data

#mle2 seems a bit more robust to singularities than mle
fit <- mle2(minuslogl=logisticfunc,start = list(a = 0,b=1),lower=c(a=-7,b=.01),upper=c(a=7,b=20),method="L-BFGS-B")
#fit<-mle2(logisticfunc,start = list(a = 1,b=2))
bestFit=numTrials*logisticfun(xs,coef(summary(fit))[1],coef(summary(fit))[2])
points(xs,bestFit,type="l",col="blue")
cat("Actual value of a = ",actualA, "Actual value of b = ",actualB)
cat("Fit value of a = ",coef(summary(fit))[1], "Actual value of b = ",coef(summary(fit))[2])
# some observations: less good parameter recovery when noise increases,  better recover of a (start point) than b (steepness) because
# very few points right at inflection point, particular if b is high
#Increasing number of trials also improves model recovery

#If binomial predicts that 0 of 200 trials will occur .9998, then if that ISN'T found, data is damned
#should consider adding in a stochastic parameter, like in softmax or a power parameter gamma in Luce Choice Axiom
#That way, we can explain why people have noise even on the extremes.
#Psychologically, there would perceptual precision (b) and momentary hiccups/randomness reflected by the determinism
