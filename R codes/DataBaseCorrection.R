library(stats4)
library(bbmle)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(beepr)

#####################################################
# Definitions
#####################################################

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

#####################################################
# Global variables
#####################################################

# True parameters: 
thetaTRUE <- c(0.1, 0.1, 0.05, 0.05, 150, 10, 31, 0, 0, 0, 0)
# Estimated parameters:
theta <- c(0.090, 0.091, 0.046, 0.046, 7.372,311.871, 31.209, 0, 0, 0, 0) # Estimated only absent

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
# Loading database with full information
###############################################################################

df1 = read.csv("../Python Codes/fullWSLS2BRecovered.csv", na.strings=c("","NA"))
#df1 = read.csv("../Python Codes/output.csv", na.strings=c("","NA"))
df1$Region <- df1$Category
#df1$Region <- sapply(df1$Strategy, Nombre_Region)
#df1 <- df1 %>% 
#  group_by(Player) %>%
#  mutate(RegionGo = lead(Region)) %>%
#  as.data.frame()
df1 <- df1[complete.cases(df1), ]
df1 <- df1[c('Dyad', 'Player', 'Is_there', 'Region', 'Score', 'RegionGo')]
head(df1[, 3:6])

###############################################################################
# Loading database with only absent
###############################################################################

#df2 = read.csv("../Python Codes/Only_Absent.csv", na.strings=c("","NA"))
df2 = read.csv("../Python Codes/output.csv", na.strings=c("","NA"))
df2 <- df2[complete.cases(df2), ]
df2$Region <- df2$Category
df2 <- df2[c('Dyad', 'Player', 'Is_there', 'Region', 'Score', 'RegionGo')]
df2$Region <- factor(df2$Region, levels = regiones, ordered = TRUE)
df2$RegionGo <- factor(df2$RegionGo, levels = regiones, ordered = TRUE)
df2 <- df2[order(df2$Region, df2$Score), ] 
head(df2[, 3:6])

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
head(dfA)

dfB <- df2[, 3:6]
dfB$Freqs <- apply(dfB, 1, function(x) {
  i <- as.character(x[[2]][1])
  s <- as.numeric(x[[3]][1])
  k <- as.character(x[[4]][1])
  #cat('\ni', i, 's', s, 'k', k)
  return(getRelFreq(i, s, k, df2))
})
dfB <- unique(dfB)
head(dfB)

dfA$Exp <- "Full"
dfB$Exp <- "Only absent"

df <- rbind(dfA, dfB)
#df <- dfA

beep()

###############################################################################
# Graph effect of keeping only absent...
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
  geom_point(aes(x = Score, y = Freqs, color=Exp, shape=Is_there), df_RS, alpha = 0.8, size=2) +
  #  scale_colour_manual(values = c("RS" = "#999999", 
  #                                 "ALL" = "#E69F00")) +  
  #  scale_shape_manual(values = c("RS" = 1, 
  #                                "ALL" = 2),
  #                     name="Jumps to") +  
  scale_x_continuous(limits = c(min_score, 35)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  labs(color = "Source of data") +
  xlab("Score") +
  ylab("") +
  #  ylab("Rel. Freq./Probability") +
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
  geom_point(aes(x = Score, y = Freqs, color=Exp, shape=Is_there), df_RS, alpha = 0.8, size=2) +
  #  scale_colour_manual(values = c("RS" = "#999999", 
  #                                 "ALL" = "#E69F00")) +  
  #  scale_shape_manual(values = c("RS" = 1, 
  #                                "ALL" = 2),
  #                     name="Jumps to") +  
  scale_x_continuous(limits = c(min_score, 35)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  labs(color = "Source of data") +
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
  geom_point(aes(x = Score, y = Freqs, color=Exp, shape=Is_there), df_ALL, alpha = 0.8, size=2) +
  #  scale_colour_manual(values = c("RS" = "#999999", 
  #                                 "ALL" = "#E69F00")) +  
  #  scale_shape_manual(values = c("RS" = 1, 
  #                                "ALL" = 2),
  #                     name="Jumps to") +  
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  labs(color = "Source of data") +
  xlab("Score") +
  ylab("") +
  #  ylab("Rel. Freq./Probability") +
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
  geom_point(aes(x = Score, y = Freqs, color=Exp, shape=Is_there), df_ALL, alpha = 0.8, size=2) +
#  scale_colour_manual(values = c("RS" = "#999999", 
#                                 "ALL" = "#E69F00")) +  
#  scale_shape_manual(values = c("RS" = 1, 
#                                "ALL" = 2),
#                     name="Jumps to") +  
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  labs(color = "Source of data") +
  xlab("Score") +
  ylab("") +
  #  ylab("Rel. Freq./Probability") +
  ggtitle("Transition from ALL to ALL") +
  theme_bw()

gALL2ALL


legend <- get_legend(gRS2RS)
gRS2RS <- gRS2RS + theme(legend.position="none")
gALL2RS <- gALL2RS + theme(legend.position="none")
gRS2ALL <- gRS2ALL + theme(legend.position="none")
gALL2ALL <- gALL2ALL + theme(legend.position="none")

grid.arrange(gRS2RS, gRS2ALL, gALL2RS, gALL2ALL,
             nrow = 2, 
             right=legend, 
             top="Transitions - Only data")


#################################################
# Including models on top of data plots
#################################################

# Transition from RS to RS
#xs <- seq(-128,32,length.out=161)
xs <- seq(min_score,32,length.out=(32-min_score + 1))
# Model
fitTRUE <- sapply(xs, WSprob, i='RS', k='RS', theta=thetaTRUE, regiones=regiones)
# Model fitted from only absent
fitEST <- sapply(xs, WSprob, i='RS', k='RS', theta=theta, regiones=regiones)
dfmodels <- data.frame(xs, fitTRUE, fitEST)
head(dfmodels)

# Dummy plot to get legend
dummyplot <- ggplot() +
  geom_line(aes(x = xs, y = fitTRUE, color="Original"), dfmodels, size = tamanho) + 
  geom_line(aes(x = xs, y = fitEST, color = "Recovered"), dfmodels, size = tamanho) + 
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  scale_color_manual(values=c("Original"="#F0E442",
                                "Recovered"="#009E73"),
                       name="Transition")  +
  theme_bw() +
  theme(legend.position="bottom")

legend2 <- get_legend(dummyplot)

gRS2RS <- gRS2RS +
  geom_line(aes(x = xs, y = fitTRUE), dfmodels, size = tamanho, color="#F0E442") + 
  geom_line(aes(x = xs, y = fitEST), dfmodels, size = tamanho, color="#009E73") + 
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  theme_bw() +
  theme(legend.position="none")

gRS2RS

# Transition from RS to ALL
#xs <- seq(-128,32,length.out=161)
xs <- seq(min_score,32,length.out=(32-min_score + 1))
# Model
fitTRUE <- sapply(xs, WSprob, i='RS', k='ALL', theta=thetaTRUE, regiones=regiones)
# Model fitted from only absent
fitEST <- sapply(xs, WSprob, i='RS', k='ALL', theta=theta, regiones=regiones)
dfmodels <- data.frame(xs, fitTRUE, fitEST)
head(dfmodels)

gRS2ALL <- gRS2ALL +
  geom_line(aes(x = xs, y = fitTRUE), dfmodels, size = tamanho, color="#F0E442") + 
  geom_line(aes(x = xs, y = fitEST), dfmodels, size = tamanho, color="#009E73") + 
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  theme_bw() +
  theme(legend.position="none")

gRS2ALL

# Transition from ALL to RS
#xs <- seq(-128,32,length.out=161)
xs <- seq(min_score,32,length.out=(32-min_score + 1))
# Model
fitTRUE <- sapply(xs, WSprob, i='ALL', k='RS', theta=thetaTRUE, regiones=regiones)
# Model fitted from only absent
fitEST <- sapply(xs, WSprob, i='ALL', k='RS', theta=theta, regiones=regiones)
dfmodels <- data.frame(xs, fitTRUE, fitEST)
head(dfmodels)

gALL2RS <- gALL2RS +
  geom_line(aes(x = xs, y = fitTRUE), dfmodels, size = tamanho, color="#F0E442") + 
  geom_line(aes(x = xs, y = fitEST), dfmodels, size = tamanho, color="#009E73") + 
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  theme_bw() +
  theme(legend.position="none")

gALL2RS

# Transition from ALL to ALL
#xs <- seq(-128,32,length.out=161)
xs <- seq(min_score,32,length.out=(32-min_score + 1))
# Model
fitTRUE <- sapply(xs, WSprob, i='ALL', k='ALL', theta=thetaTRUE, regiones=regiones)
# Model fitted from only absent
fitEST <- sapply(xs, WSprob, i='ALL', k='ALL', theta=theta, regiones=regiones)
dfmodels <- data.frame(xs, fitTRUE, fitEST)
head(dfmodels)

gALL2ALL <- gALL2ALL +
  geom_line(aes(x = xs, y = fitTRUE), dfmodels, size = tamanho, color="#F0E442") + 
  geom_line(aes(x = xs, y = fitEST), dfmodels, size = tamanho, color="#009E73") + 
  scale_x_continuous(limits = c(min_score, 33)) + 
  scale_y_continuous(limits = c(0, 1.01)) + 
  theme_bw() +
  theme(legend.position="none")

gALL2ALL

grid.arrange(gRS2RS, gRS2ALL, gALL2RS, gALL2ALL,
             nrow = 2, 
             right=legend, 
             bottom=legend2,
             top="Transitions - Model recovery")
 