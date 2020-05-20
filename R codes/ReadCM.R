library(janitor)
library(dplyr)
library(ggplot2)
library(gridExtra)

##############

Columns <- c('Model', 'Num', 
             'wALL', 'wNOTHING', 'wLEFT', 'wIN', 
             'Alpha', 'Beta', 'Gamma', 
             'Delta', 'Epsilon', 'Zeta', 'Dev')
fittedPars <- data.frame(t(c('MB', contador, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
colnames(fittedPars) <- Columns
fittedPars <- fittedPars[-1, ]
a <- seq(0, 3)
for (contador in a) {
  pars <- c('MB', contador, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  archivo <- paste("MBiases_Parameter_fit_nmkb_", contador, ".csv", sep="")
  df = read.csv(archivo)
  df$par <- as.double(df$par)
  aux <- df$par
  pars[3:6] <- aux
  pars[13] <- unique(df$value)
  aux <- data.frame(t(pars))
  colnames(aux) <- Columns
  fittedPars <- rbind(fittedPars, aux)
}
for (contador in a) {
  pars <- c('WS', contador, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  archivo <- paste("WSLS_Parameter_fit_nmkb_", contador, ".csv", sep="")
  df = read.csv(archivo)
  df$par <- as.double(df$par)
  aux <- df$par
  pars[3:9] <- aux
  pars[13] <- unique(df$value)
  aux <- data.frame(t(pars))
  colnames(aux) <- Columns
  fittedPars <- rbind(fittedPars, aux)
}
for (contador in a) {
  pars <- c('FR', contador, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  archivo <- paste("FRA_Parameter_fit_nmkb_", contador, ".csv", sep="")
  df = read.csv(archivo)
  df$par <- as.double(df$par)
  aux <- df$par
  pars[3:12] <- aux
  pars[13] <- unique(df$value)
  aux <- data.frame(t(pars))
  colnames(aux) <- Columns
  fittedPars <- rbind(fittedPars, aux)
}
fittedPars$Exp <- as.character("Fitted")
head(fittedPars)
dfDevs <- fittedPars %>%
  select('Num', 'Model', 'Dev')
fittedPars <- fittedPars %>%
  select('Model', 'Num', 
         'wALL', 'wNOTHING', 'wLEFT', 'wIN', 
         'Alpha', 'Beta', 'Gamma', 
         'Delta', 'Epsilon', 'Zeta')

archivo <- '../Data/Confusion/sim_data_rel.csv'
realPars = read.csv(archivo)
realPars$Exp <- as.character("Real")
realPars <- realPars[realPars$Num < 4, ]
df <- rbind(realPars, fittedPars)
head(df)
data <- df
data$wALL <- as.double(data$wALL)
data$wNOTHING <- as.double(data$wNOTHING)
data$wLEFT <- as.double(data$wLEFT)
data$wIN <- as.double(data$wIN)
data$Alpha <- as.double(data$Alpha)
data$Beta <- as.double(data$Beta)
data$Gamma <- as.double(data$Gamma)
data$Delta <- as.double(data$Delta)
data$Epsilon <- as.double(data$Epsilon)
data$Zeta <- as.double(data$Zeta)
data <- data[order(data$Model, data$Num), ]
head(data)

data_MB <- data[data$Model == 'MB', ]
data_WS <- data[data$Model == 'WS', ]
data_FR <- data[data$Model == 'FR', ]

# Drawing biases parameters fit
p1 <- plot_Parameter_Recovery_Biases(data_MB, "MBiases")
p2 <- plot_Parameter_Recovery_Biases(data_WS, "WSLS")
p3 <- plot_Parameter_Recovery_Biases(data_FR, "FRA")
pBiases <- grid.arrange(p1, p2, p3, nrow=3)

# Drawing WSLS parameters fit
p2 <- plot_Parameter_Recovery_WSLS(data_WS, "WSLS")
p3 <- plot_Parameter_Recovery_WSLS(data_FR, "FRA")
pWSLS <- grid.arrange(p2, p3, nrow=2)

# Drawing FRA parameters fit
pFRA <- plot_Parameter_Recovery_FRA(data_FR, "FRA")

#################################
# Confusion matrix
#################################

dfDevs
