source("Model_Plots.R")
library(janitor)
library(dplyr)
library(ggplot2)
library(gridExtra)

##############

Columns <- c('Model', 'Num', 
             'wALL', 'wNOTHING', 'wLEFT', 'wIN', 
             'Alpha', 'Beta', 'Gamma', 
             'Delta', 'Epsilon', 'Zeta', 'Dev')
fittedPars <- data.frame(t(c('MB', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
colnames(fittedPars) <- Columns
fittedPars <- fittedPars[-1, ]
a <- seq(0, 9)
for (contador in a) {
  pars <- c('MB', contador, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  rotulo <- paste('MB', contador, sep="")
  archivo <- paste("../Data/Confusion/Estimations/MBiases_Parameter_fit_nmkb_", rotulo, ".csv", sep="")
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
  rotulo <- paste('WS', contador, sep="")
  archivo <- paste("../Data/Confusion/Estimations/WSLS_Parameter_fit_nmkb_", rotulo, ".csv", sep="")
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
  rotulo <- paste('FR', contador, sep="")
  archivo <- paste("../Data/Confusion/Estimations/FRA_Parameter_fit_nmkb_", rotulo, ".csv", sep="")
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
fittedPars <- fittedPars %>%
  select('Num', 'Model', 
         'wALL', 'wNOTHING', 'wLEFT', 'wIN', 
         'Alpha', 'Beta', 'Gamma', 
         'Delta', 'Epsilon', 'Zeta', 'Exp')
head(fittedPars)

archivo <- '../Data/Confusion/Simulations/sim_data_rel.csv'
realPars = read.csv(archivo)
realPars$Exp <- as.character("Real")
head(realPars)

dim(fittedPars)
dim(realPars)
# realPars <- realPars[1:16, ]
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

# Drawing parameter fit biases
p1 <- plot_Parameter_Recovery_Biases(data_MB, "MBiases")
p2 <- plot_Parameter_Recovery_Biases(data_WS, "WSLS")
p3 <- plot_Parameter_Recovery_Biases(data_FR, "FRA")
pMBiases <- grid.arrange(p1, p2, p3, nrow=3)

# Drawing parameter fit WSLS
p1 <- plot_Parameter_Recovery_WSLS(data_WS, "WSLS")
p2 <- plot_Parameter_Recovery_WSLS(data_FR, "FRA")
pWSLS <- grid.arrange(p1, p2, nrow=2)

# Drawing parameter fit FRA
pFRA <- plot_Parameter_Recovery_FRA(data_FR, "FRA")

#################################
# Confusion matrix
#################################

Columns <- c('ModelReal', 'Num', 'ModelFitted', 'Dev')
fittedDev <- data.frame(t(c('MB', 0, 'MB', 0)))
colnames(fittedDev) <- Columns
fittedDev <- fittedDev[-1, ]
a <- seq(0, 9)
for (contador in a) {
  
  pars <- c('MB', contador, 'MB', 0)
  rotulo <- paste('MB', contador, sep="")
  archivo <- paste("../Data/Confusion/Estimations/MBiases_Parameter_fit_nmkb_", rotulo, ".csv", sep="")
  df = read.csv(archivo)
  df$value <- as.double(df$value)
  pars[4] <- unique(df$value)
  aux <- data.frame(t(pars))
  colnames(aux) <- Columns
  fittedDev <- rbind(fittedDev, aux)

  pars <- c('MB', contador, 'WS', 0)
  rotulo <- paste('WS', contador, sep="")
  archivo <- paste("../Data/Confusion/Estimations/MBiases_Parameter_fit_nmkb_", rotulo, ".csv", sep="")
  df = read.csv(archivo)
  df$value <- as.double(df$value)
  pars[4] <- unique(df$value)
  aux <- data.frame(t(pars))
  colnames(aux) <- Columns
  fittedDev <- rbind(fittedDev, aux)
  
  pars <- c('MB', contador, 'FR', 0)
  rotulo <- paste('FR', contador, sep="")
  archivo <- paste("../Data/Confusion/Estimations/MBiases_Parameter_fit_nmkb_", rotulo, ".csv", sep="")
  df = read.csv(archivo)
  df$value <- as.double(df$value)
  pars[4] <- unique(df$value)
  aux <- data.frame(t(pars))
  colnames(aux) <- Columns
  fittedDev <- rbind(fittedDev, aux)

  pars <- c('WS', contador, 'MB', 0)
  rotulo <- paste('MB', contador, sep="")
  archivo <- paste("../Data/Confusion/Estimations/WSLS_Parameter_fit_nmkb_", rotulo, ".csv", sep="")
  df = read.csv(archivo)
  df$value <- as.double(df$value)
  pars[4] <- unique(df$value)
  aux <- data.frame(t(pars))
  colnames(aux) <- Columns
  fittedDev <- rbind(fittedDev, aux)
  
  pars <- c('WS', contador, 'WS', 0)
  rotulo <- paste('WS', contador, sep="")
  archivo <- paste("../Data/Confusion/Estimations/WSLS_Parameter_fit_nmkb_", rotulo, ".csv", sep="")
  df = read.csv(archivo)
  df$value <- as.double(df$value)
  pars[4] <- unique(df$value)
  aux <- data.frame(t(pars))
  colnames(aux) <- Columns
  fittedDev <- rbind(fittedDev, aux)
  
  pars <- c('WS', contador, 'FR', 0)
  rotulo <- paste('FR', contador, sep="")
  archivo <- paste("../Data/Confusion/Estimations/WSLS_Parameter_fit_nmkb_", rotulo, ".csv", sep="")
  df = read.csv(archivo)
  df$value <- as.double(df$value)
  pars[4] <- unique(df$value)
  aux <- data.frame(t(pars))
  colnames(aux) <- Columns
  fittedDev <- rbind(fittedDev, aux)
  
  pars <- c('FR', contador, 'MB', 0)
  rotulo <- paste('MB', contador, sep="")
  archivo <- paste("../Data/Confusion/Estimations/FRA_Parameter_fit_nmkb_", rotulo, ".csv", sep="")
  df = read.csv(archivo)
  df$value <- as.double(df$value)
  pars[4] <- unique(df$value)
  aux <- data.frame(t(pars))
  colnames(aux) <- Columns
  fittedDev <- rbind(fittedDev, aux)
  
  pars <- c('FR', contador, 'WS', 0)
  rotulo <- paste('WS', contador, sep="")
  archivo <- paste("../Data/Confusion/Estimations/FRA_Parameter_fit_nmkb_", rotulo, ".csv", sep="")
  df = read.csv(archivo)
  df$value <- as.double(df$value)
  pars[4] <- unique(df$value)
  aux <- data.frame(t(pars))
  colnames(aux) <- Columns
  fittedDev <- rbind(fittedDev, aux)
  
  pars <- c('FR', contador, 'FR', 0)
  rotulo <- paste('FR', contador, sep="")
  archivo <- paste("../Data/Confusion/Estimations/FRA_Parameter_fit_nmkb_", rotulo, ".csv", sep="")
  df = read.csv(archivo)
  df$value <- as.double(df$value)
  pars[4] <- unique(df$value)
  aux <- data.frame(t(pars))
  colnames(aux) <- Columns
  fittedDev <- rbind(fittedDev, aux)
  
}
head(fittedDev)
aux <- fittedDev
fittedDev <- aux
fittedDev$Num <- type.convert(fittedDev$Num)
fittedDev$Dev <- type.convert(fittedDev$Dev)
fittedDev$Simulated_Model <- factor(fittedDev$ModelReal, 
                                   levels=c('MB', 'WS', 'FR'))
fittedDev$ModelFitted <- factor(fittedDev$ModelFitted, 
                                    levels=c('MB', 'WS', 'FR'))
fittedDev <- fittedDev %>%
  dplyr::group_by(Num, Simulated_Model) %>%
  dplyr::summarize(Fit_Model = which(Dev == min(Dev))[1])

fittedDev$Fit_Model <- lapply(fittedDev$Fit_Model, function(x) {
  if(x==1) {
    return('MB')
  } else if(x==2) {
    return('WS')
  } else if(x==3) {
    return('FR')
  }
})
fittedDev$Fit_Model <- unlist(fittedDev$Fit_Model)
fittedDev$Fit_Model <- factor(fittedDev$Fit_Model, 
                        levels=c('MB', 'WS', 'FR'))
head(fittedDev)
table(fittedDev[2:3])
