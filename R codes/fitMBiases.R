source("MBIASESpred.R")
library(dfoptim)
library(bbmle)
library(beepr)

df1 = read.csv("../Data/MBiases.csv", na.strings=c("","NA"))
df1$Category <- factor(df1$Category, levels = regiones)
a <- data.frame(table(df1$Category))
freqs <- a$Freq
theta <- c(0.05, 0.05, 0.05, 0.05)

f <- searchBestFit(freqs, N=5)
