source("WSpred.R")

########################################################################
# Parameter recovery for WSLS
########################################################################

df1 = read.csv("../Python Codes/Model_recovery/M0_full.csv", na.strings=c("","NA"))
#head(df1)

args <- getFreq(df1)

# Effect of number of iterations in searching minima
N = 10
lista_fitvalues <- c()
x <- seq(1, N, by=1)

for (i in x) {
  
  print(i)
  fitresWSLS <- searchBestFit(args, i)
  lista_fitvalues <- c(lista_fitvalues, fitresWSLS$value)
  
}

plot(x, lista_fitvalues)
