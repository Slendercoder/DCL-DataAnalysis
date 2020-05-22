source("WSpred.R")

archivo <- "../Data/humans_only_absent.csv"
df1 = read.csv(archivo, na.strings=c("","NA"))
head(df1)

args <- getFreq(df1)

fitresWSLS <- searchBestFit(args, 10)

beep()
print(fitresWSLS$value) 
para_visualizar(imprimir(fitresWSLS$par))
cat('wALL', fitresWSLS$par[1], 
    'wNOTHING', fitresWSLS$par[2],
    '\nwLEFT', fitresWSLS$par[3],
    'wIN', fitresWSLS$par[4],
    '\nalpha', fitresWSLS$par[5],
    'beta', fitresWSLS$par[6],
    'gamma', fitresWSLS$par[7]) 

