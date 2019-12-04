source("WSpred_full.R")
library(dfoptim)
library(bbmle)
library(beepr)
library(dplyr)

################################################################
# To use with data estimated from only absent
################################################################

#df1 = read.csv("../Python Codes/Sweeps/Only_Absent/sim200_10.csv", na.strings=c("","NA"))
df1 = read.csv("../Python Codes/WSLS2BRecovered.csv", na.strings=c("","NA"))
df1 <- df1[complete.cases(df1), ]
df1$Region <- df1$Category
df1 <- df1[c('Dyad', 'Player', 'Region', 'Score', 'RegionGo')]
#head(df1)

args <- getArgs(df1)
args <- args[order(-args$s, args$i),] 
args <- args[c('pair', 'freq', 'sumFreq')]
args <- args[args$sumFreq > 5, ]
head(args)

#args1 <- args
#args1[10:19, ]

################################################################
# To use with data estimated from full information
################################################################

#df1 = read.csv("../Python Codes/Sweeps/Full/sim200_10.csv", na.strings=c("","NA"))
df2 = read.csv("../Python Codes/fullWSLS2BRecovered.csv", na.strings=c("","NA"))
df2$Region <- sapply(df2$Strategy, Nombre_Region)

df2 <- df2 %>% 
  group_by(Player) %>%
  mutate(RegionGo = lead(Region)) %>%
  as.data.frame()

df2 <- df2[complete.cases(df2), ]
df2 <- df2[c('Dyad', 'Player', 'Region', 'Score', 'RegionGo')]
head(df2)
args <- getArgs(df2)
args <- args[order(-args$s, args$i),] 
args <- args[c('pair', 'freq', 'sumFreq')]
args <- args[args$sumFreq > 5, ]
head(args)

#args[10:19, ]

################################################################
# To search for best parameters WSLS model with optim
################################################################

wAll <- 0.01 # w
wNoth <- 0.01 # w
wLef <- 0.01 # w
wIn <- 0.01 # w
alpha <- 10 # win stay 
beta <- 100 # steepness 
gamma <- 20 # threshold 
fitresWSLS <- nmkb(par=c(wAll, wNoth, wLef, wIn, alpha, beta, gamma),
                   fn = function(theta) WSutil(c(theta, 0, 0, 0, 0), args, regiones),
                   lower=c(0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0),
                   upper=c(0.15,
                           0.15,
                           0.15,
                           0.15,
                           500,
                           500,
                           32),
                   control=list(trace=0))

beep()
imprimir(fitresWSLS$par) 
cat('wALL', fitresWSLS$par[1], 
    'wNOTHING', fitresWSLS$par[2],
    '\nwLEFT', fitresWSLS$par[3],
    'wIN', fitresWSLS$par[4],
    '\nalpha', fitresWSLS$par[5],
    'beta', fitresWSLS$par[6],
    'gamma', fitresWSLS$par[7], '\n') 
print(fitresWSLS$value) 

