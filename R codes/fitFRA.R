source("FRApred.R")
source("Model_Plots.R")
library(dfoptim)
library(beepr)

#archivo <- "../Python Codes/Simulations/M5_full.csv"
archivo <- "../Python Codes/Simulations/N1_full.csv"

df = read.csv(archivo)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
#finding joint region
df <- find_joint_region(df)
df$RegionFULL <- unlist(df$RegionFULL)
#df <- get_FRASims(df)
beep()
head(df)

###############################################################
# Plotting...
###############################################################

alpha <- 0.3
min_score = 0
max_score = 2
theta <- c(0.001, 0.001, 0.001, 0.001, 500, 500, 32, 500, 500, 0.7)
params <- para_visualizar(imprimir(theta))

q <- plot_6panels(archivo)
regs <- c('ALL', 'DOWN', 'IN')
p <- plot_FRA_regs(df, regs)
grid.arrange(q, p, ncol=2, widths=c(2/3, 1/3))

args <- getFreqFRA(df, theta)
beep()
head(args)
#head(args)
#dim(args)

FRApred <- function(i, iV, s, j, 
                    wAll, wNoth, wLef, wIn,
                    alpha, beta, gamma, 
                    delta, epsilon, zeta) {

  # First we calculate the prior probabilities
  aux <- c(wAll, wNoth, wLef, wLef, wLef, wLef, wIn, wIn)
  # The probability of region 'RS' is 1 - the sum of the other probabilities
  if (sum(aux) > 1) {
    aux <- aux/sum(aux)
  }
  bias <- c(1 - sum(aux), aux)
  imprimir(bias)

  # Start from biases
  attractiveness <- bias
  # Add WinStay
  index <- which(regiones == i)
  # adding win stay only to focal regions
  if (i != 'RS') {
    attractiveness[index] <- attractiveness[index] + alpha * sigmoid(s, beta, gamma) 
  }
  #  print('Attractiveness with WS:')
  #  imprimir(attractiveness)
  
  similarities <- lapply(regiones[4:9], function(x) {
    f <- FRAsim(i, iV, j, x) 
    return(delta * sigmoid(f, epsilon, zeta))
  })
  similarities <- c(0, 0, 0, unlist(similarities))
  attractiveness <- attractiveness + similarities
  
  return (attractiveness)
}

wAll <- theta[1]
wNoth <- theta[2]
wLef <- theta[3]
wIn <- theta[4]
alpha <- theta[5]
beta <- theta[6]
gamma <- theta[7]
delta <- theta[8]
epsilon <- theta[9]
zeta <- theta[10]
a <- args[1,]
a <- a[c('Region', 'RegionFULL', 'Score', 'RJcode')]
x <- a$Region
y <- a$RegionFULL
z <- a$Score
u <- a$RJcode

xx <- FRApred(x, y, z, u, wAll, wNoth, wLef, wIn,
        alpha, beta, gamma,
        delta, epsilon, zeta)
imprimir(xx)

args <- args %>%
  dplyr::group_by(RegionFULL, Score, RJcode) %>%
  dplyr::mutate(probs = FRApred(Region, RegionFULL, Score, RJcode,
                                wAll, wNoth, wLef, wIn,
                                alpha, beta, gamma, 
                                delta, epsilon, zeta)) %>%
  ungroup()



f <- searchFit(theta, args)

# To search for best parameters FRA model
w1 <- 0.1 # bias ALL
w2 <- 0.1 # bias NOTHING
w3 <- 0.1 # bias LEFT
w4 <- 0.1 # bias IN
w5 <- 0.05 # alpha
w6 <- 0.5 # beta
w7 <- 0.5 # gamma
w8 <- 0.5 # delta
w9 <- 0.5 # epsilon
w10 <- 0.5 # zeta
fitresFRA <- nmkb(par=c(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10),
                   fn = function(theta) FRAutil(theta, args, regiones),
                   lower=c(0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0),
                   upper=c(0.1,
                           0.1,
                           0.1,
                           0.1,
                           500,
                           1000,
                           10),
                   control=list(trace=0))

beep()
print(fitresFRA$par) 
print(fitresFRA$value) 
dev <- fitresFRA$value

theta <- c(2.204720e-01, 1.322340e+01, 500, 0.98, 7.163690e-08, 1, 1.718298e+00, 1.2)
dev <- FRAutil(theta, args, regiones)
dev # 2126

aic <- 2*8 + dev
aic # 2142

2443 - 2246
2435 - 2230
