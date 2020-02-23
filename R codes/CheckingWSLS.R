library(ggplot2)
library(dplyr)
library(gridExtra)

###########################################################################################
###########################################################################################

plot_RSTransitions <- function(df) {

  df_RS <- df[df$Region == 'RS', ]
  head(df_RS)
  
  gRS <- ggplot() +
    geom_point(aes(x = Score, y = Freqs, color=RegionGo), df_RS, alpha = 0.4, size=1.5) +
    scale_x_continuous(limits = c(min_score, 35)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab("Score") +
    #  ylab("") +
    ylab("Rel. Freq./Probability") +
    ggtitle("Transitions from RS") +
    theme_bw()
  
  gRS
  
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
    geom_point(aes(x = Score, y = Freqs), df_RS, alpha = alpha, size=1.5) +
    scale_x_continuous(limits = c(min_score, 35)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab("Score") +
    #  ylab("") +
    ylab("Rel. Freq./Probability") +
    ggtitle("Staying at RS") +
    theme_bw()
  
  return (gRS2RS)

}

plot_FocalTransitions <- function(df) {
  
  df <- df[df$Region != 'RS', ]

  regiones <- c('ALL', 'NOTHING', 
                'DOWN', 'UP', 'LEFT', 'RIGHT',
                'IN', 'OUT')
  
  gOTHER2OTHER <- ggplot() +
    scale_x_continuous(limits = c(min_score, 35)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab("Score") +
    #  ylab("") +
    ylab("Rel. Freq./Probability") +
    ggtitle("Staying at same focal region") +
    theme_bw()
  
  
  #other <- 'RIGHT'
  #other <- 'LEFT'
  for (other in regiones) {
    df_Focal <- df[df$Region == other, ]
    df_Focal <- df_Focal[df_Focal$RegionGo == other, ]
    gOTHER2OTHER <- gOTHER2OTHER +
      geom_point(aes(x = Score, y = Freqs), df_Focal, alpha = alpha, size=1.5)
  }

  return (gOTHER2OTHER)
  
}

# Returns the value of the sigmoid function 1/(1+exp(b(x-c)))
sigmoid <- function(x, beta, gamma) {1 / (1 + exp(-beta * (x - gamma)))}

WSprob <- function(i, s, k, theta){

  regiones <- c('RS', 
                'ALL', 'NOTHING', 
                'DOWN', 'UP', 'LEFT', 'RIGHT', 
                'IN', 'OUT')
  
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
  #  print("bias")
  #  imprimir(bias)
  
  # Find the attractivenes:
  attractiveness <- bias # Start from bias
  
  # Add attractiveness to current region according to score
  if (i != 'RS') {
    index <- which(regiones == i)
    attractiveness[index] <- attractiveness[index] + alpha * sigmoid(s, beta, gamma) 
  }
  
  probs <- attractiveness / sum(attractiveness)
  #  probs <- replace(probs,probs<lowerEps2,lowerEps2)
  #  probs <- replace(probs,probs>highEps2,highEps2)
  
  probab <- probs[which(regiones == k)]
  
  return(probab)
}

plot_ModelTransitions_RS <- function(theta, pl) {
  
  xs <- seq(-128,32,length.out=161)
  fitRS <- sapply(xs, WSprob, i='RS', k='RS', theta=theta)
  dfB <- data.frame(xs, fitRS)
  pl <- pl +
    geom_line(aes(x = xs, y = fitRS), dfB, color="#999999")
  
  return(pl)
}

plot_ModelTransitions_Focal <- function(theta, pl) {
  
  xs <- seq(-128,32,length.out=161)
  regiones <- c('ALL', 'NOTHING', 
                'DOWN', 'UP', 'LEFT', 'RIGHT',
                'IN', 'OUT')
  for (other in regiones) {
    fitFocal <- sapply(xs, WSprob, i=other, k=other, theta=theta)
    dfB <- data.frame(xs, fitFocal)
    pl <- pl +
      geom_line(aes(x = xs, y = fitFocal), dfB, color="#999999")
  }
  
  return(pl)
}

para_visualizar <- function(theta) {
  a <- as.character(theta)
  params <- ''
  for (letra in a) {
    params <- paste(params, letra, ';')
  }
  return(params)
}


###########################################################################################
###########################################################################################

df2 = read.csv("../Python Codes/output.csv")
df2$Region <- df2$Category
head(df2)

#model1wsls <- lm(Consistency ~ Score_LAG1, data = df2)
#summary(model1wsls) # => Positive correlation is significant

df1 <- df2[df2$Category != 'RS', ]
df1 <- df1[df1$Category_LAG1 != 'RS', ]

#model2wsls <- lm(Consistency ~ Score_LAG1, data = df1)
#summary(model2wsls) # => Positive correlation is significant

############################
# Obtaining frequencies...
############################

df2 <- df2[df2$RegionGo != "", ]
dfA <- df2 %>% select('Region', 'Score', 'RegionGo')
df <- dfA %>%
  dplyr::group_by(Region, Score, RegionGo) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Region, Score) %>%
  dplyr::mutate(n1 = sum(n),
                Freqs = n/n1)

head(df)

############################
# Ploting data...
############################

#min_score = -50
min_score = 0
alpha <- 0.1

# Regression full data
p1 <- ggplot(df2, aes(x = Score_LAG1, y = Consistency)) + 
  geom_point(shape = 19, alpha = alpha) + 
  geom_smooth(method='lm', se=FALSE) + 
  theme_bw() +
  xlab("Score(n-1)") +
  ylab("Consistency(n)") +
  xlim(c(min_score,32)) +
  ylim(c(0,1)) +
  ggtitle('Full data')

# Regression only focal regions
p2 <- ggplot(df1, aes(x = Score_LAG1, y = Consistency)) + 
  geom_point(shape = 19, alpha = alpha) + 
  geom_smooth(method='lm', se=FALSE) + 
  theme_bw() +
  xlab("Score(n-1)") +
  ylab("Consistency(n)") +
  xlim(c(min_score,32)) +
  ylim(c(0,1)) +
  ggtitle('Only focal regions')

# Summarize data
dfc_DLIndex <- summarySE(df2, measurevar="DLIndex", groupvars="Round")
head(dfc_DLIndex)

# Plot DLIndex with error regions
g1 <- ggplot(dfc_DLIndex, aes(x=Round, y=DLIndex)) +
  geom_line(size=0.7) +
  geom_ribbon(aes(ymin = DLIndex - sd,
                  ymax = DLIndex + sd), alpha = 0.2) +
  #  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
  labs(color = "Source") +
  xlab("Round (unicorn absent)") +
  ylab("DLindex") +
  ylim(c(0,1.2)) +
  ggtitle('Division of labor') +
  theme_bw()

# Density plot
g2 <- ggplot(df2, aes(DLIndex)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 5)) + 
  labs(color = "Source of data") +
  ggtitle('Kernel density estimate') +
  theme_bw()

alpha <- 0.3
theta <- c(0, 0, 0, 0.1, 500, 500, 32)

d1 <- plot_RSTransitions(df)
d1 <- plot_ModelTransitions_RS(theta, d1)

d2 <- plot_FocalTransitions(df)
d2 <- plot_ModelTransitions_Focal(theta, d2)

params <- para_visualizar(theta)
gB <- grid.arrange(p1, d1, g1, p2, d2, g2, nrow=2,
                   bottom=params)

