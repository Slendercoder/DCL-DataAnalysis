source("WSpred.R")
library(ggplot2)
library(dplyr)
library(gridExtra)
library(Rmisc)

###########################################################################################

#min_score = -50
min_score = 0
alpha <- 0.3

###########################################################################################

#df2 = read.csv("../Python Codes/Model_recovery/M5_full.csv")
#df2 = read.csv("../Python Codes/Data_correction/Only_absent/M5_onlyA.csv")
#df2 = read.csv("../Python Codes/Data_correction/Score_correction/M5_ScoreC.csv")
df2 = read.csv("../Python Codes/Data_correction/Block_estimation/M5_Estimated.csv")
df2$Region <- df2$Category
#head(df2)

#model1wsls <- lm(Consistency ~ Score_LAG1, data = df2)
#summary(model1wsls) # => Positive correlation is significant

df1 <- df2[df2$Category != 'RS', ]
df1 <- df1[df1$Category_LAG1 != 'RS', ]

#model2wsls <- lm(Consistency ~ Score_LAG1, data = df1)
#summary(model2wsls) # => Positive correlation is significant

############################
# Obtaining frequencies...
############################

df <- getRelFreq_Rows(df1)
head(df)

############################
# Ploting data...
############################

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


#alpha <- 0.3
theta <- c(0.05, 0.05, 0.05, 0.05, 500, 500, 32)

d1 <- plot_RSTransitions(df)
d1 <- plot_ModelTransitions_RS(theta, d1,"#999999")

d2 <- plot_FocalTransitions(df)
d2 <- plot_ModelTransitions_Focal(theta, d2,"#999999")

params <- para_visualizar(theta)
gB <- grid.arrange(p1, d1, g1, p2, d2, g2, nrow=2,
                   bottom=params)

gB <- grid.arrange(p1, p2, d1, d2, nrow=2,
                   bottom=params)
