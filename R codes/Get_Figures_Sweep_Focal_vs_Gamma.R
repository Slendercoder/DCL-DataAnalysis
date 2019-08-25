library(sjPlot)
library(sjmisc)
library(ggplot2)
library(latex2exp)
#library(dplyr)

###############################################
# Focal = 0.025
###############################################
df1 = read.csv("out_Focal0.025-Gamma0.95.csv")
df1$Exp <- as.character("0.95")
# head(df1)
df2 = read.csv("out_Focal0.025-Gamma0.975.csv")
df2$Exp <- as.character("0.975")
# head(df2)
df3 = read.csv("out_Focal0.025-Gamma0.99.csv")
df3$Exp <- as.character("0.99")
# head(df3)

df1 <- df1[complete.cases(df1), ]
df2 <- df2[complete.cases(df2), ]
df3 <- df3[complete.cases(df3), ]

df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Strategy',
        'Consistency',
        'Norm_Score_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Strategy',
        'Consistency',
        'Norm_Score_LAG1',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Strategy',
        'Consistency',
        'Norm_Score_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('0.95', '0.975', '0.99'))

dfCons <- summarySE(df, measurevar="Consistency", groupvars=c("Exp", "Round"))
head(dfCons)

g11 <- ggplot(dfCons, aes(Round, Consistency, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("Consistency") +
  ggtitle('Focal = 0.025') + 
  labs(color = TeX('$\\gamma$')) +
  ylim(c(0,1)) + 
#  theme(legend.position="bottom") +
  theme_bw()

#g11 

dfDLI <- summarySE(df, measurevar="DLIndex", groupvars=c("Round", "Exp"))
head(dfDLI)

g12 <- ggplot(dfDLI, aes(Round, DLIndex, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("DLIndex") +
  labs(color = TeX('$\\gamma$')) +
  ylim(c(0,1)) + 
  theme_bw()

#g12 

df$Strategy <- lapply(df$Strategy, function(x) {
  if(x=='0' || x=='9') {
    return('RS')
  } else if(x=='1') {
    return('ALL')
  } else if(x=='2') {
    return('NOTH')
  } else if(x=='3') {
    return('DOWN')
  } else if(x=='4') {
    return('UP')
  } else if(x=='5') {
    return('LEFT')
  } else if(x=='6') {
    return('RIGHT')
  } else if(x=='7') {
    return('IN')
  } else if(x=='8') {
    return('OUT')
  } else {
    #    print(x)
    return(as.character(x))
  }
})
df$Strategy <- unlist(df$Strategy)
df$Strategy <- as.factor(df$Strategy)
df$Strategy <- factor(df$Strategy, levels = c('RS',
                                              'ALL', 
                                              'NOTH', 
                                              'DOWN', 
                                              'UP', 
                                              'LEFT', 
                                              'RIGHT', 
                                              'IN', 
                                              'OUT'))

g13 <- ggplot(df, aes(Norm_Score_LAG1, Consistency, group=Exp)) +
  geom_point(alpha = 1/8) +
  xlim(c(0.5,1)) + 
  ylim(c(0,1)) + 
  xlab("Norm. score prev. round") +
  ylab("Consistency") +
  geom_smooth(method = lm)

#g13 <- g13 + theme_sjplot()

#g13

g14 <- ggplot(df, aes(x=Strategy,  group=Exp, fill=Exp)) + 
  geom_bar(aes(y = ..prop..), stat="count", position="dodge") +
  #  geom_text(aes(label = scales::percent(..prop..),
  #                 y= ..prop.. ), stat= "count", vjust = -.5) +
  #  labs(y = "Percent", fill="Region") +
  xlab("Region") +
  ylab("Instances (%)") +
  labs(color = TeX('$\\gamma$')) +
  #  facet_grid(~Condition) +
  #  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
#  theme(legend.position="bottom") +
  theme_bw()
  
#g14

g1 <- grid.arrange(g11, g12, g13, g14, nrow = 4)

###############################################
# Focal = 0.05
###############################################
df1 = read.csv("out_Focal0.05-Gamma0.95.csv")
df1$Exp <- as.character("0.95")
# head(df1)
df2 = read.csv("out_Focal0.05-Gamma0.975.csv")
df2$Exp <- as.character("0.975")
# head(df2)
df3 = read.csv("out_Focal0.05-Gamma0.99.csv")
df3$Exp <- as.character("0.99")
# head(df3)

df1 <- df1[complete.cases(df1), ]
df2 <- df2[complete.cases(df2), ]
df3 <- df3[complete.cases(df3), ]

df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Strategy',
        'Consistency',
        'Norm_Score_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Strategy',
        'Consistency',
        'Norm_Score_LAG1',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Strategy',
        'Consistency',
        'Norm_Score_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('0.95', '0.975', '0.99'))

dfCons <- summarySE(df, measurevar="Consistency", groupvars=c("Exp", "Round"))
head(dfCons)

g21 <- ggplot(dfCons, aes(Round, Consistency, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("Consistency") +
  ggtitle('Focal = 0.05') + 
  labs(color = TeX('$\\gamma$')) +
  ylim(c(0,1)) + 
  #  theme(legend.position="bottom") +
  theme_bw()

#g21 

dfDLI <- summarySE(df, measurevar="DLIndex", groupvars=c("Round", "Exp"))
head(dfDLI)

g22 <- ggplot(dfDLI, aes(Round, DLIndex, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("DLIndex") +
  labs(color = TeX('$\\gamma$')) +
  ylim(c(0,1)) + 
  theme_bw()

#g22 

df$Strategy <- lapply(df$Strategy, function(x) {
  if(x=='0' || x=='9') {
    return('RS')
  } else if(x=='1') {
    return('ALL')
  } else if(x=='2') {
    return('NOTH')
  } else if(x=='3') {
    return('DOWN')
  } else if(x=='4') {
    return('UP')
  } else if(x=='5') {
    return('LEFT')
  } else if(x=='6') {
    return('RIGHT')
  } else if(x=='7') {
    return('IN')
  } else if(x=='8') {
    return('OUT')
  } else {
    #    print(x)
    return(as.character(x))
  }
})
df$Strategy <- unlist(df$Strategy)
df$Strategy <- as.factor(df$Strategy)
df$Strategy <- factor(df$Strategy, levels = c('RS',
                                              'ALL', 
                                              'NOTH', 
                                              'DOWN', 
                                              'UP', 
                                              'LEFT', 
                                              'RIGHT', 
                                              'IN', 
                                              'OUT'))

g23 <- ggplot(df, aes(Norm_Score_LAG1, Consistency, group=Exp)) +
  geom_point(alpha = 1/8) +
  xlim(c(0.5,1)) + 
  ylim(c(0,1)) + 
  xlab("Norm. score prev. round") +
  ylab("Consistency") +
  geom_smooth(method = lm)

#g23 <- g23 + theme_sjplot()

#g23

g24 <- ggplot(df, aes(x=Strategy,  group=Exp, fill=Exp)) + 
  geom_bar(aes(y = ..prop..), stat="count", position="dodge") +
  #  geom_text(aes(label = scales::percent(..prop..),
  #                 y= ..prop.. ), stat= "count", vjust = -.5) +
  #  labs(y = "Percent", fill="Region") +
  xlab("Region") +
  ylab("Instances (%)") +
  labs(color = TeX('$\\gamma$')) +
  #  facet_grid(~Condition) +
  #  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  #  theme(legend.position="bottom") +
  theme_bw()

#g24

g2 <- grid.arrange(g21, g22, g23, g24, nrow = 4)

###############################################
# Focal = 0.075
###############################################
df1 = read.csv("out_Focal0.075-Gamma0.95.csv")
df1$Exp <- as.character("0.95")
# head(df1)
df2 = read.csv("out_Focal0.075-Gamma0.975.csv")
df2$Exp <- as.character("0.975")
# head(df2)
df3 = read.csv("out_Focal0.075-Gamma0.99.csv")
df3$Exp <- as.character("0.99")
# head(df3)

df1 <- df1[complete.cases(df1), ]
df2 <- df2[complete.cases(df2), ]
df3 <- df3[complete.cases(df3), ]

df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Strategy',
        'Consistency',
        'Norm_Score_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Strategy',
        'Consistency',
        'Norm_Score_LAG1',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Strategy',
        'Consistency',
        'Norm_Score_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('0.95', '0.975', '0.99'))

dfCons <- summarySE(df, measurevar="Consistency", groupvars=c("Exp", "Round"))
head(dfCons)

g31 <- ggplot(dfCons, aes(Round, Consistency, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("Consistency") +
  ggtitle('Focal = 0.075') + 
  labs(color = TeX('$\\gamma$')) +
  ylim(c(0,1)) + 
  #  theme(legend.position="bottom") +
  theme_bw()

#g31 

dfDLI <- summarySE(df, measurevar="DLIndex", groupvars=c("Round", "Exp"))
head(dfDLI)

g32 <- ggplot(dfDLI, aes(Round, DLIndex, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("DLIndex") +
  labs(color = TeX('$\\gamma$')) +
  ylim(c(0,1)) + 
  theme_bw()

#g32 

df$Strategy <- lapply(df$Strategy, function(x) {
  if(x=='0' || x=='9') {
    return('RS')
  } else if(x=='1') {
    return('ALL')
  } else if(x=='2') {
    return('NOTH')
  } else if(x=='3') {
    return('DOWN')
  } else if(x=='4') {
    return('UP')
  } else if(x=='5') {
    return('LEFT')
  } else if(x=='6') {
    return('RIGHT')
  } else if(x=='7') {
    return('IN')
  } else if(x=='8') {
    return('OUT')
  } else {
    #    print(x)
    return(as.character(x))
  }
})
df$Strategy <- unlist(df$Strategy)
df$Strategy <- as.factor(df$Strategy)
df$Strategy <- factor(df$Strategy, levels = c('RS',
                                              'ALL', 
                                              'NOTH', 
                                              'DOWN', 
                                              'UP', 
                                              'LEFT', 
                                              'RIGHT', 
                                              'IN', 
                                              'OUT'))

g33 <- ggplot(df, aes(Norm_Score_LAG1, Consistency, group=Exp)) +
  geom_point(alpha = 1/8) +
  xlim(c(0.5,1)) + 
  ylim(c(0,1)) + 
  xlab("Norm. score prev. round") +
  ylab("Consistency") +
  geom_smooth(method = lm)

#g33 <- g33 + theme_sjplot()

#g33

g34 <- ggplot(df, aes(x=Strategy,  group=Exp, fill=Exp)) + 
  geom_bar(aes(y = ..prop..), stat="count", position="dodge") +
  #  geom_text(aes(label = scales::percent(..prop..),
  #                 y= ..prop.. ), stat= "count", vjust = -.5) +
  #  labs(y = "Percent", fill="Region") +
  xlab("Region") +
  ylab("Instances (%)") +
  labs(color = TeX('$\\gamma$')) +
  #  facet_grid(~Condition) +
  #  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  #  theme(legend.position="bottom") +
  theme_bw()

#g34

g3 <- grid.arrange(g31, g32, g33, g34, nrow = 4)

g <- grid.arrange(g1, g2, g3, ncol = 3)

# ggsave("ConsistencyWRTDist2FR.eps", width=3.5, height=3.5, device=cairo_ps, g3)
