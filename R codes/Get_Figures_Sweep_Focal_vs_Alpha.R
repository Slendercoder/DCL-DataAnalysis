library(ggplot2)
library(sjPlot)
library(sjmisc)
library(gridExtra)
library(grid)
library(Rmisc)
library(latex2exp)
library(squash)
#library(dplyr)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

###############################################
# Focal = 0.025
###############################################
df1 = read.csv("out_Focal0.025-Alpha0.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Focal0.025-Alpha70.csv")
df2$Exp <- as.character("70")
# head(df2)
df3 = read.csv("out_Focal0.025-Alpha150.csv")
df3$Exp <- as.character("150")
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
df$Exp <- factor(df$Exp, levels = c('0', '70', '150'))

dfCons <- summarySE(df, measurevar="Consistency", groupvars=c("Exp", "Round"))
# head(dfCons)

g11 <- ggplot(dfCons, aes(Round, Consistency, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("Consistency") +
  ggtitle(TeX('bias$_{RS}=0.8')) + 
  labs(color = TeX('$\\alpha$')) +
  ylim(c(0,1)) + 
#  theme(legend.position="bottom") +
  theme_bw()

#g11 

dfDLI <- summarySE(df, measurevar="DLIndex", groupvars=c("Round", "Exp"))
# head(dfDLI)

g12 <- ggplot(dfDLI, aes(Round, DLIndex, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("DLIndex") +
  labs(color = TeX('$\\alpha$')) +
  ylim(c(0,1)) + 
  theme_bw()

#g12 

df$Strategy <- lapply(df$Strategy, function(x) {
  if(x=='0' || x=='9') {
    return('RS')
  } else if(x=='1') {
    return('ALL')
  } else if(x=='2') {
    return('NOT')
  } else if(x=='3') {
    return('DOW')
  } else if(x=='4') {
    return('UP')
  } else if(x=='5') {
    return('LEF')
  } else if(x=='6') {
    return('RIG')
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
                                              'NOT', 
                                              'DOW', 
                                              'UP', 
                                              'LEF', 
                                              'RIG', 
                                              'IN', 
                                              'OUT'))

g13 <- ggplot(df, aes(Norm_Score_LAG1, Consistency, color=Exp)) +
  geom_point(alpha = 1/8) +
  xlim(c(0.5,1)) + 
  ylim(c(0,1)) + 
  xlab("Norm. score prev. round") +
  ylab("Consistency") +
  scale_color_discrete(name = TeX('$\\alpha$')) +
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
  labs(fill = TeX('$\\alpha$')) +
  #  facet_grid(~Condition) +
  #  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  theme_bw() +
  theme(legend.position="bottom")
  
#g14

dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g15 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
#   ggtitle(TeX('bias$_{RS}=0.6$')) + 
  labs(color = TeX('$\\alpha$')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

#g15

legend <- get_legend(g14)
g11 <- g11 + theme(legend.position="none")
g12 <- g12 + theme(legend.position="none")
g13 <- g13 + theme(legend.position="none")
g14 <- g14 + theme(legend.position="none")
g15 <- g15 + theme(legend.position="none")

g1 <- grid.arrange(g11, g12, g13, g14, g15, nrow = 5)

###############################################
# Focal = 0.05
###############################################
df1 = read.csv("out_Focal0.05-Alpha0.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Focal0.05-Alpha70.csv")
df2$Exp <- as.character("70")
# head(df2)
df3 = read.csv("out_Focal0.05-Alpha150.csv")
df3$Exp <- as.character("150")
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
df$Exp <- factor(df$Exp, levels = c('0', '70', '150'))

dfCons <- summarySE(df, measurevar="Consistency", groupvars=c("Exp", "Round"))
# head(dfCons)

g21 <- ggplot(dfCons, aes(Round, Consistency, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("Consistency") +
  ggtitle(TeX('bias$_{RS}=0.6')) + 
  labs(color = TeX('$\\alpha$')) +
  ylim(c(0,1)) + 
  #  theme(legend.position="bottom") +
  theme_bw()

#g21 

dfDLI <- summarySE(df, measurevar="DLIndex", groupvars=c("Round", "Exp"))
# head(dfDLI)

g22 <- ggplot(dfDLI, aes(Round, DLIndex, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("DLIndex") +
  labs(color = TeX('$\\alpha$')) +
  ylim(c(0,1)) + 
  theme_bw()

#g22 

df$Strategy <- lapply(df$Strategy, function(x) {
  if(x=='0' || x=='9') {
    return('RS')
  } else if(x=='1') {
    return('ALL')
  } else if(x=='2') {
    return('NOT')
  } else if(x=='3') {
    return('DOW')
  } else if(x=='4') {
    return('UP')
  } else if(x=='5') {
    return('LEF')
  } else if(x=='6') {
    return('RIG')
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
                                              'NOT', 
                                              'DOW', 
                                              'UP', 
                                              'LEF', 
                                              'RIG', 
                                              'IN', 
                                              'OUT'))

g23 <- ggplot(df, aes(Norm_Score_LAG1, Consistency, color=Exp)) +
  geom_point(alpha = 1/8) +
  xlim(c(0.5,1)) + 
  ylim(c(0,1)) + 
  xlab("Norm. score prev. round") +
  ylab("Consistency") +
  scale_color_discrete(name = TeX('$\\alpha$')) +
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
  labs(color = TeX('$\\alpha$')) +
  #  facet_grid(~Condition) +
  #  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  #  theme(legend.position="bottom") +
  theme_bw()

#g24

dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g25 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
  #   ggtitle(TeX('bias$_{RS}=0.6$')) + 
  labs(color = TeX('$\\alpha$')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

#g25

g21 <- g21 + theme(legend.position="none")
g22 <- g22 + theme(legend.position="none")
g23 <- g23 + theme(legend.position="none")
g24 <- g24 + theme(legend.position="none")
g25 <- g25 + theme(legend.position="none")

g2 <- grid.arrange(g21, g22, g23, g24, g25, nrow = 5)

###############################################
# Focal = 0.075
###############################################
df1 = read.csv("out_Focal0.075-Alpha0.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Focal0.075-Alpha70.csv")
df2$Exp <- as.character("70")
# head(df2)
df3 = read.csv("out_Focal0.075-Alpha150.csv")
df3$Exp <- as.character("150")
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
df$Exp <- factor(df$Exp, levels = c('0', '70', '150'))

dfCons <- summarySE(df, measurevar="Consistency", groupvars=c("Exp", "Round"))
# head(dfCons)

g31 <- ggplot(dfCons, aes(Round, Consistency, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("Consistency") +
  ggtitle(TeX('bias$_{RS}=0.4')) + 
  labs(color = TeX('$\\alpha$')) +
  ylim(c(0,1)) + 
  #  theme(legend.position="bottom") +
  theme_bw()

#g31 

dfDLI <- summarySE(df, measurevar="DLIndex", groupvars=c("Round", "Exp"))
# head(dfDLI)

g32 <- ggplot(dfDLI, aes(Round, DLIndex, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("DLIndex") +
  labs(color = TeX('$\\alpha$')) +
  ylim(c(0,1)) + 
  theme_bw()

#g32 

df$Strategy <- lapply(df$Strategy, function(x) {
  if(x=='0' || x=='9') {
    return('RS')
  } else if(x=='1') {
    return('ALL')
  } else if(x=='2') {
    return('NOT')
  } else if(x=='3') {
    return('DOW')
  } else if(x=='4') {
    return('UP')
  } else if(x=='5') {
    return('LEF')
  } else if(x=='6') {
    return('RIG')
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
                                              'NOT', 
                                              'DOW', 
                                              'UP', 
                                              'LEF', 
                                              'RIG', 
                                              'IN', 
                                              'OUT'))

g33 <- ggplot(df, aes(Norm_Score_LAG1, Consistency, color=Exp)) +
  geom_point(alpha = 1/8) +
  xlim(c(0.5,1)) + 
  ylim(c(0,1)) + 
  xlab("Norm. score prev. round") +
  ylab("Consistency") +
  scale_color_discrete(name = TeX('$\\alpha$')) +
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
  labs(color = TeX('$\\alpha$')) +
  #  facet_grid(~Condition) +
  #  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  #  theme(legend.position="bottom") +
  theme_bw()

#g34

dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g35 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
  #   ggtitle(TeX('bias$_{RS}=0.6$')) + 
  labs(color = TeX('$\\alpha$')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

#g35

g31 <- g31 + theme(legend.position="none")
g32 <- g32 + theme(legend.position="none")
g33 <- g33 + theme(legend.position="none")
g34 <- g34 + theme(legend.position="none")
g35 <- g35 + theme(legend.position="none")

g3 <- grid.arrange(g31, g32, g33, g34, g35, nrow = 5)

#title1=textGrob(TeX('bias$_{RS}=0.05; \\beta=500; \\gamma=0.98$'), gp=gpar(fontface="bold"))
expTex = TeX('$\\beta{=}500$, $\\gamma{=}0.98$, $\\delta{=}\\epsilon{=}\\zeta{=}0$')
title1=textGrob(expTex, gp=gpar(fontface="bold"))
g <- grid.arrange(g1, g2, g3, ncol = 3, top=legend, bottom=title1)

# ggsave("ConsistencyWRTDist2FR.eps", width=3.5, height=3.5, device=cairo_ps, g3)

