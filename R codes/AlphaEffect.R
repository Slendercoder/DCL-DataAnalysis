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
# Focal = 0.05; Beta = 500; Gamma = 0.98
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
head(dfCons)

g21 <- ggplot(dfCons, aes(Round, Consistency, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("Av. Consistency") +
#  ggtitle('Focal = 0.05') + 
#  ggtitle('Prob(RS)=0.6') + 
  labs(color = TeX('$\\alpha$')) +
  ylim(c(0,1)) + 
  theme_bw() +
  theme(legend.position="top")

#g21 

dfDLI <- summarySE(df, measurevar="DLIndex", groupvars=c("Round", "Exp"))
head(dfDLI)

g22 <- ggplot(dfDLI, aes(Round, DLIndex, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("Av. DLIndex") +
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

g23 <- ggplot(df, aes(Norm_Score_LAG1, Consistency, color=Exp)) +
  geom_point(alpha = 1/8) +
  xlim(c(0.5,1)) + 
  ylim(c(0,1)) + 
  xlab("Norm. score prev. round") +
  ylab("Consistency") +
  labs(fill = "OK") +
  scale_color_discrete(name = TeX('$\\alpha$')) +
#  labs(fill = TeX('$\\alpha$')) +
  geom_smooth(method = lm)

g23 <- g23 + theme_sjplot()

#g23

g24 <- ggplot(df, aes(x=Strategy,  group=Exp, fill=Exp)) + 
  geom_bar(aes(y = ..prop..), stat="count", position="dodge") +
  #  geom_text(aes(label = scales::percent(..prop..),
  #                 y= ..prop.. ), stat= "count", vjust = -.5) +
  #  labs(y = "Percent", fill="Region") +
  xlab("Region") +
  ylab("Instances (%)") +
  labs(fill = TeX('$\\alpha$')) +
  #  facet_grid(~Condition) +
  #  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  #  theme(legend.position="bottom") +
  theme_bw()

#g24

# AlphaEffect
legend <- get_legend(g21)
g21 <- g21 + theme(legend.position="none")
g22 <- g22 + theme(legend.position="none")
g23 <- g23 + theme(legend.position="none")
g24 <- g24 + theme(legend.position="none")

#title1=textGrob(TeX('bias$_{RS}=0.05; \\beta=500; \\gamma=0.98$'), gp=gpar(fontface="bold"))
expTex = TeX('$bias_{RS}=0.6$, $\\beta{=}500$, $\\gamma{=}0.98$')
title1=textGrob(expTex, gp=gpar(fontface="bold"))

g2 <- grid.arrange(legend,
                   g21, g22,
                   g23, g24,
                   nrow = 3, ncol=2, 
                   layout_matrix = rbind(c(1), c(5, 4), c(2, 3)), 
                   heights = c(0.2,1,1),
                   bottom=title1)

# ggsave("AlphaEffect.eps", width=7, height=5, device=cairo_ps, g2)

