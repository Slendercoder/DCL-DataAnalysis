library(ggplot2)
library(gridExtra)
library(Rmisc)
library(sjPlot)
library(sjmisc)

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

# dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g1 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
#  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
#  ggtitle(TeX('bias$_{focal}=0.4$')) + 
  labs(color = TeX('$\\alpha$                                        ')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

# g1

g2 <- ggplot(df, aes(Norm_Score_LAG1, Consistency, color=Exp)) +
  geom_point(alpha = 1/5) +
  xlim(c(0.6,1)) + 
  ylim(c(0,1)) + 
  xlab("Norm. score prev. round") +
  ylab("Consistency") +
  scale_color_discrete(name = TeX('$\\delta$')) +
  geom_smooth(method = lm)

g2 <- g2 + theme_sjplot()

# g2

legend <- get_legend(g1)
g1 <- g1 + theme(legend.position="none")
g2 <- g2 + theme(legend.position="none")

expTex = TeX('$bias$_{focal}=0.4, $\\beta{=}500$, $\\gamma{=}0.98$, $\\delta{=}\\epsilon{=}\\zeta{=}0$')
title1=textGrob(expTex, gp=gpar(fontface="bold"))
g <- grid.arrange(g1, g2, ncol = 2, top=legend, bottom=title1)

