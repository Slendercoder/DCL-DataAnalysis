library(ggplot2)
library(gridExtra)
library(grid)
library(Rmisc)
library(latex2exp)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

###############################################
# Zeta = 0
###############################################
df1 = read.csv("out_Delta0-Zeta0.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Delta0.2-Zeta0.csv")
df2$Exp <- as.character("0.2")
# head(df2)
df3 = read.csv("out_Delta0.4-Zeta0.csv")
df3$Exp <- as.character("0.4")
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
df$Exp <- factor(df$Exp, levels = c('0', '0.2', '0.4'))

dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g1 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
  ggtitle(TeX('$\\zeta{=}0$')) + 
  labs(color = TeX('$\\delta$')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

#g1

###############################################
# Zeta = 0.1
###############################################
df1 = read.csv("out_Delta0-Zeta0.1.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Delta0.2-Zeta0.1.csv")
df2$Exp <- as.character("0.2")
# head(df2)
df3 = read.csv("out_Delta0.4-Zeta0.1.csv")
df3$Exp <- as.character("0.4")
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
df$Exp <- factor(df$Exp, levels = c('0', '0.2', '0.4'))

dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g2 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
  ggtitle(TeX('$\\zeta{=}0.1$')) + 
  labs(color = TeX('$\\delta$')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

#g2

###############################################
# Zeta = 0.2
###############################################
df1 = read.csv("out_Delta0-Zeta0.2.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Delta0.2-Zeta0.2.csv")
df2$Exp <- as.character("0.2")
# head(df2)
df3 = read.csv("out_Delta0.4-Zeta0.2.csv")
df3$Exp <- as.character("0.4")
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
df$Exp <- factor(df$Exp, levels = c('0', '0.2', '0.4'))

dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g3 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
  ggtitle(TeX('$\\zeta{=}0.2$')) + 
  labs(color = TeX('$\\delta$')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

#g3

###############################################
# Zeta = 0.3
###############################################
df1 = read.csv("out_Delta0-Zeta0.3.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Delta0.2-Zeta0.3.csv")
df2$Exp <- as.character("0.2")
# head(df2)
df3 = read.csv("out_Delta0.4-Zeta0.3.csv")
df3$Exp <- as.character("0.4")
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
df$Exp <- factor(df$Exp, levels = c('0', '0.2', '0.4'))

dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g4 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
  ggtitle(TeX('$\\zeta{=}0.3$')) + 
  labs(color = TeX('$\\delta$')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

#g4


legend <- get_legend(g1)

g1 <- g1 + theme(legend.position="none")
g2 <- g2 + theme(legend.position="none")
g3 <- g3 + theme(legend.position="none")
g4 <- g4 + theme(legend.position="none")

# g <- grid.arrange(g1, g2, g3, g4, nrow = 4)

g <- grid.arrange( g1, g2,
                   g3, g4,
                   legend,
                   nrow = 3, ncol=2, 
                   layout_matrix = rbind(
                     c(1, 2), 
                     c(3, 4),
                     c(5)), 
                   heights = c(1,1,0.2),
                   bottom=title1)



