library(ggplot2)
library(gridExtra)
library(grid)
library(Rmisc)
library(latex2exp)
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

###############################################
# Epsilon = 0.3; Zeta = 0.3
###############################################
df1 = read.csv("out_Delta0-Zeta0.3.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Delta0.05-Zeta0.3.csv")
df2$Exp <- as.character("0.05")
# head(df2)
df2 = read.csv("out_Delta0.1-Zeta0.3.csv")
df2$Exp <- as.character("0.1")

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
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('0', '0.05', '0.1'))

dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g1 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
#  ggtitle(TeX('$\\epsilon{=}0.3$')) + 
  labs(color = TeX('$\\delta$              ')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

#g1

dfDLI <- summarySE(df, measurevar="DLIndex", groupvars=c("Round", "Exp"))
# head(dfDLI)

g2 <- ggplot(dfDLI, aes(Round, DLIndex, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("Av. DLIndex") +
  labs(color = TeX('$\\delta$                        ')) +
  ylim(c(0,1)) + 
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

#g2 

legend <- get_legend(g1)
g1 <- g1 + theme(legend.position="none")
g2 <- g2 + theme(legend.position="none")

expTex = TeX('$bias$_{focal}=0, $\\alpha{=}150$, $\\beta{=}500$, $\\gamma{=}0.98$, $\\epsilon{=}0.3$, $\\zeta{=}0.3$')
title1=textGrob(expTex, gp=gpar(fontface="bold"))
g <- grid.arrange(g1, g2, ncol = 2, top=legend, bottom=title1)

model3h <- lm(DLIndex ~ Consistency + Dif_consist*Joint_LAG1, data = df3)

g2 <- plot_model(model3h, 
                 type = "pred", 
                 terms = c("Dif_consist", "Joint_LAG1"), 
                 colors = c("black", "red", "blue"),
                 title = "",
                 legend.title = "Overlap",
                 axis.title = c("Absolute difference\nin consistency", "DLindex"))

g2
