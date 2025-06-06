library(ggplot2)
library(gridExtra)
library(grid)
library(Rmisc)
library(latex2exp)
library(RColorBrewer)
#display.brewer.all(colorblindFriendly = TRUE)
library(sjmisc)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

###############################################
# Eta = 1
###############################################
df1 = read.csv("out_Delta0-Eta1.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Delta100-Eta1.csv")
df2$Exp <- as.character("100")
# head(df2)
df3 = read.csv("out_Delta120-Eta1.csv")
df3$Exp <- as.character("120")
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
        'Distancias_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Strategy',
        'Consistency',
        'Norm_Score_LAG1',
        'Distancias_LAG1',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Strategy',
        'Consistency',
        'Norm_Score_LAG1',
        'Distancias_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('0', '5', '10'))

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

g3 <- ggplot(df, aes(log(Distancias_LAG1), Consistency, color=Exp)) +
  geom_point(alpha = 1/8) +
  xlab("Log of max similarity w.r.t.\nfocal regions on Round n-1") +
  ylab("Consistency on Round n") +
  scale_color_discrete(name = TeX('$\\zeta$')) +
  geom_smooth(method = lm)

# g3

g4 <- ggplot(df, aes(Norm_Score_LAG1, Consistency, color=Exp)) +
  geom_point(alpha = 1/8) +
  xlim(c(0.6,1)) + 
  ylim(c(0,1)) + 
  xlab("Norm. score prev. round") +
  ylab("Consistency") +
  scale_color_discrete(name = TeX('$\\zeta$')) +
  geom_smooth(method = lm)

g4 <- g4 + theme_sjplot()

# g4


legend <- get_legend(g2)
g1 <- g1 + theme(legend.position="none")
g2 <- g2 + theme(legend.position="none")
g3 <- g3 + theme(legend.position="none")
g4 <- g4 + theme(legend.position="none")

expTex = TeX('$bias$_{focal}=0, $\\alpha{=}150$, $\\beta{=}500$, $\\gamma{=}0.98$, $\\zeta{=}1$, $\\epsilon{=}1$')
title1=textGrob(expTex, gp=gpar(fontface="bold"))
g <- grid.arrange(g1, g3, g2, g4, ncol = 2, top=legend, bottom=title1)

model3h <- lm(DLIndex ~ Consistency + Dif_consist*Joint_LAG1, data = df3)

g2 <- plot_model(model3h, 
                 type = "pred", 
                 terms = c("Dif_consist", "Joint_LAG1"), 
                 colors = c("black", "red", "blue"),
                 title = "",
                 legend.title = "Overlap",
                 axis.title = c("Absolute difference\nin consistency", "DLindex"))

g2

