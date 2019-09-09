library(sjPlot)
library(ggplot2)
library(gridExtra)
library(Rmisc)
library(latex2exp)
library(grid)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

###############################################
# Alpha = 150; Beta = 500; Gamma = 0.98
###############################################
df1 = read.csv("out_Focal0-Alpha150.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Focal0.05-Alpha150.csv")
df2$Exp <- as.character("0.05")
# head(df2)
df3 = read.csv("out_Focal0.075-Alpha150.csv")
df3$Exp <- as.character("0.075")
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
df$Exp <- factor(df$Exp, levels = c('0', '0.05', '0.075'))

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


g1 <- ggplot(df, aes(x=Strategy,  group=Exp, fill=Exp)) + 
  geom_bar(aes(y = ..prop..), stat="count", position="dodge") +
  #  geom_text(aes(label = scales::percent(..prop..),
  #                 y= ..prop.. ), stat= "count", vjust = -.5) +
  #  labs(y = "Percent", fill="Region") +
  xlab("Region") +
  ylab("Instances (%)") +
  labs(fill = TeX('bias$_{focal}$')) +
  #  facet_grid(~Condition) +
  #  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  theme_bw() +
  theme(legend.position="top")

# g1

dfDLI <- summarySE(df, measurevar="DLIndex", groupvars=c("Round", "Exp"))
# head(dfDLI)

g2 <- ggplot(dfDLI, aes(Round, DLIndex, group=Exp, color=Exp)) +
  geom_line() +
  xlab("Round (unicorn absent)") +
  ylab("Av. DLIndex") +
  labs(color = TeX('bias$_{focal}$')) +
  ylim(c(0,1)) + 
  theme_bw() +
  theme(legend.position="top") 

#g2

legend <- get_legend(g1)
g1 <- g1 + theme(legend.position="none")
g2 <- g2 + theme(legend.position="none")

expTex = TeX('$\\alpha{=}150$, $\\delta{=}\\zeta{=}0$')
title1=textGrob(expTex, gp=gpar(fontface="bold"))
gRS <- grid.arrange(g1, g2, ncol = 2, top=legend, bottom=title1)

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

g1 <- ggplot(df, aes(Norm_Score_LAG1, Consistency, color=Exp)) +
  geom_point(alpha = 1/5) +
  xlim(c(0.6,1)) + 
  ylim(c(0,1)) + 
  xlab("Norm. score prev. round") +
  ylab("Consistency") +
  scale_color_discrete(name = TeX('$\\alpha$                    ')) +
  geom_smooth(method = lm) +
  theme(legend.position="top")

legend <- get_legend(g1)

g1 <- g1 + theme_sjplot()

g1 <- g1 + theme(legend.position="none")

expTex = TeX('bias$_{focal}=0.05$, $\\delta{=}\\zeta{=}0$')
title1=textGrob(expTex, gp=gpar(fontface="bold"))
gAlpha <- grid.arrange(g1, ncol = 1, top=legend, bottom=title1)

gWSLS <- grid.arrange(gAlpha, gRS, ncol = 2,  widths = c(0.33, 0.6))

###############################################
# Epsilon = 1
###############################################
df1 = read.csv("out_Epsilon1-Zeta0.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Epsilon1-Zeta1.csv")
df2$Exp <- as.character("1")
# head(df2)
df3 = read.csv("out_Epsilon1-Zeta10.csv")
df3$Exp <- as.character("10")
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
df$Exp <- factor(df$Exp, levels = c('0', '1', '10'))

# Density plot Size_visited
g1 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
  #  ggtitle(TeX('$\\epsilon{=}0.3$')) + 
  labs(color = TeX('$\\zeta$                 ')) +
  theme_bw() +
  theme(legend.position="top")               # Position legend in bottom right

#g1

g2 <- ggplot(df, aes(log(Distancias_LAG1), Consistency, color=Exp)) +
  geom_point(alpha = 1/8) +
  xlab("Log of max similarity w.r.t.\nfocal regions prev. round") +
  ylab("Consistency") +
  geom_smooth(method = lm)

g2 <- g2 + theme_sjplot()

#g2

legend <- get_legend(g1)
g1 <- g1 + theme(legend.position="none")
g2 <- g2 + theme(legend.position="none")

expTex = TeX('$bias$_{focal}=0.03, $\\alpha{=}150$, $\\delta{=}0$')
title1=textGrob(expTex, gp=gpar(fontface="bold"))
gZeta <- grid.arrange(g2, g1, ncol = 2, top=legend, bottom=title1)

###############################################
# Eta = 1
###############################################
df1 = read.csv("out_Delta0-Zeta1.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Delta10-Zeta1.csv")
df2$Exp <- as.character("10")
# head(df2)
df3 = read.csv("output.csv")
#df3 = read.csv("out_Delta50-Zeta1.csv")
df3$Exp <- as.character("50")
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
df$Exp <- factor(df$Exp, levels = c('0', '10', '50'))

# Density plot
g1 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
  #  ggtitle(TeX('$\\epsilon{=}0.3$')) + 
  labs(color = TeX('$\\delta$              ')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

#g1

legend <- get_legend(g1)
g1 <- g1 + theme(legend.position="none")

expTex = TeX('$bias$_{focal}=0.03, $\\alpha{=}150$, $\\zeta{=}1$')
title1=textGrob(expTex, gp=gpar(fontface="bold"))
gDelta <- grid.arrange(g1, ncol = 1, top=legend, bottom=title1)

gFRA <- grid.arrange(gZeta, gDelta, ncol = 2,  widths = c(0.66, 0.33))

g <- grid.arrange(gWSLS, gFRA, nrow = 2)


