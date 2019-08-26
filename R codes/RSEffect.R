library(ggplot2)
library(gridExtra)
library(Rmisc)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


###############################################
# Alpha = 150; Beta = 500; Gamma = 0.98
###############################################
df1 = read.csv("out_Focal0.025-Alpha150.csv")
df1$Exp <- as.character("0.2")
# head(df1)
df2 = read.csv("out_Focal0.05-Alpha150.csv")
df2$Exp <- as.character("0.4")
# head(df2)
df3 = read.csv("out_Focal0.075-Alpha150.csv")
df3$Exp <- as.character("0.6")
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
df$Exp <- factor(df$Exp, levels = c('0.2', '0.4', '0.6'))

#dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g1 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
#  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
#  ggtitle(TeX('$\\alpha{=}150$')) + 
  labs(color = TeX('bias$_{focal}$')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

# g1

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

g2 <- ggplot(df, aes(x=Strategy,  group=Exp, fill=Exp)) + 
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
  theme(legend.position="bottom")

# g2

legend <- get_legend(g2)
g1 <- g1 + theme(legend.position="none")
g2 <- g2 + theme(legend.position="none")

expTex = TeX('$\\alpha{=}150$, $\\beta{=}500$, $\\gamma{=}0.98$, $\\delta{=}\\epsilon{=}\\zeta{=}0$')
title1=textGrob(expTex, gp=gpar(fontface="bold"))
g <- grid.arrange(g1, g2, ncol = 2, top=legend, bottom=title1)
