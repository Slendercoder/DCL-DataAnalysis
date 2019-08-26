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
# Epsilon = 0.3; Zeta = 0.3
###############################################
df1 = read.csv("out_Delta0-Zeta0.3.csv")
df1$Exp <- as.character("0")
# head(df1)
df2 = read.csv("out_Delta0.1-Zeta0.3.csv")
df2$Exp <- as.character("0.1")
# head(df2)
df3 = read.csv("out_Delta0.2-Zeta0.3.csv")
df3$Exp <- as.character("0.2")

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
df$Exp <- factor(df$Exp, levels = c('0', '0.6', '1.5'))

dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g1 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
#  ggtitle(TeX('$\\epsilon{=}0.3$')) + 
  labs(color = TeX('$\\delta$')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

#g1

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
  labs(fill = TeX('$\\zeta$')) +
  #  facet_grid(~Condition) +
  #  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  theme_bw() +
  theme(legend.position="bottom")

g <- grid.arrange(g2, g1, nrow = 1)

