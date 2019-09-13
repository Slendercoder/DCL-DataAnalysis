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

df1 = read.csv("humans.csv")
df1$Exp <- as.character("Observed behavior")
head(df1)
# df2 = read.csv("out_Focal0.05-Alpha150.csv")
df2 = read.csv("WSLS.csv")
df2$Exp <- as.character("WSLS")
df2$Strategy <- df2$Category
head(df2)
# df3 = read.csv("out_Delta50-Zeta1.csv")
#df3 = read.csv("output.csv")
df3 = read.csv("FRA.csv")
df3$Exp <- as.character("FRA")
df3$Strategy <- df3$Category
head(df3)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Consistency',
        'Strategy',
        'Norm_Score_LAG1',
        'Distancias_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Consistency',
        'Strategy',
        'Norm_Score_LAG1',
        'Distancias_LAG1',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Consistency',
        'Strategy',
        'Norm_Score_LAG1',
        'Distancias_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('Observed behavior', 'WSLS', 'FRA'))
head(df)
# levels(df$Exp)
# Summarize data
dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
head(dfc_DLIndex)

# Plot DLIndex with error regions
g1 <- ggplot(dfc_DLIndex, aes(x = Round, y = DLIndex, colour=Exp, group=Exp)) +
  geom_line(size=0.7) +
  geom_ribbon(aes(ymin = DLIndex - sd,
                  ymax = DLIndex + sd), alpha = 0.2) +
#  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
  labs(color = "Source") +
  xlab("Round (unicorn absent)") +
  ylab("Division of labor") +
  theme_bw()

# Density plot
g2 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
#  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
#  scale_y_continuous(limits = c(0, 5)) + 
  labs(color = "Source of data") +
  theme_bw()

levels(df$Strategy)
df$Strategy <- lapply(df$Strategy, function(x) {
  if(x=='NOTHING') {
    return('NOT')
  } else if(x=='DOWN') {
    return('DOW')
  } else if(x=='LEFT') {
    return('LEF')
  } else if(x=='RIGHT') {
    return('RIG')
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


g3 <- ggplot(df, aes(x=Strategy,  group=Exp, fill=Exp)) + 
  geom_bar(aes(y = ..prop..), stat="count", position="dodge") +
  #  geom_text(aes(label = scales::percent(..prop..),
  #                 y= ..prop.. ), stat= "count", vjust = -.5) +
  #  labs(y = "Percent", fill="Region") +
#  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
  xlab("Region") +
  ylab("Instances (%)") +
#  labs(fill = TeX('bias$_{focal}$')) +
  #  facet_grid(~Condition) +
  #  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  theme_bw() +
  theme(legend.position="top")

g4 <- ggplot(df, aes(Norm_Score_LAG1, Consistency, color=Exp)) +
  geom_point(alpha = 1/8) +
#  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
  scale_y_continuous(limits = c(0, 1)) + 
  xlab("Score prev. round") +
  ylab("Consistency") +
  geom_smooth(method = lm)

g4 <- g4 + theme_sjplot()

#g4

g5 <- ggplot(df, aes(log(Distancias_LAG1), Consistency, color=Exp)) +
  geom_point(alpha = 1/8) +
#  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
  xlab("Log of max similarity w.r.t.\nfocal regions prev. round") +
  ylab("Consistency") +
  geom_smooth(method = lm)

g5 <- g5 + theme_sjplot()

#g5

legend <- get_legend(g3)
g1 <- g1 + theme(legend.position="none")
g2 <- g2 + theme(legend.position="none")
g3 <- g3 + theme(legend.position="none")
g4 <- g4 + theme(legend.position="none")
g5 <- g5 + theme(legend.position="none")

grid.arrange(g1, g2, g3, g4, g5, 
             nrow = 3, 
             layout_matrix = rbind(c(1, 3), c(2, 4), c(5)),
             bottom=legend)

# ggsave("ModelComparisonFull.eps", width=6.6, height=5, device=cairo_ps, g)
