library(ggplot2)
library(gridExtra)
library(grid)
library(Rmisc)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

###############################################

df1 = read.csv("../Python Codes/Sweeps/sensitivity_0.csv")
df1$Exp <- as.character("True")
head(df1)
df2 = read.csv("../Python Codes/Sweeps/sensitivity_0_simulation.csv")
df2$Exp <- as.character("Estimated")
head(df2)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
#        'Consistency',
#        'Category',
#        'Norm_Score_LAG1',
#        'Similarity_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
#        'Consistency',
#        'Category',
#        'Norm_Score_LAG1',
#        'Similarity_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('True', 'Estimated'))
head(df)
# levels(df$Exp)

# Density plot
g0 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  scale_colour_manual(values = c("True" = "#999999", "Estimated" = "#E69F00")) +  
  scale_y_continuous(limits = c(0, 6)) + 
  labs(color = "Source of data") +
  theme_bw() +
  ggtitle("0%")

g0

####################################################################

df1 = read.csv("../Python Codes/Sweeps/sensitivity_2.csv")
df1$Exp <- as.character("True")
head(df1)
df2 = read.csv("../Python Codes/Sweeps/sensitivity_2_simulation.csv")
df2$Exp <- as.character("Estimated")
head(df2)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        #        'Consistency',
        #        'Category',
        #        'Norm_Score_LAG1',
        #        'Similarity_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        #        'Consistency',
        #        'Category',
        #        'Norm_Score_LAG1',
        #        'Similarity_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('True', 'Estimated'))
head(df)
# levels(df$Exp)

# Density plot
g2 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  scale_colour_manual(values = c("True" = "#999999", "Estimated" = "#E69F00")) +  
  scale_y_continuous(limits = c(0, 6)) + 
  labs(color = "Source of data") +
  theme_bw() +
  ggtitle("20%")

g2

####################################################################

df1 = read.csv("../Python Codes/Sweeps/sensitivity_4.csv")
df1$Exp <- as.character("True")
head(df1)
df2 = read.csv("../Python Codes/Sweeps/sensitivity_4_simulation.csv")
df2$Exp <- as.character("Estimated")
head(df2)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        #        'Consistency',
        #        'Category',
        #        'Norm_Score_LAG1',
        #        'Similarity_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        #        'Consistency',
        #        'Category',
        #        'Norm_Score_LAG1',
        #        'Similarity_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('True', 'Estimated'))
head(df)
# levels(df$Exp)

# Density plot
g4 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  scale_colour_manual(values = c("True" = "#999999", "Estimated" = "#E69F00")) +  
  scale_y_continuous(limits = c(0, 6)) + 
  labs(color = "Source of data") +
  theme_bw() +
  theme(legend.position="bottom") +
  ggtitle("40%")

g4

####################################################################

df1 = read.csv("../Python Codes/Sweeps/sensitivity_6.csv")
df1$Exp <- as.character("True")
head(df1)
df2 = read.csv("../Python Codes/Sweeps/sensitivity_6_simulation.csv")
df2$Exp <- as.character("Estimated")
head(df2)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        #        'Consistency',
        #        'Category',
        #        'Norm_Score_LAG1',
        #        'Similarity_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        #        'Consistency',
        #        'Category',
        #        'Norm_Score_LAG1',
        #        'Similarity_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('True', 'Estimated'))
head(df)
# levels(df$Exp)

# Density plot
g6 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  scale_colour_manual(values = c("True" = "#999999", "Estimated" = "#E69F00")) +  
  scale_y_continuous(limits = c(0, 6)) + 
  labs(color = "Source of data") +
  theme_bw() +
  ggtitle("60%")

g6

####################################################################

legend <- get_legend(g4)
g0 <- g0 + theme(legend.position="none")
g2 <- g2 + theme(legend.position="none")
g4 <- g4 + theme(legend.position="none")
g6 <- g6 + theme(legend.position="none")

title1=textGrob("Percentage of dyads not following WSLS (N=10)", gp=gpar(fontface="bold"))
g <- grid.arrange(g0, g2, g4, g6, ncol = 2, top=title1, bottom=legend)

####################################################################
####################################################################
####################################################################
####################################################################

levels(df$Category)
#df$Category <- lapply(df$Category, function(x) {
#  if(x=='NOTHING') {
#    return('NOT')
#  } else if(x=='DOWN') {
#    return('DOW')
#  } else if(x=='LEFT') {
#    return('LEF')
#  } else if(x=='RIGHT') {
#    return('RIG')
#  } else {
#    #    print(x)
#    return(as.character(x))
#  }
#})
df$Category <- unlist(df$Category)
df$Category <- as.factor(df$Category)
#df$Category <- factor(df$Category, levels = c('RS',
#                                              'ALL', 
#                                              'NOT', 
#                                              'DOW', 
#                                              'UP', 
#                                              'LEF', 
#                                              'RIG', 
#                                              'IN', 
#                                              'OUT'))
df$Category <- factor(df$Category, levels = c('RS',
                                              'ALL', 
                                              'NOTHING', 
                                              'DOWN', 
                                              'UP', 
                                              'LEFT', 
                                              'RIGHT', 
                                              'IN', 
                                              'OUT'))


g3 <- ggplot(df, aes(x=Category,  group=Exp, fill=Exp)) + 
  geom_bar(aes(y = ..prop..), stat="count", position="dodge") +
  #  geom_text(aes(label = scales::percent(..prop..),
  #                 y= ..prop.. ), stat= "count", vjust = -.5) +
  #  labs(y = "Percent", fill="Region") +
  scale_fill_manual(values = c("True" = "#999999", "Estimated full" = "#E69F00", "Estimated absent" = "#56B4E9")) +  
  xlab("Region") +
  ylab("Instances (%)") +
  #  labs(fill = TeX('bias$_{focal}$')) +
  #  facet_grid(~Condition) +
  #  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  theme_bw() +
  theme(legend.position="top")

g3

g4 <- ggplot(df, aes(Norm_Score_LAG1, Consistency, color=Exp)) +
  geom_point(alpha = 1/8) +
  scale_colour_manual(values = c("True" = "#999999", "Estimated full" = "#E69F00", "Estimated absent" = "#56B4E9")) +  
  scale_y_continuous(limits = c(0, 1)) + 
  xlab("Score prev. round") +
  ylab("Consistency") +
  geom_smooth(method = lm)

g4 <- g4 + theme_sjplot()

g4

g5 <- ggplot(df, aes(log(Similarity_LAG1), Consistency, color=Exp)) +
  geom_point(alpha = 1/8) +
  scale_colour_manual(values = c("True" = "#999999", "Estimated full" = "#E69F00", "Estimated absent" = "#56B4E9")) +  
  xlab("Log of max similarity w.r.t.\nfocal regions prev. round") +
  ylab("Consistency") +
  geom_smooth(method = lm)

g5 <- g5 + theme_sjplot()

g5

legend <- get_legend(g3)
g1 <- g1 + theme(legend.position="none")
g2 <- g2 + theme(legend.position="none")
g3 <- g3 + theme(legend.position="none")
g4 <- g4 + theme(legend.position="none")
g5 <- g5 + theme(legend.position="none")

grid.arrange(g1, g2, g3, g4, g5, 
             nrow = 3, 
             layout_matrix = rbind(c(1, 2), c(4, 5), c(3)),
             bottom=legend)

# ggsave("ModelComparisonFull.eps", width=6.6, height=5, device=cairo_ps, g)

