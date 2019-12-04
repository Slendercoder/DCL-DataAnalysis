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

df1 = read.csv("../Python Codes/WSLS2BRecovered.csv")
df1$Exp <- as.character("Initial")
head(df1)
df2 = read.csv("../Python Codes/modelRecovered.csv")
df2$Exp <- as.character("Recovered")

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
#        'Similarity_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
#        'Similarity_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('Initial', 'Recovered'))
head(df)
# levels(df$Exp)

###############################################

# Summarize data
dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
head(dfc_DLIndex)

# Plot DLIndex with error regions
g1 <- ggplot(dfc_DLIndex, aes(x = Round, y = DLIndex, colour=Exp, group=Exp)) +
  geom_line(size=0.7) +
  geom_ribbon(aes(ymin = DLIndex - sd,
                  ymax = DLIndex + sd), alpha = 0.2) +
  scale_colour_manual(values = c("Initial" = "#999999", "Recovered" = "#E69F00")) +  
  labs(color = "Source") +
  xlab("Round (unicorn absent)") +
  ylab("Division of labor") +
  theme_bw()

g1

# Density plot
g2 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  scale_colour_manual(values = c("Initial" = "#999999", "Recovered" = "#E69F00")) +  
#  scale_y_continuous(limits = c(0, 5)) + 
  labs(color = "Model") +
  xlab("Division of labor") +
  theme_bw() +
  theme(legend.position="top")

g2

legend <- get_legend(g2)
g1 <- g1 + theme(legend.position="none")
g2 <- g2 + theme(legend.position="none")

grid.arrange(g1, g2, 
             nrow = 1,
             bottom=legend, top="Model recovery - DLindex")

