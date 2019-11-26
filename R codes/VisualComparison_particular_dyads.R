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
# Dyad 140-615
###############################################

df1 = read.csv("../Python Codes/Dyads/output-140-615.csv")
df1$Exp <- as.character("Observed behavior")
#head(df1)
df2 = read.csv("../Python Codes/Dyads/WSLS-140-615.csv")
df2$Exp <- as.character("WSLS")
#head(df2)
df3 = read.csv("../Python Codes/Dyads/FRA-140-615.csv")
df3$Exp <- as.character("FRA")
#head(df3)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
        'Similarity_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
        'Similarity_LAG1',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
        'Similarity_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('Observed behavior', 'WSLS', 'FRA'))
head(df)
# levels(df$Exp)

# Density plot
g1D <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
#  scale_y_continuous(limits = c(0, 5)) + 
  labs(color = "Source of data") +
  theme_bw()  +
  ggtitle("Inside-Outside split (Dyad 140-615)")

g1D

# Summarize data
dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
head(dfc_DLIndex)

# Plot DLIndex with error regions
g1R <- ggplot(dfc_DLIndex, aes(x = Round, y = DLIndex, colour=Exp, group=Exp)) +
  geom_line(size=0.7) +
  geom_ribbon(aes(ymin = DLIndex - sd,
                  ymax = DLIndex + sd), alpha = 0.2) +
  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
  labs(color = "Source") +
  xlab("Round (unicorn absent)") +
  ylab("Division of labor") +
  theme_bw()

g1R

legend <- get_legend(g1D)
g1D <- g1D + theme(legend.position="none")
g1DR<- g1R + theme(legend.position="none")

grid.arrange(g1D, g1R, 
             nrow = 1,
             bottom=legend)

###############################################
# Dyad 435-261
###############################################

df1 = read.csv("../Python Codes/Dyads/output-435-261.csv")
df1$Exp <- as.character("Observed behavior")
#head(df1)
df2 = read.csv("../Python Codes/Dyads/WSLS-435-261.csv")
df2$Exp <- as.character("WSLS")
#head(df2)
df3 = read.csv("../Python Codes/Dyads/FRA-435-261.csv")
df3$Exp <- as.character("FRA")
#head(df3)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
        'Similarity_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
        'Similarity_LAG1',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
        'Similarity_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('Observed behavior', 'WSLS', 'FRA'))
head(df)
# levels(df$Exp)

# Density plot
g2 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 5)) + 
  labs(color = "Source of data") +
  theme_bw()  +
  ggtitle("Left-Right split (Dyad 435-261)")

g2

###############################################
# Dyad 379-897
###############################################

df1 = read.csv("../Python Codes/Dyads/output-379-897.csv")
df1$Exp <- as.character("Observed behavior")
#head(df1)
df2 = read.csv("../Python Codes/Dyads/WSLS-379-897.csv")
df2$Exp <- as.character("WSLS")
#head(df2)
df3 = read.csv("../Python Codes/Dyads/FRA-379-897.csv")
df3$Exp <- as.character("FRA")
#head(df3)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
        'Similarity_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
        'Similarity_LAG1',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
        'Similarity_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('Observed behavior', 'WSLS', 'FRA'))
head(df)
# levels(df$Exp)

# Density plot
g3 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 5)) + 
  labs(color = "Source of data") +
  theme_bw() +
  ggtitle("No convergence (Dyad 379-897)")

g3

###############################################
# Dyad 356-137
###############################################

df1 = read.csv("../Python Codes/Dyads/output-356-137.csv")
df1$Exp <- as.character("Observed behavior")
#head(df1)
df2 = read.csv("../Python Codes/Dyads/WSLS-356-137.csv")
df2$Exp <- as.character("WSLS")
#head(df2)
df3 = read.csv("../Python Codes/Dyads/FRA-356-137.csv")
df3$Exp <- as.character("FRA")
#head(df3)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
        'Similarity_LAG1',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
        'Similarity_LAG1',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Consistency',
        'Category',
        'Norm_Score_LAG1',
        'Similarity_LAG1',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('Observed behavior', 'WSLS', 'FRA'))
head(df)
# levels(df$Exp)

# Density plot
g4 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 5)) + 
  labs(color = "Source of data") +
  theme_bw() +
  ggtitle("Top-bottom split (Dyad 356-137)")

g4


legend <- get_legend(g3)
g1 <- g1 + theme(legend.position="none")
g2 <- g2 + theme(legend.position="none")
g3 <- g3 + theme(legend.position="none")
g4 <- g4 + theme(legend.position="none")
g5 <- g5 + theme(legend.position="none")

grid.arrange(g1, g2, 
             nrow = 1,
             bottom=legend)

grid.arrange(g1, g2, g3, g4, 
             nrow = 2,
             bottom=legend)

# ggsave("ModelComparisonFull.eps", width=6.6, height=5, device=cairo_ps, g)

