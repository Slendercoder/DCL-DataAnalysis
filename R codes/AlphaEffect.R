library(ggplot2)
library(gridExtra)
library(Rmisc)

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

dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g25 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
#  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
  ggtitle(TeX('bias$_{RS}=0.6$')) + 
  labs(color = TeX('$\\alpha$')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

g25

#g2 <- grid.arrange(legend,
#                   g21, g22,
#                   g23, g24,
#                   g25,
#                   nrow = 3, ncol=2, 
#                   layout_matrix = rbind(
#                     c(5, 4), 
#                     c(2, 3),
#                     c(6, 1)), 
##                   heights = c(0.2,1,1,1),
#                   bottom=title1)
