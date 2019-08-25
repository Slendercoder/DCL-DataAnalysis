library(ggplot2)
library(gridExtra)
library(Rmisc)

###############################################
# Alpha = 150; Beta = 500; Gamma = 0.98
###############################################
df1 = read.csv("out_Focal0.025-Alpha150.csv")
df1$Exp <- as.character("0.8")
# head(df1)
df2 = read.csv("out_Focal0.05-Alpha150.csv")
df2$Exp <- as.character("0.6")
# head(df2)
df3 = read.csv("out_Focal0.075-Alpha150.csv")
df3$Exp <- as.character("0.4")
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
df$Exp <- factor(df$Exp, levels = c('0.8', '0.6', '0.4'))

dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
# head(dfc_DLIndex)

# Density plot Size_visited
g25 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
#  scale_colour_manual(values = c("0" = "#999999", "70" = "#E69F00", "150" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 3)) + 
  ggtitle(TeX('$\\alpha{=}150$')) + 
  labs(color = TeX('bias$_{RS}$')) +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

g25
