library(ggplot2)
library(gridExtra)
library(Rmisc)

df1 = read.csv("humans.csv")
df1$Exp <- as.character("Observed behavior")
# head(df1)
# df2 = read.csv("out_Focal0.05-Alpha150.csv")
df2 = read.csv("WSLS.csv")
df2$Exp <- as.character("WSLS")
# head(df2)

# df3 = read.csv("out_Delta50-Zeta1.csv")
df3 = read.csv("FRA.csv")
df3$Exp <- as.character("FRA")
# head(df3)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
#        'Strategy',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
#        'Strategy',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
#        'Strategy',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
df$Exp <- factor(df$Exp, levels = c('Observed behavior', 'WSLS', 'FRA'))
#  head(df)
# levels(df$Exp)
# Summarize data
dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
head(dfc_DLIndex)

# Plot DLIndex with error regions
p1 <- ggplot(dfc_DLIndex, aes(x = Round, y = DLIndex, colour=Exp, group=Exp)) +
  geom_line(size=0.7) +
  geom_ribbon(aes(ymin = DLIndex - sd,
                  ymax = DLIndex + sd), alpha = 0.2) +
  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
  labs(color = "Source") +
  xlab("Round (unicorn absent)") +
  ylab("Division of labor") +
  theme_bw() +
  theme(legend.position="none")               # Position legend

# Density plot
p2 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
#  scale_y_continuous(limits = c(0, 5)) + 
  labs(color = "Source of data") +
  theme_bw() +
  theme(legend.position="bottom")               # Position legend in bottom right

grid.arrange(p1, p2, nrow = 2, layout_matrix = rbind(c(1), c(2)))

ggsave("ModelComparisonFull.eps", width=6.6, height=5, device=cairo_ps, g)
