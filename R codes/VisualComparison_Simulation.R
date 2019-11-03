library(ggplot2)
library(gridExtra)
library(Rmisc)

###############################################
# 1 dyad simulated
###############################################
df1 = read.csv("../Python Codes/Sweeps/sim1_1.csv")
df1$Exp <- as.character("1")
head(df1)
df2 = read.csv("../Python Codes/Sweeps/sim1_2.csv")
df2$Exp <- as.character("2")
head(df2)
df3 = read.csv("../Python Codes/Sweeps/sim1_3.csv")
df3$Exp <- as.character("3")
head(df3)
df4 = read.csv("../Python Codes/Sweeps/sim1_4.csv")
df4$Exp <- as.character("4")
head(df4)
df5 = read.csv("../Python Codes/Sweeps/sim1_5.csv")
df5$Exp <- as.character("5")
head(df5)


# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Exp')],
  df4[c('Round', 
        'DLIndex',
        'Exp')],
  df5[c('Round', 
        'DLIndex',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
head(df)
# levels(df$Exp)

# Density plot
g1 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
#  scale_colour_manual(values = c("Model" = "#999999", "Full data" = "#E69F00", "Only unicorn absent" = "#56B4E9")) +  
  scale_y_continuous(limits = c(0, 6)) + 
  labs(color = "Source of data") +
  theme_bw() +
  ggtitle('Groups of 1') +
  theme(legend.position="none")

g1

###############################################
# 5 dyads simulated
###############################################
df1 = read.csv("../Python Codes/Sweeps/sim5_1.csv")
df1$Exp <- as.character("1")
head(df1)
df2 = read.csv("../Python Codes/Sweeps/sim5_2.csv")
df2$Exp <- as.character("2")
head(df2)
df3 = read.csv("../Python Codes/Sweeps/sim5_3.csv")
df3$Exp <- as.character("3")
head(df3)
df4 = read.csv("../Python Codes/Sweeps/sim5_4.csv")
df4$Exp <- as.character("4")
head(df4)
df5 = read.csv("../Python Codes/Sweeps/sim5_5.csv")
df5$Exp <- as.character("5")
head(df5)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Exp')],
  df4[c('Round', 
        'DLIndex',
        'Exp')],
  df5[c('Round', 
        'DLIndex',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
head(df)
# levels(df$Exp)

# Density plot
g5 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("Model" = "#999999", "Full data" = "#E69F00", "Only unicorn absent" = "#56B4E9")) +  
  scale_y_continuous(limits = c(0, 6)) + 
  labs(color = "Source of data") +
  theme_bw() +
  ggtitle('Groups of 5') +
  theme(legend.position="none")

g5

###############################################
# 10 dyads simulated
###############################################
df1 = read.csv("../Python Codes/Sweeps/sim10_1.csv")
df1$Exp <- as.character("1")
head(df1)
df2 = read.csv("../Python Codes/Sweeps/sim10_2.csv")
df2$Exp <- as.character("2")
head(df2)
df3 = read.csv("../Python Codes/Sweeps/sim10_3.csv")
df3$Exp <- as.character("3")
head(df3)
df4 = read.csv("../Python Codes/Sweeps/sim10_4.csv")
df4$Exp <- as.character("4")
head(df4)
df5 = read.csv("../Python Codes/Sweeps/sim10_5.csv")
df5$Exp <- as.character("5")
head(df5)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Exp')],
  df4[c('Round', 
        'DLIndex',
        'Exp')],
  df5[c('Round', 
        'DLIndex',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
head(df)
# levels(df$Exp)

# Density plot
g10 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("Model" = "#999999", "Full data" = "#E69F00", "Only unicorn absent" = "#56B4E9")) +  
  scale_y_continuous(limits = c(0, 6)) + 
  labs(color = "Source of data") +
  theme_bw()  +
  ggtitle('Groups of 10') +
  theme(legend.position="none")

g10

###############################################
# 50 dyads simulated
###############################################
df1 = read.csv("../Python Codes/Sweeps/sim50_1.csv")
df1$Exp <- as.character("1")
head(df1)
df2 = read.csv("../Python Codes/Sweeps/sim50_2.csv")
df2$Exp <- as.character("2")
head(df2)
df3 = read.csv("../Python Codes/Sweeps/sim50_3.csv")
df3$Exp <- as.character("3")
head(df3)
df4 = read.csv("../Python Codes/Sweeps/sim50_4.csv")
df4$Exp <- as.character("4")
head(df4)
df5 = read.csv("../Python Codes/Sweeps/sim50_5.csv")
df5$Exp <- as.character("5")
head(df5)

# Create single data frame with DLIndexes
df <- rbind(
  df1[c('Round', 
        'DLIndex',
        'Exp')],
  df2[c('Round', 
        'DLIndex',
        'Exp')],
  df3[c('Round', 
        'DLIndex',
        'Exp')],
  df4[c('Round', 
        'DLIndex',
        'Exp')],
  df5[c('Round', 
        'DLIndex',
        'Exp')]
)
df$Exp <- as.factor(df$Exp)
head(df)
# levels(df$Exp)

# Density plot
g50 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  #  scale_colour_manual(values = c("Model" = "#999999", "Full data" = "#E69F00", "Only unicorn absent" = "#56B4E9")) +  
  scale_y_continuous(limits = c(0, 6)) + 
  labs(color = "Source of data") +
  theme_bw() +
  ggtitle('Groups of 50') +
  theme(legend.position="none")

g50

###############################################
# Combining plots
###############################################

grid.arrange(g1, g5, g10, g50, 
             nrow = 2,
             top='Dispersion of simulated groups')

# ggsave("ModelComparisonFull.eps", width=6.6, height=5, device=cairo_ps, g)

