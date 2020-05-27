source("MODELpred.R")
source("Model_Plots.R")

rotulos <- c("0.1", "1", "2", "3")
archivo <- "../Data/Sweeps/softmax/1-Sweep-softmax-0.1.csv"
d1 <- read.csv(archivo)
d1$Exp <- as.character(rotulos[1])
archivo <- "../Data/Sweeps/softmax/2-Sweep-softmax-1.csv"
d2<- read.csv(archivo)
d2$Exp <- as.character(rotulos[2])
archivo <- "../Data/Sweeps/softmax/3-Sweep-softmax-2.csv"
d3 <- read.csv(archivo)
d3$Exp <- as.character(rotulos[3])
archivo <- "../Data/Sweeps/softmax/4-Sweep-softmax-3.csv"
d4 <- read.csv(archivo)
d4$Exp <- as.character(rotulos[4])
df <- rbind(d1, d2, d3, d4)

# DLindex vs. round
# Summarize data
dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))

# Plot DLIndex with error regions
colores <- cbPalette[2:5]
g7 <- ggplot(dfc_DLIndex, aes(x = Round, y = DLIndex, colour=Exp, group=Exp)) +
  geom_line(size=0.9) +
  scale_colour_manual(values = colores) +  
  xlab("Round (unicorn absent)") +
  ylab("Division of labor") +
  theme_bw()

g7

# # PLOT WSLS AT INDIVIDUAL LEVEL
# legend2 <- get_legend_from_dummy1(WS_color, cbPalette[7])
# theta <- c(0.1, 0.083, 0.05, 0.006, 0, 0, 0, 0, 0, 0)

df$Region <- df$Category
df <- getRelFreq_Rows(df)
d2 <- plot_FocalTransitions1(df)
# d2 <- plot_ModelTransitions_Focal(theta, d2, FR_color)
