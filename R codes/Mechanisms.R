source("Model_Plots.R")

archivo <- "../Data/MBiases_simulated.csv"
df1 = read.csv(archivo)
df1$Exp <- as.character("MBiases")
df1$Region <- df1$Category

archivo <- "../Data/WSLS_simulated.csv"
df2 = read.csv(archivo)
df2$Exp <- as.character("WSLS")
df2$Region <- df2$Category

archivo <- "../Data/FRA_simulated.csv"
df3 = read.csv(archivo)
df3$Exp <- as.character("FRA")
df3$Region <- df3$Category

p <- plot_3set_comparison_WSLS(df1, df2, df3)
