source("Model_Plots.R")

###############################################
# Plot humans
###############################################

archivo <- "humans_only_absent.csv"
df = read.csv(archivo)
theta <- c(0.056, 0.038, 0.01, 0.001, 499, 499, 2, 497, 499, 0.946)
p <- plot_behavior(df, theta, model=FALSE)
df <- find_joint_region(df)
df$RegionFULL <- unlist(df$RegionFULL)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
df <- get_FRASims(df) # Requires to run df <- find_joint_region(df)
df$RegionFULL <- unlist(df$RegionFULL)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
regs <- c('ALL', 'DOWN', 'LEFT', 'IN')
q <- plot_FRA_regs1(df, regs, theta)
grid.arrange(p, q, heights=c(2/3, 1/3))

###############################################
# Plot FRA model
###############################################

archivo <- "../Python Codes/FRA.csv"
df = read.csv(archivo)
theta <- c(0.056, 0.038, 0.01, 0.001, 499, 499, 2, 497, 499, 0.946)
p <- plot_behavior(df, theta)
df <- find_joint_region(df)
df$RegionFULL <- unlist(df$RegionFULL)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
df <- get_FRASims(df) # Requires to run df <- find_joint_region(df)
df$RegionFULL <- unlist(df$RegionFULL)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
regs <- c('ALL', 'DOWN', 'LEFT', 'IN')
q <- plot_FRA_regs1(df, regs, theta)
grid.arrange(p, q, heights=c(2/3, 1/3))
