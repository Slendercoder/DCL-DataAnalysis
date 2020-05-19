source("Model_Plots.R")

archivo <- "humans_only_absent.csv"
df = read.csv(archivo)
df$Region <- df$Category

###############################################
# Plot Parameter Recovery
###############################################
theta <- c(0.056, 0.038, 0.01, 0.001, 499, 499, 2, 497, 499, 0.946)
df2 <- getRelFreq_Rows(df)
d1 <- plot_RSTransitions(df2)
d1 <- plot_ModelTransitions_RS(theta, d1,"#999999")
d2 <- plot_FocalTransitions(df2)
d2 <- plot_ModelTransitions_Focal(theta, d2,"#999999")
params <- para_visualizar(theta)
params <- paste("Parameters recovered:", params)
p <- grid.arrange(d1, d2, nrow=1,
                   bottom=params)
df <- find_joint_region(df)
df$RegionFULL <- unlist(df$RegionFULL)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
df <- get_FRASims(df) # Requires to run df <- find_joint_region(df)
df$RegionFULL <- unlist(df$RegionFULL)
df$RegionGo <- factor(df$RegionGo, levels = regiones)
regs <- c('ALL', 'DOWN', 'LEFT', 'IN')
q <- plot_FRA_regs1(df, regs, theta)
grid.arrange(p, q, nrow=2) #, heights=c(2/3, 1/3))

###############################################
# Plot humans qualitative behavior
p <- plot_behavior(df)

###############################################
# Plot FRA model
archivo <- "../Python Codes/FRA.csv"
df = read.csv(archivo)
p <- plot_behavior(df)
p2 <- ggplot(df, aes(Size_visited)) +
  geom_density(size=1) +
  #  scale_y_continuous(limits = c(0, 5)) + 
  ggtitle("Model") +
  xlab("Number of tiles uncovered") +
  theme_bw()
p2


###############################################
archivo <- "humans_only_absent.csv"
df = read.csv(archivo)
df$Region <- df$Category

archivo <- "../Python Codes/FRA.csv"
df1 = read.csv(archivo)

p1 <- ggplot(df, aes(Size_visited)) +
  geom_density(size=1) +
  #  scale_y_continuous(limits = c(0, 5)) + 
  ggtitle("Humans") +
  xlab("Number of tiles uncovered") +
  theme_bw()

p2 <- ggplot(df1, aes(Size_visited)) +
  geom_density(size=1) +
  #  scale_y_continuous(limits = c(0, 5)) + 
  ggtitle("Model") +
  xlab("Number of tiles uncovered") +
  theme_bw()

grid.arrange(p1, p2, nrow=1)

model3h <- lm(DLIndex ~ Consistency + Dif_consist*Joint_LAG1, data = df)
summary(model3h)
p2 <- plot_model(model3h, 
                 type = "pred", 
                 terms = c("Dif_consist", "Joint_LAG1"), 
                 colors = c("black", "red", "blue"),
                 title = "",
                 legend.title = "Overlap",
                 axis.title = c("Absolute difference\nin consistency", "DLindex"))
