library(sjPlot)
library(sjmisc)
library(ggplot2)


df1 = read.csv("out_Focal0.075-Gamma0.975.csv")
# head(df1)

df1 <- df1[complete.cases(df1), ]

g1 <- ggplot(df1, aes(log(Norm_Score_LAG1), Consistency)) +
  geom_point(alpha = 1/8) +
  xlab("Normalized score previous round") +
  ylab("Consistency on Round n") +
  geom_smooth(method = lm)

g1 <- g1 + theme_sjplot()

g1 

df2 = read.csv("out_Focal0.05-Gamma0.975.csv")
# head(df2)

df2 <- df2[complete.cases(df2), ]

g2 <- ggplot(df2, aes(log(Norm_Score_LAG1), Consistency)) +
  geom_point(alpha = 1/8) +
  xlab("Normalized score previous round") +
  ylab("Consistency on Round n") +
  geom_smooth(method = lm)

g2 <- g2 + theme_sjplot()

g2


df9 = read.csv("out_Focal0.075-Gamma0.99.csv")
# head(df2)

df9 <- df9[complete.cases(df9), ]

g9 <- ggplot(df9, aes(log(Norm_Score_LAG1), Consistency)) +
  geom_point(alpha = 1/8) +
  xlab("Normalized score previous round") +
  ylab("Consistency on Round n") +
  geom_smooth(method = lm)

g9 <- g9 + theme_sjplot()

g9










# ggsave("ConsistencyWRTDist2FR.eps", width=3.5, height=3.5, device=cairo_ps, g3)
