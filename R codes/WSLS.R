library(ggplot2)

df1 = read.csv("humans.csv")
# head(df1)

df1 <- df1[complete.cases(df1), ]

df1$Distancias

g1 <- ggplot(df1, aes(Norm_Score_LAG1, Consistency)) +
  geom_point(alpha = 1/8) +
  xlab("Normalized score on Round n-1") +
  ylab("Consistency on Round n") +
  geom_smooth(method = lm)

g1

# ggsave("ConsistencyWRTScore.eps", width=3.5, height=3.5, device=cairo_ps, g1)
