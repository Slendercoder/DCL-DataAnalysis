library(ggplot2)

df1 = read.csv("humans.csv")
# head(df1)

df1 <- df1[complete.cases(df1), ]

# Regressing Consistency(n) w.r.t. Score(n-1)
model1h <- lm(Consistency ~ Norm_Score_LAG1, data = df1)
summary(model1h) # => Positive correlation is significant

g1 <- ggplot(df1, aes(Norm_Score_LAG1, Consistency)) +
  geom_point(alpha = 1/8) +
  theme_bw() +
  xlab("Normalized score on Round n-1") +
  ylab("Consistency on Round n") +
  geom_smooth(method = lm)

g1

ggsave("ConsistencyWRTScore.eps", width=3.5, height=3.5, device=cairo_ps, g1)
