library(sjPlot)
library(sjmisc)
library(ggplot2)


df1 = read.csv("humans.csv")
# head(df1)

df1 <- df1[complete.cases(df1), ]

# Regressing Consistency w.r.t. max similarity to focal region
model4h <- lm(Consistency ~ Distancias_LAG1, data = df1)
summary(model4h) # => Positive correlation is significant

g3 <- ggplot(df1, aes(log(Distancias_LAG1), Consistency)) +
  geom_point(alpha = 1/8) +
  xlab("Log of max similarity w.r.t.\nfocal regions on Round n-1") +
  ylab("Consistency on Round n") +
  geom_smooth(method = lm)

g3 <- g3 + theme_sjplot()

g3 

ggsave("ConsistencyWRTDist2FR.eps", width=3.5, height=3.5, device=cairo_ps, g3)
