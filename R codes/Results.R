library(dplyr)
library(ggplot2)
library(Rmisc)
library(gridExtra)

df1 = read.csv("humans.csv")
# head(df1)

df2 = read.csv("WSLS.csv")
# head(df2)

df3 = read.csv("FRA.csv")
# head(df3)

# ----------------------------------

# Check the performance on the first round, as if one-shot task
DLIRound60 <- df1$DLIndex[which(df1$Round == 60)]
mean(DLIRound60)
sd(DLIRound60)
DLIRound1 <- df1$DLIndex[which(df1$Round == 1)]
mean(DLIRound1)
sd(DLIRound1)
t.test(DLIRound60, DLIRound1) # => Difference one-shot task vs iterated task is significant

DLIRound60 <- df2$DLIndex[which(df2$Round == 60)]
mean(DLIRound60)
sd(DLIRound60)
DLIRound1 <- df2$DLIndex[which(df2$Round == 1)]
mean(DLIRound1)
sd(DLIRound1)
t.test(DLIRound60, DLIRound1) # => Difference one-shot task vs iterated task is significant

DLIRound60 <- df3$DLIndex[which(df3$Round == 60)]
mean(DLIRound60)
sd(DLIRound60)
DLIRound1 <- df3$DLIndex[which(df3$Round == 1)]
mean(DLIRound1)
sd(DLIRound1)
t.test(DLIRound60, DLIRound1) # => Difference one-shot task vs iterated task is significant


# Plot frequencies of regions
n <- length(df1$Strategy[which(df1$Round == 1)])
A <- as.data.frame(rep('First Round', n))
colnames(A) = c('Condition')
A$Strategy <- df1$Strategy[which(df1$Round == 1)]
head(A)
n <- length(df1$Strategy)
B <- as.data.frame(rep('60 Rounds', n))
colnames(B) = c('Condition')
B$Strategy <- df1$Strategy

df <- rbind(
  A[c('Strategy',
      'Condition')],
  B[c('Strategy',
      'Condition')]
)
df$Condition <- as.factor(df$Condition)
df$Condition <- factor(df$Condition, levels = c('First Round', '60 Rounds'))
df$Strategy <- lapply(df$Strategy, function(x) {
  if(x=='NOTHING') {
    return('NOTH')
  } else {
#    print(x)
    return(as.character(x))
    }
})
df$Strategy <- unlist(df$Strategy)
df$Strategy <- as.factor(df$Strategy)
df$Strategy <- factor(df$Strategy, levels = c('RS',
                                              'ALL', 
                                              'NOTH', 
                                              'DOWN', 
                                              'UP', 
                                              'LEFT', 
                                              'RIGHT', 
                                              'IN', 
                                              'OUT'))

p <- ggplot(df, aes(x=Strategy,  group=Condition, fill=Condition)) + 
  geom_bar(aes(y = ..prop..), stat="count", position="dodge") +
#  geom_text(aes(label = scales::percent(..prop..),
#                 y= ..prop.. ), stat= "count", vjust = -.5) +
#  labs(y = "Percent", fill="Region") +
  xlab("Region") +
  ylab("Observed instances (%)") +
#  facet_grid(~Condition) +
#  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  theme_bw() +
  theme(legend.position="bottom")

p

# ----------------------------------

# Regressing Consistency(n) w.r.t. Score(n-1)
model1h <- lm(Consistency ~ Norm_Score_LAG1, data = df1)
summary(model1h) # => Positive correlation is significant

model1wsls <- lm(Consistency ~ Norm_Score_LAG1, data = df2)
summary(model1wsls) # => Positive correlation is significant

model1fra <- lm(Consistency ~ Norm_Score_LAG1, data = df3)
summary(model1fra) # => Positive correlation is significant

p1 <- ggplot(df1, aes(x = Norm_Score_LAG1, y = Consistency)) + 
  geom_point(shape = 19) + 
  geom_smooth(method='lm', se=FALSE) + 
  theme_bw() +
  xlab("Score(n-1)") +
  xlim(c(0,1)) +
  ylab("Consistency(n)") +
  ylim(c(0,1)) +
  ggtitle('Humans')

p2 <- ggplot(df2, aes(x = Norm_Score_LAG1, y = Consistency)) + 
  geom_point(shape = 19) + 
  geom_smooth(method='lm', se=FALSE) + 
  theme_bw() +
  xlab("Score(n-1)") +
  xlim(c(0,1)) +
  ylab("") +
  ylim(c(0,1)) +
  ggtitle('WSLS')

p3 <- ggplot(df3, aes(x = Norm_Score_LAG1, y = Consistency)) + 
  geom_point(shape = 19) + 
  geom_smooth(method='lm', se=FALSE) + 
  theme_bw() +
  xlab("Score(n-1)") +
  xlim(c(0,1)) +
  ylab("") +
  ylim(c(0,1)) +
  ggtitle('FRA')

grid.arrange(p1, p2, p3, nrow = 1, layout_matrix = rbind(c(1, 2, 3)))

g <- arrangeGrob(p1, p2, p3, nrow = 1, layout_matrix = rbind(c(1, 2, 3)))

ggsave("ConsistencyWRTScore.eps", width=10, height=3.5, device=cairo_ps, g)


# ----------------------------------

df1 <- df1[complete.cases(df1), ]
df2 <- df2[complete.cases(df2), ]
df3 <- df3[complete.cases(df3), ]

# Regressing DLIndex w.r.t. Consistency
model2h <- lm(DLIndex ~ Consistency, data = df1)
summary(model2h) # => Positive correlation is significant

model2wsls <- lm(DLIndex ~ Consistency, data = df2)
summary(model2wsls) # => Positive correlation is significant

model2fra <- lm(DLIndex ~ Consistency, data = df3)
summary(model2fra) # => Positive correlation is significant

# ----------------------------------

# Regressing DLIndex w.r.t. Consistency with interaction between Joint(n-1) and Dif_Consist
model3h <- lm(DLIndex ~ Consistency + Dif_consist*Joint_LAG1, data = df1)
summary(model3h) # => Positive interaction is significant

anova(model2h, model3h) # => interaction effect significantly adds over main effect

model3wsls <- lm(DLIndex ~ Consistency + Dif_consist*Joint_LAG1, data = df2)
summary(model3wsls) # => Positive interaction is significant

anova(model2wsls, model3wsls) # => interaction effect significantly adds over main effect

model3fra <- lm(DLIndex ~ Consistency + Dif_consist*Joint_LAG1, data = df3)
summary(model3fra) # => Positive interaction is NOT significant!

anova(model2fra, model3fra) # => more effects significantly adds over main effect

# ----------------------------------

# Regressing Consistency w.r.t. max similarity to focal region
model4h <- lm(Consistency ~ Distancias_LAG1, data = df1)
summary(model4h) # => Positive correlation is significant

model4wsls <- lm(Consistency ~ Distancias_LAG1, data = df2)
summary(model4wsls) # => Positive correlation is significant

model4fra <- lm(Consistency ~ Distancias_LAG1, data = df3)
summary(model4fra) # => Positive correlation is significant

p1 <- ggplot(df1, aes(x = Distancias_LAG1, y = Consistency)) + 
  geom_point(shape = 19) + 
  geom_smooth(method='lm', se=FALSE) + 
  theme_bw() +
  xlab("") +
  xlim(c(0,1)) +
  ylab("Consistency(n)") +
  ylim(c(0,1)) +
  ggtitle('Humans')

p2 <- ggplot(df2, aes(x = Distancias_LAG1, y = Consistency)) + 
  geom_point(shape = 19) + 
  geom_smooth(method='lm', se=FALSE) + 
  theme_bw() +
  xlab("Max. similarity to Focal Region (n-1)") +
  xlim(c(0,1)) +
  ylab("") +
  ylim(c(0,1)) +
  ggtitle('WSLS')

p3 <- ggplot(df3, aes(x = Distancias_LAG1, y = Consistency)) + 
  geom_point(shape = 19) + 
  geom_smooth(method='lm', se=FALSE) + 
  theme_bw() +
  xlab("") +
  xlim(c(0,1)) +
  ylab("") +
  ylim(c(0,1)) +
  ggtitle('FRA')

grid.arrange(p1, p2, p3, nrow = 1, layout_matrix = rbind(c(1, 2, 3)))

g <- arrangeGrob(p1, p2, p3, nrow = 1, layout_matrix = rbind(c(1, 2, 3)))

ggsave("ConsistencyWRTDist2FR.eps", width=10, height=3.5, device=cairo_ps, g)

# ----------------------------------
