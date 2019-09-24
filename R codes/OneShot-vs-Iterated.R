library(ggplot2)

df1 = read.csv("humans.csv")
# head(df1)

# ----------------------------------

# Check the performance on the first round, as if one-shot task
DLIRound60 <- df1$DLIndex[which(df1$Round == 60)]
mean(DLIRound60)
sd(DLIRound60)
DLIRound1 <- df1$DLIndex[which(df1$Round == 1)]
mean(DLIRound1)
sd(DLIRound1)
wilcox.test(DLIRound60, DLIRound1) # => Difference one-shot task vs iterated task is significant


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
