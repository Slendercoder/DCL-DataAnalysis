library(ggplot2)

df1 = read.csv("humans_full.csv")
head(df1)

# ----------------------------------

# Check the performance on the first round, as if one-shot task
DLIRound60 <- df1$DLIndex[which(df1$Round == 60)]
mean(DLIRound60)
sd(DLIRound60)
DLIRound1 <- df1$DLIndex[which(df1$Round == 1)]
mean(DLIRound1)
sd(DLIRound1)
wilcox.test(DLIRound60, DLIRound1) # => Difference one-shot task vs iterated task is significant

DLIRound60 <- data.frame(DLIRound60)
names(DLIRound60)[1] <- "DLIndex"
DLIRound60$Exp <- as.character("Last")
DLIRound1 <- data.frame(DLIRound1)
names(DLIRound1)[1] <- "DLIndex"
DLIRound1$Exp <- as.character("First")
df <- rbind(DLIRound1, DLIRound60)

# Density plot
g2 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
  geom_density(size=1) +
  scale_colour_manual(values = c("Last" = "#E69F00", "First" = "#56B4E9")) +  
  #  scale_y_continuous(limits = c(0, 5)) + 
  scale_y_continuous(position = "right") +
  labs(color = "Round") +
  theme_bw() +
  theme(legend.position="bottom")

g2

# Plot frequencies of regions
n <- length(df1$Category[which(df1$Round == 1)])
A <- as.data.frame(rep('First Round', n))
colnames(A) = c('Condition')
A$Strategy <- df1$Category[which(df1$Round == 1)]
head(A)
n <- length(df1$Category)
B <- as.data.frame(rep('60 Rounds', n))
colnames(B) = c('Condition')
B$Strategy <- df1$Category

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
  geom_bar(aes(y = ..prop..*100), stat="count", position="dodge") +
  #  geom_text(aes(label = scales::percent(..prop..),
  #                 y= ..prop.. ), stat= "count", vjust = -.5) +
  #  labs(y = "Percent", fill="Region") +
  xlab("Region") +
  ylab("Trials on which region\n was uncovered (%)") +
  scale_fill_manual(name = "Trials on",
                    values = c("60 Rounds" = "#E69F00", "First Round" = "#56B4E9")) +  
  #  facet_grid(~Condition) +
  #  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  theme_bw() +
  theme(legend.position="bottom")

p

grid.arrange(p, g2, 
             nrow = 1,
             widths=c(0.7, 0.3))

