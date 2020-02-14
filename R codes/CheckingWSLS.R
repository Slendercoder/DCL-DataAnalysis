library(ggplot2)
library(dplyr)
library(gridExtra)

###########################################################################################
###########################################################################################

plot_RSTransitions <- function(df) {

  df_RS <- df[df$Region == 'RS', ]
  head(df_RS)
  
  gRS <- ggplot() +
    geom_point(aes(x = Score, y = Freqs, color=RegionGo), df_RS, alpha = 0.4, size=1.5) +
    scale_x_continuous(limits = c(min_score, 35)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab("Score") +
    #  ylab("") +
    ylab("Rel. Freq./Probability") +
    ggtitle("Transitions from RS") +
    theme_bw()
  
  gRS
  
  df_RS <- df[df$Region == 'RS', ]
  df_RS <- df_RS[df_RS$RegionGo != 'ALL', ]
  df_RS <- df_RS[df_RS$RegionGo != 'NOTHING', ]
  df_RS <- df_RS[df_RS$RegionGo != 'DOWN', ]
  df_RS <- df_RS[df_RS$RegionGo != 'UP', ]
  df_RS <- df_RS[df_RS$RegionGo != 'LEFT', ]
  df_RS <- df_RS[df_RS$RegionGo != 'RIGHT', ]
  df_RS <- df_RS[df_RS$RegionGo != 'IN', ]
  df_RS <- df_RS[df_RS$RegionGo != 'OUT', ]
  head(df_RS)
  
  gRS2RS <- ggplot() +
    geom_point(aes(x = Score, y = Freqs), df_RS, alpha = alpha, size=1.5) +
    scale_x_continuous(limits = c(min_score, 35)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab("Score") +
    #  ylab("") +
    ylab("Rel. Freq./Probability") +
    ggtitle("Staying at RS") +
    theme_bw()
  
  return (gRS2RS)

}

plot_FocalTransitions <- function(df) {
    
  regiones <- c('RS', 'ALL', 'NOTHING', 
                'DOWN', 'UP', 'LEFT', 'RIGHT',
                'IN', 'OUT')
  
  gOTHER2OTHER <- ggplot() +
    scale_x_continuous(limits = c(min_score, 35)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab("Score") +
    #  ylab("") +
    ylab("Rel. Freq./Probability") +
    ggtitle("Staying at same focal region") +
    theme_bw()
  
  
  #other <- 'RIGHT'
  #other <- 'LEFT'
  for (other in regiones) {
    df_RS <- df[df$Region == other, ]
    for (x in regiones) {
      if (x != other) {
        df_RS <- df_RS[df_RS$RegionGo != x, ]
      }
    }
    gOTHER2OTHER <- gOTHER2OTHER +
      geom_point(aes(x = Score, y = Freqs), df_RS, alpha = alpha, size=1.5)
  }

  return (gOTHER2OTHER)
  
}

###########################################################################################
###########################################################################################

df2 = read.csv("../Python Codes/output.csv")
df2$Region <- df2$Category

model1wsls <- lm(Consistency ~ Score_LAG1, data = df2)
summary(model1wsls) # => Positive correlation is significant

df1 <- df2[df2$Category != 'RS', ]
df1 <- df2[df2$Category_LAG1 != 'RS', ]

model2wsls <- lm(Consistency ~ Score_LAG1, data = df1)
summary(model2wsls) # => Positive correlation is significant

#min_score = -50
min_score = 0
alpha <- 0.1

# Full data
p1 <- ggplot(df2, aes(x = Score_LAG1, y = Consistency)) + 
  geom_point(shape = 19, alpha = alpha) + 
  geom_smooth(method='lm', se=FALSE) + 
  theme_bw() +
  xlab("Score(n-1)") +
  ylab("Consistency(n)") +
  xlim(c(min_score,32)) +
  ylim(c(0,1)) +
  ggtitle('Full data')

# Only focal regions
p2 <- ggplot(df1, aes(x = Score_LAG1, y = Consistency)) + 
  geom_point(shape = 19, alpha = alpha) + 
  geom_smooth(method='lm', se=FALSE) + 
  theme_bw() +
  xlab("Score(n-1)") +
  ylab("Consistency(n)") +
  xlim(c(min_score,32)) +
  ylim(c(0,1)) +
  ggtitle('Only focal regions')

############################
# Obtaining frequencies...
############################

dfA <- df2 %>% select('Region', 'Score', 'RegionGo')
df <- dfA %>%
  dplyr::group_by(Region, Score, RegionGo) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Region, Score) %>%
  dplyr::mutate(n1 = sum(n),
                Freqs = n/n1)

head(df)

############################
# Ploting data...
############################

g1 <- plot_RSTransitions(df)
g2 <- plot_FocalTransitions(df)
gB <- grid.arrange(p1, p2, g1, g2, nrow=2)

