source("WSpred.R")
# source("FRApred.R")
library(ggplot2)
library(gridExtra)
library(Rmisc)
library(sjPlot)
library(sjmisc)

####################################################################################
# Global variables
####################################################################################

regiones <- c('RS',
              'ALL', 
              'NOTHING', 
              'BOTTOM', 
              'TOP', 
              'LEFT', 
              'RIGHT', 
              'IN', 
              'OUT')

#cbPalette <- c("#999999","#004949","#ff6db6",
#               "#490092","#b66dff","#b6dbff",
#               "#924900","#24ff24","#ffff6d")

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#924900","#24ff24","#ffff6d","#b6dbff")

#cbPalette <- c("#999999","#004949","#009292","#ff6db6","#ffb6db",
#               "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
#               "#920000","#924900","#db6d00","#24ff24","#ffff6d")
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

alpha <- 1

####################################################################################
# Functions
####################################################################################

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

get_legend_from_dummy <- function(True_model_color, Recovered_model_color) {
  
  min_score <- 0
  max_score <- 33
  xs <- seq(min_score,32,length.out=(32-min_score + 1)*1000)
  dummyplot <- ggplot() +
    geom_line(aes(x = xs, y = xs, color = "True"), size = 1) + 
    geom_line(aes(x = xs, y = xs, color = "Recovered"), size = 1) + 
    scale_x_continuous(limits = c(min_score, max_score)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    scale_color_manual(values=c("True"=True_model_color,
                                "Recovered"=Recovered_model_color),
                       name="")  +
    theme_bw() +
    theme(legend.position="bottom")
  
  legend2 <- get_legend(dummyplot)
  
  return(legend2)
  
}

get_legend_from_dummy1 <- function(True_model_color, Recovered_model_color) {
  
  min_score <- 0
  max_score <- 33
  xs <- seq(min_score,32,length.out=(32-min_score + 1)*1000)
  dummyplot <- ggplot() +
    geom_line(aes(x = xs, y = xs, color = "WSLS"), size = 1.5) + 
    geom_line(aes(x = xs, y = xs, color = "FRA"), size = 1.5) + 
    scale_x_continuous(limits = c(min_score, max_score)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    scale_color_manual(values=c("WSLS"=True_model_color,
                                "FRA"=Recovered_model_color),
                       name="")  +
    theme_bw() +
    theme(legend.position="bottom")
  
  legend2 <- get_legend(dummyplot)
  
  return(legend2)
  
}

plot_RSTransitions <- function(df) {
  
  min_score <- 0
  max_score <- 33
  df_RS <- df[df$Region == 'RS', ]
  df_RS <- df_RS[df_RS$RegionGo != 'ALL', ]
  df_RS <- df_RS[df_RS$RegionGo != 'NOTHING', ]
  df_RS <- df_RS[df_RS$RegionGo != 'BOTTOM', ]
  df_RS <- df_RS[df_RS$RegionGo != 'TOP', ]
  df_RS <- df_RS[df_RS$RegionGo != 'LEFT', ]
  df_RS <- df_RS[df_RS$RegionGo != 'RIGHT', ]
  df_RS <- df_RS[df_RS$RegionGo != 'IN', ]
  df_RS <- df_RS[df_RS$RegionGo != 'OUT', ]
  print(dim(df_RS))
  head(df_RS)
  
  gRS2RS <- ggplot() +
    geom_point(aes(x = Score, y = Freqs), df_RS, alpha = alpha, size=1.5) +
    scale_x_continuous(limits = c(min_score, max_score)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab("Score") +
    #  ylab("") +
    ylab("Rel. Freq./Probability") +
    ggtitle("Staying at RS") +
    theme_bw()
  
  return (gRS2RS)
  
}

plot_FocalTransitions <- function(df) {
  
  df <- df[df$Region != 'RS', ]
  df <- df[df$Region == df$RegionGo, ]
  
  min_score <- 0
  max_score <- 33
  alpha <- 0.5
  
  g <- ggplot() +
    geom_point(aes(x = Score, y = Freqs), df, alpha = alpha, size=1.5) + 
    scale_x_continuous(limits = c(min_score, max_score)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab("Score") +
    #  ylab("") +
    ylab("Rel. Freq./Probability") +
    ggtitle("Re-select focal") +
    theme_bw()
  
  return (g)
  
}

plot_FocalTransitions1 <- function(df) {
  
  df <- df[df$Region != 'RS', ]
  
  min_score <- 0
  max_score <- 33
  
  gOTHER2OTHER <- ggplot() +
    scale_x_continuous(limits = c(min_score, max_score)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab("Score") +
    #  ylab("") +
    ylab("Rel. Freq./Probability") +
    ggtitle("Re-select focal") +
    theme_bw()
  
  regiones <- c('ALL', 'NOTHING', 
                'BOTTOM', 'TOP', 'LEFT', 'RIGHT',
                'IN', 'OUT')
  
  x <- 2
  for (other in regiones) {
    df_Focal <- df[df$Region == other, ]
    df_Focal <- df_Focal[df_Focal$RegionGo == other, ]
    print(dim(df_Focal))
    gOTHER2OTHER <- gOTHER2OTHER +
      geom_point(aes(x = Score, 
                     y = Freqs, 
                     color=cbPalette[x]), 
                 df_Focal, alpha = alpha, size=1.5)
    x <- x + 1
  }
  
  return (gOTHER2OTHER)
  
}

plot_ModelTransitions_RS <- function(theta, pl, plColor) {
  
  xs <- seq(0,32,length.out=200)
  fitRS <- sapply(xs, WSprob, i='RS', k='RS', theta=theta)
  dfB <- data.frame(xs, fitRS)
  pl <- pl +
    geom_line(aes(x = xs, y = fitRS), dfB, color=plColor, size=0.7)
  
  return(pl)
}

plot_ModelTransitions_Focal <- function(theta, pl, plColor) {
  
  xs <- seq(0,32,length.out=200)
  # print('xs')
  # print(xs)
  regiones <- c('ALL', 'NOTHING', 
                'BOTTOM', 'TOP', 'LEFT', 'RIGHT',
                'IN', 'OUT')
  for (other in regiones) {
    fitFocal <- sapply(xs, WSprob, i=other, k=other, theta=theta)
    print(paste('fitFocal', other))
    print(fitFocal)
    dfB <- data.frame(xs, fitFocal)
    pl <- pl +
      geom_line(aes(x = xs, y = fitFocal), dfB, color=plColor, size=0.7)
  }
  
  return(pl)
}

plot_FocalTransitions_3models <- function(df1, df2, df3) {
  
  labeldf1 <- "MBiases"
  labeldf2 <- "WSLS"
  labeldf3 <- "FRA"
  colordf1 <- cbPalette[4]
  colordf2 <- cbPalette[5]
  colordf3 <- cbPalette[7]
  
  df1 <- getRelFreq_Rows(df1)
  df1 <- df1[df1$Region != 'RS', ]
  df1$Exp <- as.character("MBiases")
  
  df2 <- getRelFreq_Rows(df2)
  df2 <- df2[df2$Region != 'RS', ]
  df2$Exp <- as.character("WSLS")
  
  df3 <- getRelFreq_Rows(df3)
  df3 <- df3[df3$Region != 'RS', ]
  df3$Exp <- as.character("FRA")
  
  df <- rbind(df1, df2, df3)

  min_score <- 0
  max_score <- 33
  
  gOTHER2OTHER <- ggplot() +
    scale_x_continuous(limits = c(min_score, max_score)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab("Score") +
    #  ylab("") +
    ylab("Relative Freq.") +
    ggtitle("Stubbornness") +
    theme_bw()
  
  regiones <- c('ALL', 'NOTHING', 
                'BOTTOM', 'TOP', 'LEFT', 'RIGHT',
                'IN', 'OUT')
  
  for (other in regiones) {
    df_Focal <- df[df$Region == other, ]
    df_Focal <- df_Focal[df_Focal$RegionGo == other, ]
    gOTHER2OTHER <- gOTHER2OTHER +
      geom_point(aes(x = Score, y = Freqs, color=Exp), 
                 data = df_Focal, 
                 alpha = alpha, 
                 size=1.5) +
      scale_colour_manual(values = c(colordf1,colordf2,colordf3)) +  
      theme_bw() +
      theme(legend.position="none")
  }
  
  return (gOTHER2OTHER)
  
}

plot_6panels <- function(archivo) {
  
  df2 = read.csv(archivo)
  df2$Region <- df2$Category
  df1 <- df2[df2$Category != 'RS', ]
  df1 <- df1[df1$Category_LAG1 != 'RS', ]
  ############################
  # Obtaining frequencies...
  ############################
  df <- getRelFreq_Rows(df2)
  head(df)
  ############################
  # Ploting data...
  ############################
  # Regression full data
  p1 <- ggplot(df2, aes(x = Score, y = Consistency_LEAD1)) + 
    geom_point(shape = 19, alpha = alpha) + 
    geom_smooth(method='lm', se=FALSE) + 
    theme_bw() +
    xlab("Score(n-1)") +
    ylab("Consistency(n)") +
    xlim(c(min_score,32)) +
    ylim(c(0,1)) +
    ggtitle('Full data')
  # Regression only focal regions
  p2 <- ggplot(df1, aes(x = Score, y = Consistency_LEAD1)) + 
    geom_point(shape = 19, alpha = alpha) + 
    geom_smooth(method='lm', se=FALSE) + 
    theme_bw() +
    xlab("Score(n-1)") +
    ylab("Consistency(n)") +
    xlim(c(min_score,32)) +
    ylim(c(0,1)) +
    ggtitle('Only focal regions')
  # Summarize data
  dfc_DLIndex <- summarySE(df2, measurevar="DLIndex", groupvars="Round")
  head(dfc_DLIndex)
  # Plot DLIndex with error regions
  g1 <- ggplot(dfc_DLIndex, aes(x=Round, y=DLIndex)) +
    geom_line(size=0.7) +
    geom_ribbon(aes(ymin = DLIndex - sd,
                    ymax = DLIndex + sd), alpha = 0.2) +
    #  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
    labs(color = "Source") +
    xlab("Round (unicorn absent)") +
    ylab("DLindex") +
    ylim(c(0,1.2)) +
    ggtitle('Division of labor') +
    theme_bw()
  # Density plot
  g2 <- ggplot(df2, aes(DLIndex)) +
    geom_density(size=1) +
    #  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
    #  scale_y_continuous(limits = c(0, 5)) + 
    labs(color = "Source of data") +
    ggtitle('Kernel density estimate') +
    theme_bw()
  d1 <- plot_RSTransitions(df)
  d1 <- plot_ModelTransitions_RS(theta, d1,"#999999")
  d2 <- plot_FocalTransitions(df)
  d2 <- plot_ModelTransitions_Focal(theta, d2,"#999999")
  params <- para_visualizar(theta)
  gB <- grid.arrange(p1, d1, g1, p2, d2, g2, nrow=2,
                     bottom=params)
  
  return (gB)
}

plot_4panels <- function(archivo, theta) {
  
  df2 = read.csv(archivo)
  df2$Region <- df2$Category
  df1 <- df2[df2$Category != 'RS', ]
  df1 <- df1[df1$Category_LAG1 != 'RS', ]
  ############################
  # Obtaining frequencies...
  ############################
  df <- getRelFreq_Rows(df2)
  head(df)
  ############################
  # Ploting data...
  ############################
  min_score <- 0
  max_score <- 33
  # Regression full data
  p1 <- ggplot(df2, aes(x = Score_LAG1, y = Consistency)) + 
    geom_point(shape = 19, alpha = alpha) + 
    geom_smooth(method='lm', se=FALSE) + 
    theme_bw() +
    xlab("Score(n-1)") +
    ylab("Consistency(n)") +
    xlim(c(min_score,max_score)) +
    ylim(c(0,1)) +
    ggtitle('Full data')
  # Regression only focal regions
  p2 <- ggplot(df1, aes(x = Score_LAG1, y = Consistency)) + 
    geom_point(shape = 19, alpha = alpha) + 
    geom_smooth(method='lm', se=FALSE) + 
    theme_bw() +
    xlab("Score(n-1)") +
    ylab("Consistency(n)") +
    xlim(c(min_score,32)) +
    ylim(c(0,1)) +
    ggtitle('Only focal regions')

  d1 <- plot_RSTransitions(df)
  d1 <- plot_ModelTransitions_RS(theta, d1,"#999999")
  d2 <- plot_FocalTransitions(df)
  d2 <- plot_ModelTransitions_Focal(theta, d2,"#999999")
  params <- para_visualizar(theta)
  gB <- grid.arrange(p1, p2, d1, d2, nrow=2,
                     bottom=params)
  
  return (gB)
}

plot_2panels <- function(archivo) {
  
  df2 = read.csv(archivo)
  df2$Region <- df2$Category
  
  ############################
  # Ploting data...
  ############################
  # Summarize data
  dfc_DLIndex <- summarySE(df2, measurevar="DLIndex", groupvars="Round")
  head(dfc_DLIndex)
  # Plot DLIndex with error regions
  g1 <- ggplot(dfc_DLIndex, aes(x=Round, y=DLIndex)) +
    geom_line(size=0.7) +
    geom_ribbon(aes(ymin = DLIndex - sd,
                    ymax = DLIndex + sd), alpha = 0.2) +
    #  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
    labs(color = "Source") +
    xlab("Round (unicorn absent)") +
    ylab("DLindex") +
    ylim(c(0,1.2)) +
    ggtitle('Division of labor') +
    theme_bw()
  # Density plot
  g2 <- ggplot(df2, aes(DLIndex)) +
    geom_density(size=1) +
    #  scale_colour_manual(values = c("Observed behavior" = "#999999", "WSLS" = "#E69F00", "FRA" = "#56B4E9")) +  
    #  scale_y_continuous(limits = c(0, 5)) + 
    labs(color = "Source of data") +
    ggtitle('Kernel density estimate') +
    theme_bw()
  
  gB <- grid.arrange(g1, g2, nrow=2,
                     bottom=params)
  
  return (gB)
}

plot_ModelRecovery_Dyad <- function(model_to_recover, model_recovered, True_model_color, Recovered_model_color) {
  
  df1 = read.csv(model_to_recover)
  df1$Exp <- as.character("Initial")
  head(df1)
  df2 = read.csv(model_recovered)
  df2$Exp <- as.character("Recovered")
  
  # Create single data frame with DLIndexes
  df <- rbind(
    df1[c('Round', 
          'DLIndex',
          'Consistency',
          'Category',
          'Norm_Score_LAG1',
          #        'Similarity_LAG1',
          'Exp')],
    df2[c('Round', 
          'DLIndex',
          'Consistency',
          'Category',
          'Norm_Score_LAG1',
          #        'Similarity_LAG1',
          'Exp')]
  )
  df$Exp <- as.factor(df$Exp)
  df$Exp <- factor(df$Exp, levels = c('Initial', 'Recovered'))
  head(df)
  # levels(df$Exp)
  
  # Summarize data
  dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
  head(dfc_DLIndex)
  
  # Plot DLIndex with error regions
  g1 <- ggplot(dfc_DLIndex, aes(x = Round, y = DLIndex, colour=Exp, group=Exp)) +
    geom_line(size=0.7) +
    geom_ribbon(aes(ymin = DLIndex - sd,
                    ymax = DLIndex + sd), alpha = 0.2) +
    scale_colour_manual(values = c("Initial" = True_model_color,
                                   "Recovered" = Recovered_model_color)) +  
    labs(color = "Source") +
    xlab("Round (unicorn absent)") +
    ylab("Division of labor") +
    theme_bw()
  
  g1
  
  # Density plot
  g2 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
    geom_density(size=1) +
    scale_colour_manual(values = c("Initial" = True_model_color,
                                   "Recovered" = Recovered_model_color)) +  
    #  scale_y_continuous(limits = c(0, 5)) + 
    labs(color = "Model") +
    xlab("Division of labor") +
    theme_bw() +
    theme(legend.position="top")
  
  g2
  
  legend <- get_legend(g2)
  g1 <- g1 + theme(legend.position="none")
  g2 <- g2 + theme(legend.position="none")
  
  grid.arrange(g1, g2, 
               nrow = 1,
               bottom=legend, top="Model recovery - DLindex")
  
}

plot_Model_correction_and_recovery_Dyad <- function(model_to_recover, 
                                                    model_recovered, 
                                                    model_corrected_and_recovered, 
                                                    True_model_color, 
                                                    Recovered_model_color,
                                                    Corrected_and_recovered_model_color) {
  
  df1 = read.csv(model_to_recover)
  df1$Exp <- as.character("Initial")
  df2 = read.csv(model_recovered)
  df2$Exp <- as.character("Recovered")
  df3 = read.csv(model_corrected_and_recovered)
  df3$Exp <- as.character("Corrected and recovered")
  
  # Create single data frame with DLIndexes
  df <- rbind(
    df1[c('Round', 
          'DLIndex',
          'Consistency',
          'Category',
          'Norm_Score_LAG1',
          #        'Similarity_LAG1',
          'Exp')],
    df2[c('Round', 
          'DLIndex',
          'Consistency',
          'Category',
          'Norm_Score_LAG1',
          #        'Similarity_LAG1',
          'Exp')],
    df3[c('Round', 
          'DLIndex',
          'Consistency',
          'Category',
          'Norm_Score_LAG1',
          #        'Similarity_LAG1',
          'Exp')]
  )
  df$Exp <- as.factor(df$Exp)
  df$Exp <- factor(df$Exp, levels = c('Initial', 'Recovered', 'Corrected and recovered'))
  head(df)
  # levels(df$Exp)
  
  # Summarize data
  dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
  head(dfc_DLIndex)
  
  # Plot DLIndex with error regions
  g1 <- ggplot(dfc_DLIndex, aes(x = Round, y = DLIndex, colour=Exp, group=Exp)) +
    geom_line(size=0.7) +
    geom_ribbon(aes(ymin = DLIndex - sd,
                    ymax = DLIndex + sd), alpha = 0.2) +
    scale_colour_manual(values = c("Initial" = True_model_color, 
                                   "Recovered" = Recovered_model_color, 
                                   "Corrected and recovered" = Corrected_and_recovered_model_color)) +  
    labs(color = "Source") +
    xlab("Round (unicorn absent)") +
    ylab("Division of labor") +
    theme_bw()
  
  g1
  
  # Density plot
  g2 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
    geom_density(size=1) +
    scale_colour_manual(values = c("Initial" = True_model_color, 
                                   "Recovered" = Recovered_model_color, 
                                   "Corrected and recovered" = Corrected_and_recovered_model_color)) +  
    #  scale_y_continuous(limits = c(0, 5)) + 
    labs(color = "Model") +
    xlab("Division of labor") +
    theme_bw() +
    theme(legend.position="top")
  
  g2
  
  legend <- get_legend(g2)
  g1 <- g1 + theme(legend.position="none")
  g2 <- g2 + theme(legend.position="none")
  
  grid.arrange(g1, g2, 
               nrow = 1,
               bottom=legend, top="Model correction and recovery - DLindex")
  
}

plot_sample_variation <- function(ruta, N) {
  
  # Create single data frame
  print("Creating dataframe...")
  archivo = paste(ruta, 1, ".csv", sep="")
  df = read.csv(archivo)
  df$Exp <- as.character("1")
  df <- df[c('Round', 'DLIndex', 'Exp')]
  for (i in seq(2, N, by=1)) {
    
    archivo = paste(ruta, i, ".csv", sep="")
    df1 = read.csv(archivo)
    df1$Exp <- as.character(i)
    df1 <- df1[c('Round', 'DLIndex', 'Exp')]
    df <- rbind(df, df1)
  }
  df$Exp <- as.factor(df$Exp)

  # Summarize data
  print("Summarizing...")
  dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))

  # Plot DLIndex with error regions
  print("Plotting...")
  g1 <- ggplot(dfc_DLIndex, aes(x = Round, y = DLIndex, colour=Exp, group=Exp)) +
    geom_line(size=0.7) +
    geom_ribbon(aes(ymin = DLIndex - sd,
                    ymax = DLIndex + sd), alpha = 1/(N)) +
    xlab("Round (unicorn absent)") +
    ylab("Division of labor") +
    theme_bw() +
    theme(legend.position="none")

  # Density plot
  g2 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
    geom_density(size=1) +
    #  scale_y_continuous(limits = c(0, 5)) + 
    xlab("Division of labor") +
    theme_bw() +
    theme(legend.position="none")
  
  grid.arrange(g1, g2, nrow = 1)
  
}

ModelProb <- function(regionFrom, regionGo, s, k, theta){
  
  # FRA model returns probability of going from regionFrom to regionGo
  # given FRA similarity to region k
  
  wALL <- theta[1]
  wNOTHING <- theta[2]
  wLEFT <- theta[3]
  wIN <- theta[4]
  alpha <- theta[5]
  beta <- theta[6]
  gamma <- theta[7]
  delta0 <- theta[8]
  epsilon <- theta[9]
  zeta <- theta[10]
  delta1 <- theta[11]
  delta2 <- theta[12]
  delta3 <- theta[13]
  
  aux <- c(wALL, wNOTHING, wLEFT, wLEFT, wLEFT, wLEFT, wIN, wIN)
  
  # The probability of region 'RS' is 1 - the sum of the other probabilities
  if (sum(aux) > 1) {
    aux <- aux/sum(aux)
  }
  bias <- c(1 - sum(aux), aux)
  #  print('bias')
  #  imprimir(bias)
  
  # Find the attractivenes:
  focal <- 0
  index <- which(regiones == regionGo)
  #  print(paste('Indice regiones:', index))
  attractiveness <- bias[index]
  #  print(paste('attractiveness:', attractiveness))
  
  if (regionGo == 'ALL') {
    delta <- delta0
  } else if (reginGo == 'NOTHING') {
    delta <- delta1
  } else if (reginGo == 'LEFT') {
    delta <- delta2
  } else if (reginGo == 'RIGHT') {
    delta <- delta2
  } else if (reginGo == 'TOP') {
    delta <- delta2
  } else if (reginGo == 'BOTTOM') {
    delta <- delta2
  } else if (reginGo == 'IN') {
    delta <- delta3
  } else if (reginGo == 'OUT') {
    delta <- delta3
  }
  
  #  print(paste('Considering case from', regionFrom, 'to', regionGo))
  if (regionFrom == 'RS') { 
    #    print('Entering case from RS')
    if (regionGo != 'RS') {
      #      print('Entering case to Focal')
      if(k == regionGo) {
        attractiveness <- attractiveness + delta * sigmoid(s, epsilon, zeta)
        focal <- delta * sigmoid(s, epsilon, zeta)
      }
    } else {
      #      print('Entering case to RS')
      focal <- delta * sigmoid(s, epsilon, zeta)
    }
  } else {
    #    print('Entering case from Focal')
    if(regionFrom == regionGo) {
      attractiveness <- attractiveness + delta
      focal <- delta
    } else if(k == regionGo) {
      attractiveness <- attractiveness +  delta * sigmoid(s, epsilon, zeta)
      focal <- delta * sigmoid(s, epsilon, zeta)
    }
  }
  
  probab <- attractiveness / (1 + focal)
  
  return(probab)
} # end ModelProb

plot_RSTransitions_FRA <- function(df, k) {
  
  df_RS <- df[df$Region == 'RS', ]
  df_RS <- df_RS[df_RS$RegionGo != 'ALL', ]
  df_RS <- df_RS[df_RS$RegionGo != 'NOTHING', ]
  df_RS <- df_RS[df_RS$RegionGo != 'BOTTOM', ]
  df_RS <- df_RS[df_RS$RegionGo != 'TOP', ]
  df_RS <- df_RS[df_RS$RegionGo != 'LEFT', ]
  df_RS <- df_RS[df_RS$RegionGo != 'RIGHT', ]
  df_RS <- df_RS[df_RS$RegionGo != 'IN', ]
  df_RS <- df_RS[df_RS$RegionGo != 'OUT', ]
  print(dim(df_RS))
  head(df_RS)
  
  rotulo_x <- paste("FRASim", k, sep="")
  
  gRS2RS <- ggplot() +
    geom_point(aes(x = FRASim, y = Freqs), 
               data = df_RS, 
               alpha = alpha, 
               size = 3,
               shape = 4) +
    scale_x_continuous(limits = c(min_score, max_score)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab(rotulo_x) +
    #  ylab("") +
    ylab("Rel. Freq./Probability") +
    ggtitle("Staying at RS") +
    theme_bw()
  
  return (gRS2RS)
  
}

plot_FRASim_k_RS2RS <- function(df1, k) {
  
  min_score <- 0
  max_score <- 1.2
  rotulo_x <- paste("FRASim", k, sep="")

  # Plot RS to RS
  df_Focal <- df1[df1$Region == 'RS', ]
  df_Focal <- df_Focal[df_Focal$RegionGo == 'RS', ]
  print(dim(df_Focal))
  color_a_usar <- cbPalette[1]
  g1 <- ggplot() +
    geom_point(aes(x = FRASim, y = Freqs, shape=RegionGo, color=RegionGo, group=RegionGo),
               data = df_Focal, 
               color = color_a_usar, 
               alpha = alpha, 
               size=3,
               shape = 4) +
    scale_x_continuous(limits = c(min_score, max_score)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab(rotulo_x) +
    ylab("Rel. Freq./Probability") +
    ggtitle("Staying at RS") +
    theme_bw() +
    theme(legend.position="none")

  color_a_usar <- cbPalette[7]
  
  xs <- seq(0,2,length.out=200)
  fitFocal <- sapply(xs, ModelProb, regionFrom='RS', regionGo='RS', k=k, theta=theta)
  dfB <- data.frame(xs, fitFocal)
  g1 <- g1 +
    geom_line(aes(x = xs, y = fitFocal), dfB, color=color_a_usar, size=0.7)
  
  return(g1)

}
  
plot_Transitions_FRASim_k <- function(args, k) {
  
  num_k <- which(regiones == k)
  min_score <- 0
  max_score <- 1.2
  alpha <- 0.5
  rotulo_x <- paste("FRASim", k, sep="")
  columna <- which(colnames(args)==rotulo_x)
  titulo <- paste("From RS to", k)
  print(paste("Plotting", titulo))
  
  # Plot RS to k
  df_Focal <- args[args$Region == 'RS', ]
  df_Focal <- df_Focal[, c(5,num_k + 4)]
  df_Focal$y <- lapply(df_Focal$freqs, function(x) return(unlist(x)[num_k]))
  df_Focal$Freqs <- unlist(df_Focal$y)
  df_Focal <- df_Focal[, c(2,4)]
  names(df_Focal) <- c('FRASim', 'Freqs')

  color_a_usar <- cbPalette[1]
  g2 <- ggplot() +
    geom_point(aes(x = FRASim, y = Freqs),#, shape=RegionGo, color=RegionGo, group=RegionGo),
               data = df_Focal, 
               color = color_a_usar, 
               alpha = alpha, 
               size=3,
               shape = 4) +
    scale_x_continuous(limits = c(min_score, max_score)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab(rotulo_x) +
    ylab("Rel. Freq./Probability") +
    ggtitle(titulo) +
    theme_bw() +
    theme(legend.position="none")

  return(g2)
  
}

plot_ModelTransition_k_FRA <- function(df1, theta, k) {
  
  pl <- plot_Transitions_FRASim_k(df1, k)

  color_a_usar <- cbPalette[7]

  xs <- seq(0,2,length.out=200)
  fitFocal <- sapply(xs, ModelProb, regionFrom='RS', regionGo=k, k=k, theta=theta)
  dfB <- data.frame(xs, fitFocal)
  pl <- pl +
    geom_line(aes(x = xs, y = fitFocal), dfB, color=color_a_usar, size=0.7)

  return(pl)

}

plot_FRA_regs <- function(df, regs, theta) {
  
  n <- length(regs)
  k <- regs[1]
  df1 <- getFreq_based_on_FRASim(df, k)
  # q <- plot_FRASim_k_RS2RS(df1, k)
  # print(paste('Plotting from RS to', k))
  p <- plot_ModelTransition_k_FRA(df1, theta, k)
  # pl <- grid.arrange(q, p, nrow=1)
  pl <- grid.arrange(p)
  
  if (n > 1) {
    contador <- 2
    for (k in regs[2:n]) {
      # print(paste('Plotting from RS to', k))
      df1 <- getFreq_based_on_FRASim(df, k)
      # q <- plot_FRASim_k_RS2RS(df1, k)
      p <- plot_ModelTransition_k_FRA(df1, theta, k)
      # pl1 <- grid.arrange(q, p, nrow=1)
      # pl <- grid.arrange(pl, pl1, nrow=2, heights=c((contador - 1)/contador, 1/contador))
      pl <- grid.arrange(pl, p, nrow=1, widths=c((contador - 1)/contador, 1/contador))
      contador <- contador + 1
    }
  }
  
  return (pl)
  
}

plot_FRA_regs1 <- function(df, regs, theta) {
  
  # NO RS2RS Transition
  
  n <- length(regs)
  k <- regs[1]
  df1 <- getFreq_based_on_FRASim(df, k)
  print(paste('Plotting from RS to', k))
  pl <- plot_ModelTransition_k_FRA(df1, theta, k)

  if (n > 1) {
    contador <- 2
    for (k in regs[2:n]) {
      print(paste('Plotting from RS to', k))
      df1 <- getFreq_based_on_FRASim(df, k)
      p <- plot_ModelTransition_k_FRA(df1, theta, k)
      pl <- grid.arrange(pl, p, nrow=1, widths=c((contador - 1)/contador, 1/contador))
      contador <- contador + 1
    }
  }
  
  return (pl)
  
}

plot_behavior <- function(df2) {
  
  # Plots the behavior of data in terms of:
  # Top left -- Scatter plot: Consistency(n) ~ Score(n-1)
  # Top middle -- Two-way interaction effect: absolute difference in consistency(n) * overlap(n-1)
  # Top right -- Scatter plot: Consistency(n) ~ log max similarity to focal region
  # Center left -- DLindex vs. round
  # Center right -- Kernel Density Estimate of DLindex

  min_score <- 0
  max_score <- 33

  # 1...
  # Top left -- Scatter plot: Consistency(n) ~ Score(n-1)
  # Regression full data
  p1 <- ggplot(df2, aes(x = Score, y = Consistency_LEAD1)) + 
    geom_point(shape = 19, alpha = alpha) + 
    geom_smooth(method='lm', se=FALSE) + 
    theme_bw() +
    xlab("Score(n-1)") +
    ylab("Consistency(n)") +
    xlim(c(min_score,32)) +
    ylim(c(0,1)) +
    ggtitle('Full data')

  # 2...
  # Top middle -- Two-way interaction effect: absolute difference in consistency(n) * overlap(n-1)
  model3h <- lm(DLIndex ~ Consistency + Dif_consist*Joint_LAG1, data = df2)
  p2 <- plot_model(model3h, 
                   type = "pred", 
                   terms = c("Dif_consist", "Joint_LAG1"), 
                   colors = c("black", "red", "blue"),
                   title = "",
                   legend.title = "Overlap",
                   axis.title = c("Absolute difference\nin consistency", "DLindex"))
  
  # 3...
  # Top right -- Scatter plot: Consistency(n) ~ Max similarity to focal region
  p3 <- ggplot(df2, aes(Similarity_LAG1, Consistency)) +
    geom_point(alpha = 1/8) +
    xlab("Max similarity w.r.t.\nfocal regions on Round n-1") +
    ylab("Consistency\n on Round n") +
    geom_smooth(method = lm)
  
  # 4...
  # Center left -- DLindex vs. round
  # Summarize data
  dfc_DLIndex <- summarySE(df2, measurevar="DLIndex", groupvars=c("Round"))
  # Plot DLIndex with error regions
  p4 <- ggplot(dfc_DLIndex, aes(x = Round, y = DLIndex)) +
    geom_line(size=0.7) +
    geom_ribbon(aes(ymin = DLIndex - sd,
                    ymax = DLIndex + sd), alpha = alpha) +
    xlab("Round (unicorn absent)") +
    ylab("Division of labor") +
    theme_bw()
  
  # 5...
  # Center middle -- Kernel Density Estimate of DLindex
  p5 <- ggplot(df2, aes(DLIndex)) +
    geom_density(size=1) +
    #  scale_y_continuous(limits = c(0, 5)) + 
    xlab("Division of labor") +
    theme_bw()
  
  top <- grid.arrange(p1, p2, p3, nrow=1)
  center <- grid.arrange(p4, p5, nrow=1)
  gB <- grid.arrange(top, center, 
                     nrow=2)
  
  return(gB)
  
}

plot_3set_comparison_WSLS <- function(df1, df2, df3) {
  
  # Plot three panels
  
  min_score <- 0
  max_score <- 33
  
  labeldf1 <- "MBiases"
  labeldf2 <- "WSLS"
  labeldf3 <- "FRA"
  colordf1 <- cbPalette[4]
  colordf2 <- cbPalette[5]
  colordf3 <- cbPalette[7]
  
  df <- rbind(
    df1[c('Round', 
          'DLIndex',
          'Consistency',
          'Consistency_LEAD1',
          'Category',
          'Score',
          'Score_LAG1',
          'Dif_consist',
          'Joint_LAG1',
          'Similarity_LAG1',
          'Exp')],
    df2[c('Round', 
          'DLIndex',
          'Consistency',
          'Consistency_LEAD1',
          'Category',
          'Score',
          'Score_LAG1',
          'Dif_consist',
          'Joint_LAG1',
          'Similarity_LAG1',
          'Exp')],
    df3[c('Round', 
          'DLIndex',
          'Consistency',
          'Consistency_LEAD1',
          'Category',
          'Score',
          'Score_LAG1',
          'Dif_consist',
          'Joint_LAG1',
          'Similarity_LAG1',
          'Exp')]
  )
  df$Exp <- as.factor(df$Exp)
  df$Exp <- factor(df$Exp, levels = c(labeldf1, labeldf2, labeldf3))
  df$Category <- lapply(df$Category, function(x) {
    if(x=='ALL') {
      return('A.')
    } else if(x=='NOTHING') {
      return('N.')
    } else if(x=='BOTTOM') {
      return('B.')
    } else if(x=='TOP') {
      return('T.')
    } else if(x=='T.') {
      return('B.')
    } else if(x=='LEFT') {
      return('L.')
    } else if(x=='L.') {
      return('B.')
    } else if(x=='RIGHT') {
      return('R.')
    } else if(x=='IN') {
      return('I.')
    } else if(x=='OUT') {
      return('O.')
    } else {
      return(as.character(x))
    }
  })
  df$Category <- unlist(df$Category)
  df$Category <- as.factor(df$Category)
  df$Category <- factor(df$Category, levels = c('RS',
                                                'A.', 
                                                'N.', 
                                                'B.', 
                                                'T.', 
                                                'L.', 
                                                'R.', 
                                                'I.', 
                                                'O.'))
  
  # 1...
  # Top left -- Bar plot: Percentage of trials regions uncovered
  g1 <- ggplot(df, aes(x=Category,  group=Exp, fill=Exp)) + 
    geom_bar(aes(y = ..prop..*100), stat="count", position="dodge") +
    xlab("Region") +
    ylab("% of trials region\n was uncovered") +
    scale_fill_manual(name = "Model",
                      values = c(colordf1,colordf2,colordf3)) +  
    theme_bw() +
    theme(legend.position="bottom")
  
  # 2...
  # Consistency(n) ~ Score(n-1)
  g2 <- ggplot(df, aes(x = Score, y = Consistency_LEAD1, color=Exp)) +
    geom_point(alpha = 1/8) +
    scale_colour_manual(values = c(colordf1,colordf2,colordf3)) +  
    xlab("Score(n-1)") +
    ylab("Consistency(n)") +
    xlim(c(min_score,max_score)) +
    ylim(c(0,1)) +
    geom_smooth(method = lm) + 
    theme_bw() +
    theme(legend.position="bottom")
  
  # 3...
  # Consistency(n) ~ Max similarity to focal region
  g3 <- ggplot(df, aes(x = Similarity_LAG1, y = Consistency, color=Exp)) +
    geom_point(alpha = 1/8) +
    scale_colour_manual(values = c(colordf1,colordf2,colordf3)) +  
    xlab("Max similarity w.r.t.\n focal regions(n-1)") +
    ylab("Consistency(n)") +
    xlim(c(0,1)) +
    ylim(c(0,1)) +
    geom_smooth(method = lm) + 
    theme_bw() +
    theme(legend.position="bottom")

  # 7...
  # DLindex vs. round
  # Summarize data
  dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
  # Plot DLIndex with error regions
  g7 <- ggplot(dfc_DLIndex, aes(x = Round, y = DLIndex, colour=Exp, group=Exp)) +
    geom_line(size=0.9) +
    scale_colour_manual(values = c(colordf1,colordf2,colordf3)) +  
    xlab("Round (unicorn absent)") +
    ylab("Division of labor") +
    theme_bw()
  
  legend <- get_legend(g1)
  g1 <- g1 + theme(legend.position="none")
  g2 <- g2 + theme(legend.position="none")
  g3 <- g3 + theme(legend.position="none")
  g7 <- g7 + theme(legend.position="none")

  p <- grid.arrange(g1, g2, 
                    g3, g7, 
                    bottom=legend,
                    nrow=2)

}

savePlot <- function(file, myPlot) {
  pdf(file)
  print(myPlot)
  dev.off()
}

########################################
# Plot Parameter Recovery 
########################################

plot_Parameter_Recovery_Biases <- function(data, titulo) {

  color_punto <- "gray"
  
  wALL_Real <- data$wALL[data$Exp == 'Real']
  wALL_Fitted <- data$wALL[data$Exp == 'Fitted']
  Rmax <- max(wALL_Real)
  Fmax <- max(wALL_Fitted)
  m <- max(Rmax,Fmax)
  df <- data.frame(wALL_Real, wALL_Fitted)
  g3 <- ggplot(df, aes(x = wALL_Real, y = wALL_Fitted)) +
    geom_point(color=color_punto)+
    scale_x_continuous(breaks=seq(0,m,3), limits=c(0, m)) +
#    xlim(c(0,m)) +
    ylim(c(0,m)) +
    theme_bw() + 
    geom_abline(intercept = 0, slope = 1, color="black", 
                linetype="dashed", size=0.5)
                     
  g3
  wNOTHING_Real <- data$wNOTHING[data$Exp == 'Real']
  wNOTHING_Fitted <- data$wNOTHING[data$Exp == 'Fitted']
  Rmax <- max(wNOTHING_Real)
  Fmax <- max(wNOTHING_Fitted)
  m <- max(Rmax,Fmax)
  df <- data.frame(wNOTHING_Real, wNOTHING_Fitted)
  g2 <- ggplot(df, aes(x = wNOTHING_Real, y = wNOTHING_Fitted)) +
    geom_point(color=color_punto)+
    scale_x_continuous(breaks=seq(0,m,3), limits=c(0, m)) +
#    xlim(c(0,m)) +
    ylim(c(0,m)) +
    theme_bw() + 
    geom_abline(intercept = 0, slope = 1, color="black", 
                linetype="dashed", size=0.5)
  g2
  wLEFT_Real <- data$wLEFT[data$Exp == 'Real']
  wLEFT_Fitted <- data$wLEFT[data$Exp == 'Fitted']
  Rmax <- max(wLEFT_Real)
  Fmax <- max(wLEFT_Fitted)
  m <- max(Rmax,Fmax)
  df <- data.frame(wLEFT_Real, wLEFT_Fitted)
  g1 <- ggplot(df, aes(x = wLEFT_Real, y = wLEFT_Fitted)) +
    geom_point(color=color_punto)+
    scale_x_continuous(breaks=seq(0,m,3), limits=c(0, m)) +
#    xlim(c(0,m)) +
    ylim(c(0,m)) +
    theme_bw() + 
    geom_abline(intercept = 0, slope = 1, color="black", 
                linetype="dashed", size=0.5)
  g1
  wIN_Real <- data$wIN[data$Exp == 'Real']
  wIN_Fitted <- data$wIN[data$Exp == 'Fitted']
  Rmax <- max(wIN_Real)
  Fmax <- max(wIN_Fitted)
  m <- max(Rmax,Fmax)
  df <- data.frame(wIN_Real, wIN_Fitted)
  g0 <- ggplot(df, aes(x = wIN_Real, y = wIN_Fitted)) +
    geom_point(color=color_punto)+
    scale_x_continuous(breaks=seq(0,m,3), limits=c(0, m)) +
#    xlim(c(0,m)) +
    ylim(c(0,m)) +
    theme_bw() + 
    geom_abline(intercept = 0, slope = 1, color="black", 
                linetype="dashed", size=0.5)
  g0
  
  pFR <- grid.arrange(g3, g2, g1, g0, nrow=1, top=titulo)
}

plot_Parameter_Recovery_WSLS <- function(data, titulo) {

  color_punto <- "gray"
  
  Alpha_Real <- data$Alpha[data$Exp == 'Real']
  Alpha_Fitted <- data$Alpha[data$Exp == 'Fitted']
  Rmax <- max(Alpha_Real)
  Fmax <- max(Alpha_Fitted)
  m <- max(Rmax,Fmax)
  df <- data.frame(Alpha_Real, Alpha_Fitted)
  g1 <- ggplot(df, aes(x = Alpha_Real, y = Alpha_Fitted)) +
    geom_point(color=color_punto)+
    scale_x_continuous(breaks=seq(0,m,length.out=3), limits=c(0, m)) +
#    xlim(c(0,m)) +
    ylim(c(0,m)) +
    theme_bw() + 
    geom_abline(intercept = 0, slope = 1, color="black", 
                linetype="dashed", size=0.5)
  g1
  Gamma_Real <- data$Gamma[data$Exp == 'Real']
  Gamma_Fitted <- data$Gamma[data$Exp == 'Fitted']
  Rmax <- max(Gamma_Real)
  Fmax <- max(Gamma_Fitted)
  m <- max(Rmax,Fmax)
  df <- data.frame(Gamma_Real, Gamma_Fitted)
  g2 <- ggplot(df, aes(x = Gamma_Real, y = Gamma_Fitted)) +
    geom_point(color=color_punto)+
    scale_x_continuous(breaks=seq(0,m,length.out=3), limits=c(0, m)) +
#    xlim(c(0,m)) +
    ylim(c(0,m)) +
    theme_bw() + 
    geom_abline(intercept = 0, slope = 1, color="black", 
                linetype="dashed", size=0.5)
  g2
  
  pFR <- grid.arrange(g1, g2, nrow=1, top=titulo)
}

plot_Parameter_Recovery_FRA <- function(data, titulo) {
  
  color_punto <- "gray"
  
  Zeta_Real <- data$Zeta[data$Exp == 'Real']
  Zeta_Fitted <- data$Zeta[data$Exp == 'Fitted']
  Rmax <- max(Zeta_Real)
  Fmax <- max(Zeta_Fitted)
  m <- max(Rmax,Fmax)
  df <- data.frame(Zeta_Real, Zeta_Fitted)
  g3 <- ggplot(df, aes(x = Zeta_Real, y = Zeta_Fitted)) +
    geom_point(color=color_punto)+
    scale_x_continuous(breaks=seq(0,m,length.out=3), limits=c(0, m)) +
#    xlim(c(0,m)) +
    ylim(c(0,m)) +
    theme_bw() + 
    geom_abline(intercept = 0, slope = 1, color="black", 
                linetype="dashed", size=0.5)
  g3
  Delta_Real <- data$Delta[data$Exp == 'Real']
  Delta_Fitted <- data$Delta[data$Exp == 'Fitted']
  Rmax <- max(Delta_Real)
  Fmax <- max(Delta_Fitted)
  m <- max(Rmax,Fmax)
  df <- data.frame(Delta_Real, Delta_Fitted)
  g1 <- ggplot(df, aes(x = Delta_Real, y = Delta_Fitted)) +
    geom_point(color=color_punto)+
    scale_x_continuous(breaks=seq(0,m,length.out=3), limits=c(0, m)) +
#    xlim(c(0,m)) +
    ylim(c(0,m)) +
    theme_bw() + 
    geom_abline(intercept = 0, slope = 1, color="black", 
                linetype="dashed", size=0.5)
  g1
  
  pFR <- grid.arrange(g1, g3, nrow=2, top=titulo)
}

plot_behavioral_data_fit <- function(df1, df2, df3, df4) {
  
  # Plot three panels
  
  min_score <- 0
  max_score <- 33
  
  labeldf1 <- "Humans"
  labeldf2 <- "MBiases"
  labeldf3 <- "WSLS"
  labeldf4 <- "FRA"
  colordf1 <- cbPalette[1]
  colordf2 <- cbPalette[4]
  colordf3 <- cbPalette[5]
  colordf4 <- cbPalette[7]
  
  df <- rbind(
    df1[c('Round', 
          'DLIndex',
          'Consistency',
#          'Consistency_LEAD1',
          'Category',
          'Score',
          'Score_LAG1',
          'Dif_consist',
          'Joint_LAG1',
          'Similarity_LAG1',
          'Exp')],
    df2[c('Round', 
          'DLIndex',
          'Consistency',
#          'Consistency_LEAD1',
          'Category',
          'Score',
          'Score_LAG1',
          'Dif_consist',
          'Joint_LAG1',
          'Similarity_LAG1',
          'Exp')],
    df3[c('Round', 
          'DLIndex',
          'Consistency',
#          'Consistency_LEAD1',
          'Category',
          'Score',
          'Score_LAG1',
          'Dif_consist',
          'Joint_LAG1',
          'Similarity_LAG1',
          'Exp')],
    df4[c('Round', 
          'DLIndex',
          'Consistency',
#          'Consistency_LEAD1',
          'Category',
          'Score',
          'Score_LAG1',
          'Dif_consist',
          'Joint_LAG1',
          'Similarity_LAG1',
          'Exp')]
  )
  df$Exp <- as.factor(df$Exp)
  df$Exp <- factor(df$Exp, levels = c(labeldf1, labeldf2, labeldf3, labeldf4))
  df$Category <- lapply(df$Category, function(x) {
    if(x=='ALL') {
      return('A.')
    } else if(x=='NOTHING') {
      return('N.')
    } else if(x=='BOTTOM') {
      return('B.')
    } else if(x=='TOP') {
      return('T.')
    } else if(x=='T.') {
      return('B.')
    } else if(x=='LEFT') {
      return('L.')
    } else if(x=='L.') {
      return('B.')
    } else if(x=='RIGHT') {
      return('R.')
    } else if(x=='IN') {
      return('I.')
    } else if(x=='OUT') {
      return('O.')
    } else {
      return(as.character(x))
    }
  })
  df$Category <- unlist(df$Category)
  df$Category <- as.factor(df$Category)
  df$Category <- factor(df$Category, levels = c('RS',
                                                'A.', 
                                                'N.', 
                                                'B.', 
                                                'T.', 
                                                'L.', 
                                                'R.', 
                                                'I.', 
                                                'O.'))
  
  # 1...
  # Top left -- Bar plot: Percentage of trials regions uncovered
  g1 <- ggplot(df, aes(x=Category,  group=Exp, fill=Exp)) + 
    geom_bar(aes(y = ..prop..*100), stat="count", position="dodge") +
    xlab("Region") +
    ylab("% of trials region\n was uncovered") +
    scale_fill_manual(name = "Source of data",
                      values = c(colordf1,colordf2,colordf3,colordf4)) +  
    theme_bw() +
    theme(legend.position="right")
  
  # 2...
  # Consistency(n) ~ Score(n-1)
  g2 <- ggplot(df, aes(x = Score_LAG1, y = Consistency, color=Exp)) +
    geom_point(alpha = 1/8) +
    scale_colour_manual(values = c(colordf1,colordf2,colordf3,colordf4)) +  
    xlab("Score(n-1)") +
    ylab("Consistency(n)") +
    xlim(c(min_score,max_score)) +
    ylim(c(0,1)) +
    geom_smooth(method = lm) + 
    theme_bw() +
    theme(legend.position="bottom")
  
  # 3...
  # Consistency(n) ~ Max similarity to focal region
  g3 <- ggplot(df, aes(x = Similarity_LAG1, y = Consistency, color=Exp)) +
    geom_point(alpha = 1/8) +
    scale_colour_manual(values = c(colordf1,colordf2,colordf3,colordf4)) +  
    xlab("Max similarity w.r.t.\n focal regions(n-1)") +
    ylab("Consistency(n)") +
    xlim(c(0,1)) +
    ylim(c(0,1)) +
    geom_smooth(method = lm) + 
    theme_bw() +
    theme(legend.position="bottom")
  
  # 7...
  # DLindex vs. round
  # Summarize data
  dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
  # Plot DLIndex with error regions
  g7 <- ggplot(dfc_DLIndex, aes(x = Round, y = DLIndex, colour=Exp, group=Exp)) +
    geom_line(size=0.9) +
    scale_colour_manual(values = c(colordf1,colordf2,colordf3,colordf4)) +  
    xlab("Round (unicorn absent)") +
    ylab("Division of labor") +
    theme_bw()
  
  # 8...
  # Kernel density estimate DLindex
  g8 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
    geom_density(size=1) +
    scale_colour_manual(values = c(colordf1,colordf2,colordf3,colordf4)) +  
    xlab("Division of labor") +
    # scale_y_continuous(limits = c(0, 5)) + 
    # labs(color = "Source of data") +
    theme_bw()
  
  legend <- get_legend(g1)
  g1 <- g1 + theme(legend.position="none")
  g2 <- g2 + theme(legend.position="none")
  g3 <- g3 + theme(legend.position="none")
  g7 <- g7 + theme(legend.position="none")
  g8 <- g8 + theme(legend.position="none")
  
  p <- grid.arrange(g1, g2, g3, g7, g8,legend,
                    nrow=2)

}

plot_behavioral_data_fit1 <- function(df1, df2, df3, df4) {
  
  # Plot three panels
  
  min_score <- 0
  max_score <- 33
  
  labeldf1 <- "Humans"
  labeldf2 <- "MBiases"
  labeldf3 <- "WSLS"
  labeldf4 <- "FRA"
  colordf1 <- cbPalette[1]
  colordf2 <- cbPalette[4]
  colordf3 <- cbPalette[5]
  colordf4 <- cbPalette[7]
  
  df <- rbind(
    df1[c('Round', 
          'DLIndex',
          'Consistency',
          #          'Consistency_LEAD1',
          'Category',
          'Score',
          'Score_LAG1',
          'Dif_consist',
          'Joint_LAG1',
          'Similarity_LAG1',
          'Exp')],
    df2[c('Round', 
          'DLIndex',
          'Consistency',
          #          'Consistency_LEAD1',
          'Category',
          'Score',
          'Score_LAG1',
          'Dif_consist',
          'Joint_LAG1',
          'Similarity_LAG1',
          'Exp')],
    df3[c('Round', 
          'DLIndex',
          'Consistency',
          #          'Consistency_LEAD1',
          'Category',
          'Score',
          'Score_LAG1',
          'Dif_consist',
          'Joint_LAG1',
          'Similarity_LAG1',
          'Exp')],
    df4[c('Round', 
          'DLIndex',
          'Consistency',
          #          'Consistency_LEAD1',
          'Category',
          'Score',
          'Score_LAG1',
          'Dif_consist',
          'Joint_LAG1',
          'Similarity_LAG1',
          'Exp')]
  )
  df$Exp <- as.factor(df$Exp)
  df$Exp <- factor(df$Exp, levels = c(labeldf1, labeldf2, labeldf3, labeldf4))
  df$Category <- lapply(df$Category, function(x) {
    if(x=='ALL') {
      return('A.')
    } else if(x=='NOTHING') {
      return('N.')
    } else if(x=='BOTTOM') {
      return('B.')
    } else if(x=='TOP') {
      return('T.')
    } else if(x=='T.') {
      return('B.')
    } else if(x=='LEFT') {
      return('L.')
    } else if(x=='L.') {
      return('B.')
    } else if(x=='RIGHT') {
      return('R.')
    } else if(x=='IN') {
      return('I.')
    } else if(x=='OUT') {
      return('O.')
    } else {
      return(as.character(x))
    }
  })
  df$Category <- unlist(df$Category)
  df$Category <- as.factor(df$Category)
  df$Category <- factor(df$Category, levels = c('RS',
                                                'A.', 
                                                'N.', 
                                                'B.', 
                                                'T.', 
                                                'L.', 
                                                'R.', 
                                                'I.', 
                                                'O.'))
  
  # 1...
  # Top left -- Bar plot: Percentage of trials regions uncovered
  g1 <- ggplot(df, aes(x=Category,  group=Exp, fill=Exp)) + 
    geom_bar(aes(y = ..prop..*100), stat="count", position="dodge") +
    xlab("Region") +
    ylab("% of trials region\n was uncovered") +
    scale_fill_manual(name = "Source of data",
                      values = c(colordf1,colordf2,colordf3,colordf4)) +  
    theme_bw() +
    theme(legend.position="bottom")
  
  # 2...
  # Consistency(n) ~ Score(n-1)
  g2 <- ggplot(df, aes(x = Score_LAG1, y = Consistency, color=Exp)) +
    geom_point(alpha = 1/8) +
    scale_colour_manual(values = c(colordf1,colordf2,colordf3,colordf4)) +  
    xlab("Score(n-1)") +
    ylab("Consistency(n)") +
    xlim(c(min_score,max_score)) +
    ylim(c(0,1)) +
    geom_smooth(method = lm) + 
    theme_bw() +
    theme(legend.position="bottom")
  
  # 3...
  # Consistency(n) ~ Max similarity to focal region
  g3 <- ggplot(df, aes(x = Similarity_LAG1, y = Consistency, color=Exp)) +
    geom_point(alpha = 1/8) +
    scale_colour_manual(values = c(colordf1,colordf2,colordf3,colordf4)) +  
    xlab("Max similarity w.r.t.\n focal regions(n-1)") +
    ylab("Consistency(n)") +
    xlim(c(0,1)) +
    ylim(c(0,1)) +
    geom_smooth(method = lm) + 
    theme_bw() +
    theme(legend.position="bottom")
  

  # 8...
  # Kernel density estimate DLindex
  g8 <- ggplot(df, aes(DLIndex, colour=Exp, group=Exp)) +
    geom_density(size=1) +
    scale_colour_manual(values = c(colordf1,colordf2,colordf3,colordf4)) +  
    xlab("Division of labor") +
    # scale_y_continuous(limits = c(0, 5)) + 
    # labs(color = "Source of data") +
    theme_bw()

  # Summarize data
  dfc_DLIndex <- summarySE(df, measurevar="DLIndex", groupvars=c("Exp", "Round"))
  # Plot DLIndex with error regions
  g5 <- ggplot(dfc_DLIndex, aes(x=Round, y=DLIndex, group=Exp, color=Exp)) +
    geom_line(size=0.9) +
    # geom_ribbon(aes(ymin = DLIndex - sd,
    #                 ymax = DLIndex + sd), alpha = 0.05) +
    scale_colour_manual(values = c(colordf1,colordf2,colordf3,colordf4)) +  
    labs(color = "Source") +
    xlab("Round (unicorn absent)") +
    ylab("DLindex") +
    ylim(c(0.25,1)) +
    theme_bw()
  
  legend <- get_legend(g1)
  g2 <- g2 + theme(legend.position="none")
  g3 <- g3 + theme(legend.position="none")
  g1 <- g1 + theme(legend.position="none")
  g8 <- g8 + theme(legend.position="none")
  g5 <- g5 + theme(legend.position="none")
  
  # p <- grid.arrange(g1, g2, g3, nrow=1)
  # q <- grid.arrange(g8, legend, nrow=2)
  # g <- grid.arrange(g5, q, widths=c(2/3, 1/3))
  return (grid.arrange(g5, g8, nrow=1, bottom=legend))
  
}

plot_individual_behavior <- function(df){
  
  plots <- list()
  df$Region <- df$Category
  print("Finding WS frequencies...")
  df1 <- getRelFreq_Rows(df)
  print(head(df1))
  print("Plotting WS frequencies...")
  plots[[1]] <- plot_FocalTransitions(df1)
  # pl <- grid.arrange(pl, p, nrow=1)
  print("Finding joint region...")
  df1 <- find_joint_region(df)
  print("Finding FRAsims...")
  df1 <- get_FRASims(df1)
  print("Plotting FRA frequencies...")
  regs <- c('ALL', 'LEFT')
  contador <- 2
  for (k in regs){
    print("Finding FRA frequencies...")
    df2 <- getFreq_based_on_FRASim(df1,k)
    print(head(df2))
    plots[[contador]] <- plot_Transitions_FRASim_k(df2, k)    
    # pl <- grid.arrange(pl, p, nrow=1, widths=c((contador - 1)/contador, 1/contador))
    contador <- contador + 1
  }
  return(plots)
  
} # end plot_individual_behavior

plot_model_on_top_behavior <- function(thetaWS, thetaFR, p1, p2, p3) {
  
  plots <- list()
  WS_color <- cbPalette[5]
  FR_color <- cbPalette[7]
  min_score = 0
  legend2 <- get_legend_from_dummy1(WS_color, FR_color)
  
  p1 <- plot_ModelTransitions_Focal(thetaWS, p1, WS_color)
  p1 <- plot_ModelTransitions_Focal(thetaFR, p1, FR_color)
  p1 <- grid.arrange(p1, bottom=legend2)
  plots[[1]] <- p1
  
  xs <- seq(0,2,length.out=200)
  regs <- c('ALL', 'LEFT')
  k <- regs[1]
  print(paste("Plotting FRA model on FRAsim transition", k))
  fitFocal <- sapply(xs, ModelProb, regionFrom='RS', regionGo=k, k=k, theta=unlist(thetaFR))
  dfB <- data.frame(xs, fitFocal)
  plots[[2]] <- p2 +
    geom_line(aes(x = xs, y = fitFocal), dfB, color=FR_color, size=0.7)
  k <- regs[2]
  print(paste("Plotting FRA model on FRAsim transition", k))
  fitFocal <- sapply(xs, ModelProb, regionFrom='RS', regionGo=k, k=k, theta=thetaFR)
  dfB <- data.frame(xs, fitFocal)
  plots[[3]] <- p3 +
    geom_line(aes(x = xs, y = fitFocal), dfB, color=FR_color, size=0.7)
  
  return(plots)  
  
} # plot_model_on_top_behavior
