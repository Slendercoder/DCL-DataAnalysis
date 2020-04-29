source("WSpred.R")
source("FRApred.R")
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
              'DOWN', 
              'UP', 
              'LEFT', 
              'RIGHT', 
              'IN', 
              'OUT')

cbPalette <- c("#999999","#004949","#ff6db6",
               "#490092","#b66dff","#b6dbff",
               "#924900","#24ff24","#ffff6d")
#cbPalette <- c("#999999","#004949","#009292","#ff6db6","#ffb6db",
#               "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
#               "#920000","#924900","#db6d00","#24ff24","#ffff6d")
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

alpha <- 0.5

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

plot_RSTransitions <- function(df) {
  
  min_score <- 0
  max_score <- 33
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
  
  regiones <- c('ALL', 'NOTHING', 
                'DOWN', 'UP', 'LEFT', 'RIGHT',
                'IN', 'OUT')
  
  min_score <- 0
  max_score <- 33
  
  gOTHER2OTHER <- ggplot() +
    scale_x_continuous(limits = c(min_score, max_score)) + 
    scale_y_continuous(limits = c(0, 1.01)) + 
    xlab("Score") +
    #  ylab("") +
    ylab("Rel. Freq./Probability") +
    ggtitle("Staying at same focal region") +
    theme_bw()
  
  
  #other <- 'RIGHT'
  #other <- 'LEFT'
  for (other in regiones) {
    df_Focal <- df[df$Region == other, ]
    df_Focal <- df_Focal[df_Focal$RegionGo == other, ]
    gOTHER2OTHER <- gOTHER2OTHER +
      geom_point(aes(x = Score, y = Freqs), df_Focal, alpha = alpha, size=1.5)
  }
  
  return (gOTHER2OTHER)
  
}

plot_ModelTransitions_RS <- function(theta, pl, plColor) {
  
  xs <- seq(-128,32,length.out=161)
  fitRS <- sapply(xs, WSprob, i='RS', k='RS', theta=theta)
  dfB <- data.frame(xs, fitRS)
  pl <- pl +
    geom_line(aes(x = xs, y = fitRS), dfB, color=plColor, size=1)
  
  return(pl)
}

plot_ModelTransitions_Focal <- function(theta, pl, plColor) {
  
  xs <- seq(-128,32,length.out=161)
  regiones <- c('ALL', 'NOTHING', 
                'DOWN', 'UP', 'LEFT', 'RIGHT',
                'IN', 'OUT')
  for (other in regiones) {
    fitFocal <- sapply(xs, WSprob, i=other, k=other, theta=theta)
    dfB <- data.frame(xs, fitFocal)
    pl <- pl +
      geom_line(aes(x = xs, y = fitFocal), dfB, color=plColor, size=1)
  }
  
  return(pl)
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
  
plot_RSTransitions_FRA <- function(df, k) {
  
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
  max_score <- 2
  rotulo_x <- paste("FRASim", k, sep="")

  # Plot RS to RS
  df_Focal <- df1[df1$Region == 'RS', ]
  df_Focal <- df_Focal[df_Focal$RegionGo == 'RS', ]
  color_a_usar <- cbPalette[which(regiones == 'RS')]
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

  color_a_usar <- cbPalette[which(regiones == 'RS')]
  
  xs <- seq(0,2,length.out=200)
  fitFocal <- sapply(xs, ModelProb, regionFrom='RS', regionGo='RS', k=k, theta=theta)
  dfB <- data.frame(xs, fitFocal)
  g1 <- g1 +
    geom_line(aes(x = xs, y = fitFocal), dfB, color=color_a_usar, size=0.8)
  
  return(g1)

}
  
plot_Transitions_FRASim_k <- function(df1, k) {
  
  min_score <- 0
  max_score <- 2
  rotulo_x <- paste("FRASim", k, sep="")
  titulo <- paste("From RS to", k)
  
  # Plot RS to k
  df_Focal <- df1[df1$Region == 'RS', ]
  df_Focal <- df_Focal[df_Focal$RegionGo == k, ]
  color_a_usar <- cbPalette[which(regiones == k)]
  g2 <- ggplot() +
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
    ggtitle(titulo) +
    theme_bw() +
    theme(legend.position="none")

  return(g2)
  
}

plot_ModelTransition_k_FRA <- function(df1, theta, k) {
  
  pl <- plot_Transitions_FRASim_k(df1, k)

  color_a_usar <- cbPalette[which(regiones == k)]

  xs <- seq(0,2,length.out=200)
  fitFocal <- sapply(xs, ModelProb, regionFrom='RS', regionGo=k, k=k, theta=theta)
  dfB <- data.frame(xs, fitFocal)
  pl <- pl +
    geom_line(aes(x = xs, y = fitFocal), dfB, color=color_a_usar, size=0.8)

  return(pl)

}

plot_FRA_regs <- function(df, regs) {
  
  n <- length(regs)
  k <- regs[1]
  df1 <- getFreq_based_on_FRASim(df, k)
  q <- plot_FRASim_k_RS2RS(df1, k)
  print(paste('Plotting from RS to', k))
  p <- plot_ModelTransition_k_FRA(df1, theta, k)
  pl <- grid.arrange(q, p, nrow=1)
  
  if (n > 1) {
    contador <- 2
    for (k in regs[2:n]) {
      print(paste('Plotting from RS to', k))
      df1 <- getFreq_based_on_FRASim(df, k)
      q <- plot_FRASim_k_RS2RS(df1, k)
      p <- plot_ModelTransition_k_FRA(df1, theta, k)
      pl1 <- grid.arrange(q, p, nrow=1)
      pl <- grid.arrange(pl, pl1, nrow=2, heights=c((contador - 1)/contador, 1/contador))
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

plot_behavior <- function(df2, theta, model=TRUE) {
  
  # Plots the behavior of data in terms of:
  # Top left -- Scatter plot: Consistency(n) ~ Score(n-1)
  # Top middle -- Two-way interaction effect: absolute difference in consistency(n) * overlap(n-1)
  # Top right -- Scatter plot: Consistency(n) ~ log max similarity to focal region
  # Center left -- DLindex vs. round
  # Center middle -- Kernel Density Estimate of DLindex
  # Center right -- WSLS
  
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
  # Top right -- Scatter plot: Consistency(n) ~ log max similarity to focal region
  p3 <- ggplot(df2, aes(log(Similarity_LAG1), Consistency)) +
    geom_point(alpha = 1/8) +
    xlab("Log of max similarity w.r.t.\nfocal regions on Round n-1") +
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
  
  # 6...
  # Center right -- WSLS
  df2 <- getRelFreq_Rows(df2)
  p6 <- plot_FocalTransitions(df2)

  if (model==TRUE) {
    p6 <- plot_ModelTransitions_Focal(theta, p6,"#999999")
    parametros <- para_visualizar(theta)
    parametros <- paste("Parameters:", parametros)
  } else {
    parametros <- ""
  }
  
  top <- grid.arrange(p1, p2, p3, nrow=1)
  center <- grid.arrange(p4, p5, p6, nrow=1)
  gB <- grid.arrange(top, center, 
                     nrow=2,
                     bottom=parametros)
  
  return(gB)
  
}

savePlot <- function(file, myPlot) {
  pdf(file)
  print(myPlot)
  dev.off()
}