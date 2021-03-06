library(dplyr)
source("ClassifyRegions.R")

letterCode <- function(x, letras) {
  code <- ''
  for (i in 1:length(x)) {
    if (x[i]==1) {
      code <- paste(code, letras[i], sep = '')
    }
  }
  #  if (code=='') {code <- 'NOTHING'}
  return(code)
}

getFreqFromGameFRA_2 <- function(df1) {
  
  df1 <- df1[complete.cases(df1), ]
  
  # Get visited tiles for each player
  perDyad <- df1 %>% group_by(Dyad)
  perPlayer <- perDyad %>% group_by(Player)
  
  # Create vector for columns with region
  columns <- c()
  for (i in c(1:8)) {
    for (j in c(1:8)) {
      columns <- append(columns, paste('a', paste(i, j, sep=''), sep=''))
    }
  }
  
  # Build the region column per player
  perPlayer$visitedRegion <- lapply(as.list(as.data.frame(t(perPlayer[columns]))), function(x) x)
  perDyad <- perPlayer %>% ungroup
  aux <- perDyad %>% ungroup
  df1$vR <- aux$visitedRegion
  head(df1)
  
  # Build the frequency table for each (i,j,s) triple
  auxDF <- data.frame(c('Dyad', NA), 
                      c('Player', NA), 
                      #                    c('Region', NA), 
                      c('RegionFULL', NA), 
                      #                    c('RegionGo', NA), 
                      c('RJoint', NA), 
                      c('Score', NA))
  colnames(auxDF) = as.character(unlist(auxDF[1, ])) # the first row will be the header
  auxDF = auxDF[-1, ]          # removing the first row.
  auxDF = auxDF[-1, ]          # removing the first row.
  #auxDF$RJoint <- list(0)
  #auxDF$Score <- 0
  
  parejas <- unique(df1$Dyad)
  pareja <- parejas[2]
  
  for (pareja in unique(df1$Dyad)) {
    # Create the joint region
    parejaDF <- df1[which(df1$Dyad == pareja), ]
    parejaDF[order(parejaDF$Round), ]
    jugador <- unique(parejaDF$Player)
    r1 <- parejaDF$vR[which(parejaDF$Player == jugador[1])]
    r2 <- parejaDF$vR[which(parejaDF$Player == jugador[2])]
    newDF <- data.frame(rep(0, length(r1)))
    newDF$a <- r1
    newDF$b <- r2
    lst <- as.list(as.data.frame(t(newDF)))
    newDF$rJoint <- lapply(lst, function(x) as.numeric(unlist(x[2])) * as.numeric(unlist(x[3])))
    
    # Create dataframe for first player
    DF <- data.frame(seq(1, length(r1), by=1))
    DF$Dyad <- rep(pareja, length(r1))
    DF$Player <- rep(as.character(jugador[1]), length(r1))
    DF$Region <- parejaDF$Category[which(parejaDF$Player == jugador[1])]
    DF$RegionFULL <- parejaDF$vR[which(parejaDF$Player == jugador[1])]
    DF$RegionGo <- lead(DF$Region, 2)
    DF$RJoint <- newDF$rJoint
    DF$Score <- parejaDF$Score[which(parejaDF$Player == jugador[1])]
    DF <- DF[c('Dyad', 'Player', 'Region', 'RegionFULL', 'RegionGo', 'RJoint', 'Score')]
    #  DF <- DF[c('Dyad', 'Player', 'RegionFULL', 'RJoint', 'Score')]
    
    # Add dataframe to big dataframe
    auxDF <- rbind(auxDF, DF)
    #  auxDF <- na.omit(auxDF)
    
    # Create dataframe for second player
    DF <- data.frame(seq(1, length(r2), by=1))
    DF$Dyad <- rep(pareja, length(r2))
    DF$Player <- rep(as.character(jugador[2]), length(r2))
    DF$Region <- parejaDF$Category[which(parejaDF$Player == jugador[2])]
    DF$RegionFULL <- parejaDF$vR[which(parejaDF$Player == jugador[2])]
    DF$RegionGo <- lead(DF$Region, 2)
    DF$RJoint <- newDF$rJoint
    DF$Score <- parejaDF$Score[which(parejaDF$Player == jugador[2])]
    DF <- DF[c('Dyad', 'Player', 'Region', 'RegionFULL', 'RegionGo', 'RJoint', 'Score')]
    #  DF <- DF[c('Dyad', 'Player', 'RegionFULL', 'RJoint', 'Score')]
    
    # Add dataframe to big dataframe
    auxDF <- rbind(auxDF, DF)
    #  auxDF <- na.omit(auxDF)
  }
  head(auxDF)
  dim(auxDF)
  auxDF$Player <- as.character(auxDF$Player)
  
  # Code overlapping regions 
  lst <- as.list(as.data.frame(t(auxDF$RJoint)))
  regionesJuntos <- lapply(lst, function(x) {
    cadena <- as.character(unlist(x))
    letterCode(cadena, letras)
  })
  
  auxDF$RJcode <- regionesJuntos
  
  # summary(auxDF$Score)
  # Divide the scores in three levels: 1 -> (-128, 13) | 2 -> (14, 27) | 3 -> (28, 32)
  #auxDF$scoreLevel <- sapply(auxDF$Score, function(x) {
  #  if (x < 14) {return(1)}
  #  else if (x < 28) {return(2)}
  #  else {return(3)}
  #})
  
  aux <- auxDF[c('Dyad', 'Player', 'Region', 'RegionGo', 'RJcode', 'Score')]
  aux$Region <- as.character(aux$Region)
  aux$RegionGo <- as.character(aux$RegionGo)
  #  aux$Rcode <- as.character(aux$Rcode)
  aux$RJcode <- as.character(aux$RJcode)
  head(aux)
  
  return(aux)
}

getFreqFromGameFRA_1 <- function(df1) {
  
  print('Running...')
#  print(head(df1))
  
#  df1 <- df1[complete.cases(df1), ]
  
  # Get visited tiles for each player
  print('Creating perPlayer...')
  perDyad <- df1 %>% group_by(Dyad)
  perPlayer <- perDyad %>% group_by(Player)
  
  # Create vector for columns with region
  print('Creating columns...')
  columns <- c()
  for (i in c(1:8)) {
    for (j in c(1:8)) {
      columns <- append(columns, paste('a', paste(i, j, sep=''), sep=''))
    }
  }
  
  # Build the region column per player
  print('Building visited region column...')
  perPlayer$visitedRegion <- lapply(as.list(as.data.frame(t(perPlayer[columns]))), function(x) x)
  perDyad <- perPlayer %>% ungroup
  aux <- perDyad %>% ungroup
  df1$vR <- aux$visitedRegion
  head(df1)
  
  # Build the frequency table for each (i,j,s) triple
  print('Building frequency table...')
  auxDF <- data.frame(c('Dyad', NA), 
                      c('Player', NA), 
                      #                    c('Region', NA), 
                      c('RegionFULL', NA), 
                      #                    c('RegionGo', NA), 
                      c('RJoint', NA), 
                      c('Score', NA))
  colnames(auxDF) = as.character(unlist(auxDF[1, ])) # the first row will be the header
  auxDF = auxDF[-1, ]          # removing the first row.
  auxDF = auxDF[-1, ]          # removing the first row.
  #auxDF$RJoint <- list(0)
  #auxDF$Score <- 0
  
  parejas <- unique(df1$Dyad)
#  pareja <- parejas[2]
  
  print('...creating joint region...')
  for (pareja in unique(df1$Dyad)) {
    print('Working with dyad:')
    print(pareja)
    # Create the joint region
    parejaDF <- df1[which(df1$Dyad == pareja), ]
    parejaDF[order(parejaDF$Round), ]
    jugador <- unique(parejaDF$Player)
    r1 <- parejaDF$vR[which(parejaDF$Player == jugador[1])]
    r2 <- parejaDF$vR[which(parejaDF$Player == jugador[2])]
    newDF <- data.frame(rep(0, length(r1)))
    newDF$a <- r1
    newDF$b <- r2
    lst <- as.list(as.data.frame(t(newDF)))
    newDF$rJoint <- lapply(lst, function(x) as.numeric(unlist(x[2])) * as.numeric(unlist(x[3])))
    
    # Create dataframe for first player
    DF <- data.frame(seq(1, length(r1), by=1))
    DF$Dyad <- rep(pareja, length(r1))
    DF$Player <- rep(as.character(jugador[1]), length(r1))
    DF$Region <- parejaDF$Category[which(parejaDF$Player == jugador[1])]
    DF$RegionFULL <- parejaDF$vR[which(parejaDF$Player == jugador[1])]
    DF$RegionGo <- lead(DF$Region, 1)
    DF$RJoint <- newDF$rJoint
    DF$Score <- parejaDF$Score[which(parejaDF$Player == jugador[1])]
    DF <- DF[c('Dyad', 'Player', 'Region', 'RegionFULL', 'RegionGo', 'RJoint', 'Score')]
    #  DF <- DF[c('Dyad', 'Player', 'RegionFULL', 'RJoint', 'Score')]
    
    # Add dataframe to big dataframe
    auxDF <- rbind(auxDF, DF)
    #  auxDF <- na.omit(auxDF)
    
    # Create dataframe for second player
    DF <- data.frame(seq(1, length(r2), by=1))
    DF$Dyad <- rep(pareja, length(r2))
    DF$Player <- rep(as.character(jugador[2]), length(r2))
    DF$Region <- parejaDF$Category[which(parejaDF$Player == jugador[2])]
    DF$RegionFULL <- parejaDF$vR[which(parejaDF$Player == jugador[2])]
    DF$RegionGo <- lead(DF$Region, 1)
    DF$RJoint <- newDF$rJoint
    DF$Score <- parejaDF$Score[which(parejaDF$Player == jugador[2])]
    DF <- DF[c('Dyad', 'Player', 'Region', 'RegionFULL', 'RegionGo', 'RJoint', 'Score')]
    #  DF <- DF[c('Dyad', 'Player', 'RegionFULL', 'RJoint', 'Score')]
    
    # Add dataframe to big dataframe
    auxDF <- rbind(auxDF, DF)
    #  auxDF <- na.omit(auxDF)
  }
  head(auxDF)
  dim(auxDF)
  auxDF$Player <- as.character(auxDF$Player)
  
  # Code overlapping regions 
  lst <- as.list(as.data.frame(t(auxDF$RJoint)))
  regionesJuntos <- lapply(lst, function(x) {
    cadena <- as.character(unlist(x))
    letterCode(cadena, letras)
  })
  
  auxDF$RJcode <- regionesJuntos
  
  # summary(auxDF$Score)
  # Divide the scores in three levels: 1 -> (-128, 13) | 2 -> (14, 27) | 3 -> (28, 32)
  #auxDF$scoreLevel <- sapply(auxDF$Score, function(x) {
  #  if (x < 14) {return(1)}
  #  else if (x < 28) {return(2)}
  #  else {return(3)}
  #})
  
  aux <- auxDF[c('Dyad', 'Player', 'Region', 'RegionGo', 'RJcode', 'Score')]
  aux$Region <- as.character(aux$Region)
  aux$RegionGo <- as.character(aux$RegionGo)
  #  aux$Rcode <- as.character(aux$Rcode)
  aux$RJcode <- as.character(aux$RJcode)
  head(aux)
  
  return(aux)
}

getFreqFromGameFRA <- function(df1) {
  
  aux <- getFreqFromGameFRA_1(df1)
#  aux1 <- getFreqFromGameFRA_1(df1)
#  head(aux1)
#  aux2 <- getFreqFromGameFRA_2(df1)
#  head(aux2)
  
#  aux <- merge(aux1, aux2)
  aux <- aux[complete.cases(aux), ]
  head(aux)
  write.csv(aux, file = "frequencies.csv", row.names = FALSE)
  print('Frequencies written in frequencies.csv!')
}
