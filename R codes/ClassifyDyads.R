library(dplyr)

df1 = read.csv("../Data/humans_only_absent.csv")
head(df1)

byseveral <- dplyr::group_by(df1, Dyad)
regionsAll <- dplyr::summarize(byseveral, DLMean = mean(DLIndex), DLsd = sd(DLIndex), DLMax = max(DLIndex))
head(regionsAll)

# m <- mean(df1$DLIndex)
m <- 0.65

regionsAll$Division <- apply(regionsAll, 1, function(x) {
  dyad <- as.character(unlist(x[1])[[1]])
  me <- as.double(x[2])
  ma <- as.double(x[4])
  if (me > m) {
    dat1 <- df1[which(df1$Dyad == dyad), ]
#    head(dat1)
    dat2 <- dat1[which(df1$DLIndex == ma), ]
#    head(dat2)
    regs <- unique(dat2$Category)
    return(paste(regs, collapse="-"))
  } else {return('No DL')}
})

head(regionsAll)

regions <- regionsAll[which(regionsAll$DLMean > m), ]

byseveral <- dplyr::group_by(regions, Dyad)
regions <- dplyr::summarize(byseveral, DLMean = unique(DLMean), Division = paste(Division, collapse="-"))
print(dim(regions))

regions$DL <- rep('?', 26)
regions$DL[1] <- 'Inside-Outside'
regions$DL[2:4] <- 'Top-Bottom'
regions$DL[5] <- 'Mix'
regions$DL[6] <- 'Top-Bottom'
regions$DL[7] <- 'All-Nothing'
regions$DL[8] <- 'Top-Bottom'
regions$DL[9:10] <- 'All-Nothing'
regions$DL[11:12] <- 'Left-Right'
regions$DL[13] <- 'All-Nothing'
regions$DL[14] <- 'Left-Right'
regions$DL[15:16] <- 'All-Nothing'
regions$DL[17:18] <- 'Top-Bottom'
regions$DL[19:21] <- 'Left-Right'
regions$DL[22:23] <- 'Top-Bottom'
regions$DL[24] <- 'All-Nothing'
regions$DL[25:26] <- 'Left-Right'

regions[1:13, ]
regions[14:26, ]
head(regions)

dyadsHigh <- unique(regions$Dyad)
regionsAll$DL <- unlist(lapply(regionsAll$Dyad, function(x) {
  dyad <- as.character(x[1])
  if (dyad %in% dyadsHigh) {
    r <- as.character(regions$DL[which(regions$Dyad == dyad)])
    return(r)
  } else {return('RS')}
}))
head(regionsAll)

regionsAll$DL[11] <- 'Nothing-Nothing'
regionsAll$DL[24] <- 'Nothing-Nothing'
regionsAll$DL[42] <- 'Nothing-Nothing'
regionsAll$DL[5] <- 'All-All'

write.csv(regionsAll, file = '../Data/classificationDL.csv', row.names = FALSE)

byseveral <- dplyr::group_by(regionsAll, DL)
resumen <- dplyr::summarize(byseveral, average = mean(DLMean), desvEst = sd(DLMean), count = length(DL))
resumen$desvEst[is.na(resumen$desvEst)] <- 0
print(resumen)

tb <- table(regionsAll$DL)
plot(tb)
