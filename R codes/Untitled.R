
a <- seq(0, 9)
for (contador in a) {
  archivo <- paste("MBiases_Parameter_fit_nmkb_", contador, ".csv", sep="")
  
  df = read.csv(archivo)