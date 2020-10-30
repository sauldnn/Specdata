pollutantmean <- function(directory, pollutant, id) {
  permonitor<-matrix(nrow = length(id), ncol = 2)
  contador=1
  for(i in id) {
    libro<-read.csv(paste(directory, "/" , id, ".csv", sep=""), header=TRUE)
    Agente<-libro$pollutant
    good<-complete.cases(Agente)
    permonitor[contador, 2]<-length(Agente[good])
    permonitor[contador, 1]<-sum(Agente, na.rm = TRUE)
    contador<-contador+1
  }
  Promesa<-sum(permonitor[,1])/sum(permonitor[,2])
}
  