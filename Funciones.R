pollutantmean <- function(directory, pollutant, id = 1:332) {
  permonitor<-matrix(nrow = length(id), ncol = 2)
  contador<-1
  good<-0
  for(i in id) {
    libro<-read.csv(paste(directory, "/" , sprintf('%0.3d',i), ".csv", sep=""), header=TRUE)
    Agente<-libro[[pollutant]]
    good<-complete.cases(Agente)
    numm<-length(Agente[good])
    permonitor[contador, 2]<-numm
    permonitor[contador, 1]<-sum(Agente, na.rm = TRUE)
    contador<-contador+1
  }
  Promesa<-sum(permonitor[,1])/sum(permonitor[,2])
  Promesa
}

complete<-function(directory, id){
  mediciones<-matrix(nrow = length(id), ncol = 2)
  contador<-1
  for (i in id) {
    libro<-read.csv(paste(directory, "/" , sprintf('%0.3d',i), ".csv", sep=""), header=TRUE)
    good<-complete.cases(libro)
    numero<-length(libro[good,][,1])
    mediciones[contador, 2]<-numero
    mediciones[contador, 1]<-i
    contador<-contador+1
  }
  colnames(mediciones)<-c("id", "nobs")
  mediciones
}

corr<-function(directory, threshold=0){
  vec_cor<-vector("numeric", length=333)
  cont <- 1
  for (i in 1:10) {
    libro <- read.csv(paste(directory, "/", sprintf('%0.3d',i), ".csv", sep=""), header=TRUE)
    good <- complete.cases(libro)
    completo <- libro[good, ]
    
    numobs<-complete(directory,i)
 
    if (numobs[[1, 2]] < threshold){
      next
    }
    else{
      nitrate <- completo[["nitrate"]]
      sulfate <- completo[["sulfate"]]
      vec_cor[cont]<-cor(nitrate, sulfate)
      cont<-cont+1
    }
  }
  vec_cor
}
