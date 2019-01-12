Validacion <- c("Estación", "WRF average", "")

#Sacamos promedios para todos los meses para cada estación). 
WRF_average <- aggregate(Tabla_final[,12] ~ Tabla_final[,3], FUN = mean)

OBS_average <- aggregate(Tabla_final[,2] ~ Tabla_final[,3], FUN = mean)

#Crear una función para los estadísticos. 
##Correlación
library(plyr)
correlacion <- function(x){
  tmp <- cor.test(x[,12], x[,2])
  pvalue <- tmp$p.value
  estadistico <- tmp$estimate
  valores <- data.frame(c(estadistico,pvalue))
  valores
}


##Correlación
####temperatura
library(plyr)
correlacion <- function(x){
  tmp <- cor.test(x[,3], x[,5])
  pvalue <- tmp$p.value
  estadistico <- tmp$estimate
  valores <- data.frame(c(estadistico,pvalue))
  valores
}

correlaciones <- ddply(Obs_WRF, .(Obs_WRF[,1]), correlacion)

#precipitacion
library(plyr)
correlacion <- function(x){
  tmp <- cor.test(x[,4], x[,11])
  pvalue <- tmp$p.value
  estadistico <- tmp$estimate
  valores <- data.frame(c(estadistico,pvalue))
  valores
}

correlaciones <- ddply(Obs_WRF, .(Obs_WRF[,1]), correlacion)

#Mean Difference
mean_diff <- function(x){
  tmp1 <- t.test(x[,12], x[,2])
  estadistico <- tmp1$estimate
  pvalue <- tmp1$p.value
  valort <- tmp1$statistic
  #esperado menos observado
  diferencia <- tmp1$estimate[1]-tmp1$estimate[2]
  valores <- data.frame(c(diferencia,valort,pvalue))
}


diferenciasmedias <- ddply(Tabla_final, .(Tabla_final[,3]), mean_diff)
a <-t.test(Tabla_final$HourTemAve,Tabla_final$TC)

##RMSE

f <- Tabla_final$TC
o <- Tabla_final$HourTemAve 
  
RMSE <- function(x){
  error <- x[,12] - x[,2]
  tmp <- sqrt(mean(error^2))
  tmp
}

rootmeansquare <- ddply(Tabla_final, .(Tabla_final[,3]), RMSE)


RMSE(f, o)

sqrt((Tabla_final[,12] - Tabla_final[,2])^2)

#temperatura
RMSE <- function(x){
  error <- x[,3] - x[,5]
  tmp <- sqrt(mean(error^2))
  tmp
}

rootmeansquare <- ddply(Obs_WRF, .(Obs_WRF[,1]), RMSE)

#precipitacion
RMSE <- function(x){
  error <- x[,4] - x[,11]
  tmp <- sqrt(mean(error^2))
  tmp
}

rootmeansquare <- ddply(Obs_WRF, .(Obs_WRF[,1]), RMSE)

#####
tmp2 <- (x[,12], x[,2])
  estadistico <- tmp1$estimate
  pvalue <- tmp1$p.value
  valort <- tmp1$statistic
  #esperado menos observado
  diferencia <- tmp1$estimate[1]-tmp1$estimate[2]
  valores <- data.frame(c(diferencia,valort,pvalue))
}


