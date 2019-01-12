#LIMPEZA DE DATOS OBSERVADOS DE ESTACIONES

#Importamos los datos de cada estaci??n. 
##Reemplazar el acr??nimo de la estaci??n para agregar una diferente.

library(readxl)
ALTZ_07_13 <- read_excel("/Volumes/Seagate/Tesis/obs_julio/ALTZ_07_13.xlsx")
View(ALTZ_07_13)

##De cada estaci??n nos quedamos con las columnas de las variables "fecha", "TempAire" y "Precipitacion".

myvars <- c("fecha", "TempAire", "Precipitacion")
ALTZ_07_13 <- ALTZ_07_13[myvars]
ATLC_07_13 <- ATLC_07_13[myvars]
CRCA_07_13 <- CRCA_07_13[myvars]
ECOG_07_13 <- ECOG_07_13[myvars]
ENCI_07_13 <- ENCI_07_13[myvars]
HCHP_07_13 <- HCHP_07_13[myvars]  
HMNT_07_13 <- HMNT_07_13[myvars]
IGUA_07_13 <- IGUA_07_13[myvars] 
IMTA_07_13 <- IMTA_07_13[myvars]
IZMT_07_13 <- IZMT_07_13[myvars] 
LCHI_07_13 <- LCHI_07_13[myvars] 
LGZM_07_13 <- LGZM_07_13[myvars] 
LMLI_07_13 <- LMLI_07_13[myvars] 
MLII_07_13 <- MLII_07_13[myvars] 
NVTO_07_13 <- NVTO_07_13[myvars]
PIZP_07_13 <- NVTO_07_13[myvars] 
PRMD_07_13 <- PRMD_07_13[myvars]
SRHT_07_13 <- SRHT_07_13[myvars] 
TPZT_07_13 <- TPZT_07_13[myvars]
TRMS_07_13 <- TRMS_07_13[myvars]
TZNT_07_13 <- TZNT_07_13[myvars]
UTTE_07_13 <- UTTE_07_13[myvars]
VLBR_07_13 <- VLBR_07_13[myvars]

#Funciones:
##Interpolaci??n lineal para missing values

LinearInterpol<- function (x) {
  Precipitacion<-na.approx(x[,3],  na.rm = FALSE) 
  Temperatura<-na.approx(x[,2],  na.rm = FALSE) 
  Weather<-cbind(Precipitacion,Temperatura)
  return(Weather)
}

##Get hourly precipitation and temperature
HourlyData<-function(Data){
  HourSumPrec <- rollapply(Data[,1],6,(sum),by=6,by.column=TRUE,align='right')
  #length(HourSumPrec)##8760 correct
  HourTemAve <- rollapply(Data[,2],6,(mean),by=6,by.column=TRUE,align='right')
  #length(HourTemAve)##8760 correct
  WeatherHour<-cbind(HourSumPrec,HourTemAve)
  return(WeatherHour)
}


#ALTZ

##Determinamos el n??mero de NAs
sum(is.na(ALTZ_07_13$TempAire)) #La estaci??n est?? incompleta

##Faltan 10 registros intermedios y los ??ltimos 5.
##Para los 10 registros intermedios corremos la funci??n de interpolaci??n lineal. 

ALTZ_07_13_interpol <- as.data.frame(LinearInterpol(ALTZ_07_13))

##Los ??ltimos 5 registros (NA) los llenamos con el ??ltimo valor registrado. 

ALTZ_07_13_interpol$TempAire[is.na(ALTZ_07_13_interpol$TempAire)] <- 9.8

ALTZ_07_13_interpol$Precipitacion[is.na(ALTZ_07_13_interpol$Precipitacion)] <- 0

##Ahora corremos la funci??n HourlyData para juntar las 6 observaciones de cada hora en una sola.

ALTZ_07_13_hourly <- as.data.frame(HourlyData(ALTZ_07_13_interpol))

#ATLC
##Determinamos el n??mero de NAs
sum(is.na(ATLC_07_13$TempAire)) #La estaci??n est?? completa

##A??n as?? corremos la funci??n de Linear Interpolation para que las columnas correspondan en la funci??n de Hourly Data. 

ATLC_07_13_interpol <- as.data.frame(LinearInterpol(ATLC_07_13))

##Corremos la funci??n HourlyData
ATLC_07_13_hourly <- HourlyData(ATLC_07_13_interpol)

#El procedimiento de la estaci??n ATLC es el mismo para todas las estaciones, excepto: 
##IZMT, LCHI, LGZM, LMLI, MLII, PIZP, TPZT, UTTE y VLBR. 

#CRCA
sum(is.na(CRCA_07_13$TempAire)) #La estaci??n est?? completa
CRCA_07_13_interpol <- as.data.frame(LinearInterpol(CRCA_07_13))
CRCA_07_13_hourly <- HourlyData(CRCA_07_13_interpol)

#ECOG
sum(is.na(ECOG_07_13$TempAire)) #La estaci??n est?? completa
ECOG_07_13_interpol <- as.data.frame(LinearInterpol(ECOG_07_13))
ECOG_07_13_hourly <- HourlyData(ECOG_07_13_interpol)

#ENCI
sum(is.na(ENCI_07_13$TempAire)) #La estaci??n est?? completa
ENCI_07_13_interpol <- as.data.frame(LinearInterpol(ENCI_07_13))
ENCI_07_13_hourly <- HourlyData(ENCI_07_13_interpol)

#HCHP
sum(is.na(HCHP_07_13$TempAire)) #La estaci??n est?? completa
HCHP_07_13_interpol <- as.data.frame(LinearInterpol(HCHP_07_13))
HCHP_07_13_hourly <- HourlyData(HCHP_07_13_interpol)

#HMNT
sum(is.na(HMNT_07_13$TempAire)) #La estaci??n est?? completa
HMNT_07_13_interpol <- as.data.frame(LinearInterpol(HMNT_07_13))
HMNT_07_13_hourly <- HourlyData(HMNT_07_13_interpol)

#IGUA
sum(is.na(IGUA_07_13$TempAire)) #La estaci??n est?? completa
IGUA_07_13_interpol <- as.data.frame(LinearInterpol(IGUA_07_13))
IGUA_07_13_hourly <- HourlyData(IGUA_07_13_interpol)

#IMTA
sum(is.na(IMTA_07_13$TempAire)) #La estaci??n est?? completa
IMTA_07_13_interpol <- as.data.frame(LinearInterpol(IMTA_07_13))
IMTA_07_13_hourly <- HourlyData(IMTA_07_13_interpol)

#IZMT
sum(is.na(IZMT_07_13$TempAire))
sum(is.na(IZMT_07_13$Precipitacion)) #La estaci??n est?? incompleta

##Faltan 45 registros de temperatura y 42 de precipitaci??n. No hay NAs al final. 
##Corremos Interpolaci??n Lineal

IZMT_07_13_interpol <- as.data.frame(LinearInterpol(IZMT_07_13))

##Ahora corremos la funci??n HourlyData para juntar las 6 observaciones de cada hora en una sola.

IZMT_07_13_hourly <- as.data.frame(HourlyData(IZMT_07_13_interpol))

#LCHI
sum(is.na(LCHI_07_13$TempAire))
sum(is.na(LCHI_07_13$Precipitacion))

##Faltan los ??ltimos 5 registros de cada varaible. 
LCHI_07_13_interpol <- as.data.frame(LinearInterpol(LCHI_07_13))

##Los ??ltimos 5 registros (NA) los llenamos con el ??ltimo valor registrado. 

LCHI_07_13_interpol$TempAire[is.na(LCHI_07_13_interpol$TempAire)] <- 13.7

LCHI_07_13_interpol$Precipitacion[is.na(LCHI_07_13_interpol$Precipitacion)] <- 0

##Ahora corremos la funci??n HourlyData para juntar las 6 observaciones de cada hora en una sola.

LCHI_07_13_hourly <- as.data.frame(HourlyData(LCHI_07_13_interpol))

#LGZM
sum(is.na(LGZM_07_13$TempAire))
sum(is.na(LGZM_07_13$Precipitacion)) #La estaci??n est?? incompleta

##Faltan los ??ltimos 4 registros de cada varaible. 
LGZM_07_13_interpol <- as.data.frame(LinearInterpol(LGZM_07_13))

##Los ??ltimos 4 registros (NA) los llenamos con el ??ltimo valor registrado. 

LGZM_07_13_interpol$TempAire[is.na(LGZM_07_13_interpol$TempAire)] <- 16.5

LGZM_07_13_interpol$Precipitacion[is.na(LGZM_07_13_interpol$Precipitacion)] <- 0

##Ahora corremos la funci??n HourlyData para juntar las 6 observaciones de cada hora en una sola.

LGZM_07_13_hourly <- as.data.frame(HourlyData(LGZM_07_13_interpol))

#LMLI
sum(is.na(LMLI_07_13$TempAire))
sum(is.na(LMLI_07_13$Precipitacion)) #La estaci??n est?? incompleta

##Faltan los ??ltimos 5 registros de cada varaible. 
LMLI_07_13_interpol <- as.data.frame(LinearInterpol(LMLI_07_13))

##Los ??ltimos 5 registros (NA) los llenamos con el ??ltimo valor registrado. 

LMLI_07_13_interpol$TempAire[is.na(LMLI_07_13_interpol$TempAire)] <- 19.3

LMLI_07_13_interpol$Precipitacion[is.na(LMLI_07_13_interpol$Precipitacion)] <- 0

##Ahora corremos la funci??n HourlyData para juntar las 6 observaciones de cada hora en una sola.

LMLI_07_13_hourly <- as.data.frame(HourlyData(LMLI_07_13_interpol))

#MLII
sum(is.na(MLII_07_13$TempAire))
sum(is.na(MLII_07_13$Precipitacion)) #La estaci??n est?? incompleta

##Faltan los ??ltimos 6 registros de cada varaible. 
MLII_07_13_interpol <- as.data.frame(LinearInterpol(MLII_07_13))

##Los ??ltimos 6 registros (NA) los llenamos con el ??ltimo valor registrado. 

MLII_07_13_interpol$TempAire[is.na(MLII_07_13_interpol$TempAire)] <- 18.5

MLII_07_13_interpol$Precipitacion[is.na(MLII_07_13_interpol$Precipitacion)] <- 0

##Ahora corremos la funci??n HourlyData para juntar las 6 observaciones de cada hora en una sola.

MLII_07_13_hourly <- as.data.frame(HourlyData(MLII_07_13_interpol))

#NVTO
sum(is.na(NVTO_07_13$TempAire)) #La estaci??n est?? completa
NVTO_07_13_interpol <- as.data.frame(LinearInterpol(NVTO_07_13))
NVTO_07_13_hourly <- HourlyData(NVTO_07_13_interpol)

#PIZP
sum(is.na(PIZP_07_13$TempAire))
sum(is.na(PIZP_07_13$Precipitacion)) #La estaci??n est?? incompleta
PIZP_07_13_interpol <- as.data.frame(LinearInterpol(PIZP_07_13))
PIZP_07_13_hourly <- HourlyData(PIZP_07_13_interpol)

#PRMD
sum(is.na(PRMD_07_13$TempAire)) #La estaci??n est?? completa
PRMD_07_13_interpol <- as.data.frame(LinearInterpol(PRMD_07_13))
PRMD_07_13_hourly <- HourlyData(PRMD_07_13_interpol)

#SRHT
sum(is.na(SRHT_07_13$TempAire)) #La estaci??n est?? completa
SRHT_07_13_interpol <- as.data.frame(LinearInterpol(SRHT_07_13))
SRHT_07_13_hourly <- HourlyData(SRHT_07_13_interpol)

#TPZT
sum(is.na(TPZT_07_13$TempAire))
sum(is.na(TPZT_07_13$Precipitacion)) #La estaci??n est?? incompleta

##Faltan 1473 registros de temperatura y 1472 de precipitacion. 
TPZT_07_13_interpol <- as.data.frame(LinearInterpol(TPZT_07_13))

##Ahora corremos la funci??n HourlyData para juntar las 6 observaciones de cada hora en una sola.
TPZT_07_13_hourly <- as.data.frame(HourlyData(TPZT_07_13_interpol))

#TRMS
sum(is.na(TRMS_07_13$TempAire)) #La estaci??n est?? completa
TRMS_07_13_interpol <- as.data.frame(LinearInterpol(TRMS_07_13))
TRMS_07_13_hourly <- HourlyData(TRMS_07_13_interpol)

#TZNT
sum(is.na(TZNT_07_13$TempAire))
sum(is.na(TZNT_07_13$Precipitacion)) #La estaci??n est?? completa
TZNT_07_13_interpol <- as.data.frame(LinearInterpol(TZNT_07_13))
TZNT_07_13_hourly <- HourlyData(TZNT_07_13_interpol)

#UTTE
sum(is.na(UTTE_07_13$TempAire))
sum(is.na(UTTE_07_13$Precipitacion)) #La estaci??n est?? incompleta

##Faltan 6 registros de cada variable. No hay NAs al final.  
UTTE_07_13_interpol <- as.data.frame(LinearInterpol(UTTE_07_13))

##Ahora corremos la funci??n HourlyData para juntar las 6 observaciones de cada hora en una sola.
UTTE_07_13_hourly <- as.data.frame(HourlyData(UTTE_07_13_interpol))

#VLBR
sum(is.na(VLBR_07_13$TempAire))
sum(is.na(VLBR_07_13$Precipitacion)) #La estaci??n est?? incompleta

##Faltan 10 registros de cada variable. 5 en medio y 5 al final.
VLBR_07_13_interpol <- as.data.frame(LinearInterpol(VLBR_07_13))

##Los ??ltimos 4 registros (NA) los llenamos con el ??ltimo valor registrado. 

VLBR_07_13_interpol$TempAire[is.na(VLBR_07_13_interpol$TempAire)] <- 19.8

VLBR_07_13_interpol$Precipitacion[is.na(VLBR_07_13_interpol$Precipitacion)] <- 0

##Ahora corremos la funci??n HourlyData para juntar las 6 observaciones de cada hora en una sola.

VLBR_07_13_hourly <- as.data.frame(HourlyData(VLBR_07_13_interpol))

#JUNTAMOS LOS OBJETOS "_hourly" EN UNA TABLA

observados <- as.data.frame(rbind(ALTZ_07_13_hourly, ATLC_07_13_hourly, CRCA_07_13_hourly, ECOG_07_13_hourly,
                                  ENCI_07_13_hourly, HCHP_07_13_hourly, HMNT_07_13_hourly, IGUA_07_13_hourly,
                                  IMTA_07_13_hourly, IZMT_07_13_hourly, LCHI_07_13_hourly, LGZM_07_13_hourly,
                                  LMLI_07_13_hourly, MLII_07_13_hourly, NVTO_07_13_hourly, PIZP_07_13_hourly,
                                  PRMD_07_13_hourly, SRHT_07_13_hourly, TPZT_07_13_hourly, TRMS_07_13_hourly,
                                  TZNT_07_13_hourly, UTTE_07_13_hourly, VLBR_07_13_hourly))

#Generamos una columna en "observados" para las estaciones
Estaciones <- c("ALTZ", "ATLC", "CRCA", "ECOG", 
                "ENCI", "HCHP", "HMNT", "IGUA", 
                "IMTA", "IZMT", "LCHI", "LGZM", 
                "LMLI", "MLII", "NVTO", "PIZP", 
                "PRMD", "SRHT", "TPZT", "TRMS", 
                "TZNT", "UTTE", "VLBR")

ColEstaciones <- rep(Estaciones, each=length(ALTZ_07_13_hourly[,1]))

observados$Estaciones <- ColEstaciones

colnames(observados) <- c("PrecOBS", "TempOBS", "Estacion")


###El output.csv no estaba acomodado alfabetimcamnete como nuestros datos observados, así que 
#reacomodamos las columnas para ordenarlo desde ALTZ hasta VLBR.

wrfout_D03_2013_07 <- wrfout_D03_2013_07[c(1,2,21,22,23,24,25,26,27,28,29,6,7,8,3,4,5,15,16,17,66,67,68,
                                     12,13,14,42,43,44,57,58,59,18,19,20,45,46,47,60,61,62,69,
                                     70,71,30,31,32,33,34,35,36,37,38,48,49,50,51,52,53,54,55,
                                     56,9,10,11,63,64,65,39,40,41)]

#Acomodamos el los datos modelados
wrf <- as.data.frame (wrfout_D03_2013_07[3:71])

wrf_temp <- stack(wrf, 
                   select=c(V21, V24, V27, V6, V3, V15, V66, V12, V42, V57, V18, V45, V60, V69, V30, V33, V36, V48, V51, V54, V9, V63, V39))
colnames(wrf_temp) <- c("Temp_WRF", "Estacion")

wrf_p1 <- stack(wrf,
                 select = c(V22, V25, V28, V7, V4, V16, V67, V13, V43, V58, V19, V46, V61, V70, V31, V34, V37, V49, V52, V55, V10, V64, V40))
colnames(wrf_p1) <- c("P1_WRF", "Estacion")

wrf_p2 <- stack(wrf,
                 select = c(V23, V26, V29, V8, V5, V17, V68, V14, V44, V59, V20, V47, V62, V71, V32, V35, V38, V50, V53, V56, V11, V65, V41))
colnames(wrf_p2) <- c("P2_WRF", "Estacion")

wrf_out <- cbind.data.frame(wrf_temp[,1], wrf_p1[,1], wrf_p2[,1])

colnames(wrf_out) <- c("TempWRF", "P1WRF", "P2WRF")

wrf_out


###convertimos la columna de temperatura a grados centigrados
wrf_out$TempWRF <- wrf_out$TempWRF - 273

###creamos una columna con la precipitación desagregada de wrf
####n-(n+1) ###La precipitacion de wrf es acumulada, entonces hay que hacer una nueva columna en donde no este acumulada. 

Obs_WRF$diff <- ave(Obs_WRF$P2WRF, FUN=function(x) c(0, diff(x)))

sum(Obs_WRF$diff < 0)  #los valores del 1ro de julio saldrán negativos porque no hay comparacion, entonces los ajustamos a cero. 

Obs_WRF[,c(11)][Obs_WRF[, c(11)] < 0] <- 0

#Merge nuestros dos dataframes de observados y predicciones
Obs_WRF <- cbind(observados, wrf_out)

##Creamos una columna con la fecha y la hora.
start1 <- as.POSIXct("2013-07-01")
interval1 <- 60
end1 <- start + as.difftime(31, units="days")
x1 <- seq(from=start1, by=interval*60, to=end1)
FechaHora1 <- as.factor(x1[1:744])

#Creamos un objeto que guarde el numero de estaciones
numeroestaciones1 <- 23

#Vector que ser?? la columna que integrar?? la fecha y la hora.
Tiempo1 <- rep(FechaHora1, times = numeroestaciones1)

#Nombramos nuestra columna como "Fecha"
Obs_WRF$Fecha <- Tiempo1

##reordenamos las columnas
Obs_WRF<- Obs_WRF[c(3,7,2,1,4,5,6)]

#PRIMERAS GRAFICAS
##precipitacion
###observada

agregacion_obs <- function(x){
  tmp <- aggregate(x[,4] ~ x[,9]+x[,1], FUN = sum)
}

prec_agregacion_obs <- agregacion_obs (Obs_WRF)
colnames(prec_agregacion_obs) <- c("Dia", "Estacion", "PrecObs")

promedio_obs <- function(x){
  tmp <- aggregate(x[,3] ~ x[,1], FUN = mean)
}

prec_promedio_obs <- promedio_obs(prec_agregacion)
colnames(prec_promedio_obs) <- c("Dia", "Promedio")

graf_prec_obs <- ggplot(prec_promedio_obs, aes(Dia, Promedio)) + 
  geom_point()

###wrf
#corremos para precipitacion wrf de nuevo
agregacion_wrf <- function(x){
  tmp <- aggregate(x[,11] ~ x[,9]+x[,1], FUN = sum)
}

prec_agregacion_wrf <- agregacion_wrf (Obs_WRF)
colnames(prec_agregacion_wrf) <- c("Dia", "Estacion", "PrecWRF")


promedio_wrf <- function(x){
  tmp <- aggregate(x[,3] ~ x[,1], FUN = mean)
}

prec_promedio_wrf <- promedio_wrf(prec_agregacion_wrf)
colnames(prec_promedio_wrf) <- c("Dia", "Promedio")

graf_prec_wrf <- ggplot(prec_promedio_wrf, aes(Dia, Promedio)) + 
  geom_point()


##temperatura
###observada
agregacion_obs <- function(x){
  tmp <- aggregate (x[,3] ~ x[,9]+x[,1], FUN = mean)
}

temp_agregacion_obs <- agregacion_obs(Obs_WRF)
colnames(temp_agregacion) <- c("Dia", "Estacion", "Temperatura")

promedio_obs <- function(x){
  tmp <- aggregate(x[,3] ~ x[,1], FUN = mean)
}

temp_promedio_obs <- promedio_obs(temp_agregacion)
colnames(temp_promedio_obs) <- c("Dia", "Promedio")

graf_temp_obs <- ggplot(temp_promedio_obs, aes(Dia, Promedio)) + 
  geom_point()

###wrf
agregacion_wrf <- function(x){
  tmp <- aggregate (x[,5] ~ x[,9]+x[,1], FUN = mean)
}

temp_agregacion_wrf <- agregacion_wrf(Obs_WRF)
colnames(temp_agregacion_wrf) <- c("Dia", "Estacion", "Temperatura")

promedio_wrf <- function(x){
  tmp <- aggregate(x[,3] ~ x[,1], FUN = mean)
}

temp_promedio_wrf <- promedio_wrf(temp_agregacion_wrf)
colnames(temp_promedio_wrf) <- c("Dia", "Promedio")

graf_temp_wrf <- ggplot(temp_promedio_wrf, aes(Dia, Promedio)) + 
  geom_point()








