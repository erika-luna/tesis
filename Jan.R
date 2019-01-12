###Eliminar columnas y conservar solo temp y precipitación

head(ALTZ_07_13)
ALTZ_07_13[4] <- list(NULL)

ECOG_07_13[2:5] <- list(NULL)
ECOG_07_13[3:4] <- list(NULL)
ECOG_07_13[4] <- list(NULL)

ATLC_07_13

myvars <- c("fecha", "TempAire", "Precipitacion")
newdata <- ATLC_07_13[myvars]

###Weather Stations Analysis Script
##Load libraries
library(data.table)
library(chron)
library(zoo)


###Functions

##obtener los porcentajes y conteo the valores faltantes para cada estaciÛn meteorolÛgica 
Missing <- function(x) {
  percentage <- colMeans(is.na(x))*100
  suma <- colSums(is.na(x))
  return (list(percentage=percentage, suma=suma))
  }
  

miss_ALTZ <- Missing(ALTZ_07_13)
miss_ENII <- Missing(ENII_07_13)

LinearInterpol<- function (x) {
  Precipitacion<-na.approx(x[,3],  na.rm = TRUE) 
  Temperatura<-na.approx(x[,4],  na.rm = TRUE) 
  Weather<-cbind(Precipitacion,Temperatura)
  return(Weather)
}


NewLinearInter <- function(x) {
  Precipitacion <- na.approx(x[,3], rule = 2)
  Temperatura <- na.approx(x[,4], rule = 2)
  Weather <- cbind(Precipitacion, Temperatura)
  return(Weather)
}

prueba1 <- NewLinearInter(ALTZ_07_13)

# create example dataset
df = data.frame(value = mtcars$qsec,
                time = 1:nrow(mtcars))

# replace some values with NA (you can experiment with different values)
Atlaco$TempAire[c(16.5)] = NA

# build linear model based on existing data (model ignores rows with NAs)
m = lm(TempAire ~ X, data = Atlaco)

# add predictions as a column
df$pred_value = predict(m, newdata = df)

# replace (only) NAs with predictions
df$interp_value = ifelse(is.na(df$value), df$pred_value, df$value)

# plot existing and interpolated data
ggplot()+
  geom_point(data=df, aes(time, value), size=5)+
  geom_point(data=df, aes(time, interp_value), col="red")

prueba2 <- LinearInterpol(Atlaco)

# create example dataset
dfENII = data.frame(time = 1:nrow(ENII_07_13),
                    fecha = ENII_07_13$fecha,
                    prec = ENII_07_13$Precipitacion,
                    temp = ENII_07_13$TempAire)

prueba3 <- LinearInterpol(dfENII)

prueba4 <- LinearInterpol(ENII_07_13)

prueba2 <- NewLinearInter(dfENII)

INTERPOLATION <- function(x) {
  pp <- na.approx(x[,3], na.rm = TRUE)
}

is.na(prueba2)



# NOT RUN {

sample.xts <- xts(ALTZ_07_13, descr='my new xts object')

ALTZ_07_13[2:5] <- list(NULL)

AIRE <- subset(ALTZ_07_13, select = fecha:TempAire)

approx <- na.approx(AIRE[,2], na.rm = FALSE)


AIRE2 <- subset(ENII_07_13, select = fecha:TempAire)

AIRE2[2:5] <- list(NULL)

approx2 <- na.approx(AIRE2[,2], na.rm = FALSE)

library(lubridate)
Date <- ALTZ_07_13$fecha
temperature <- ALTZ_07_13$TempAire

New_temp <- approx(temperature, Date)


lala <- New_temp$x

lala <- as.data.frame(lala)


library(readxl)
ALTZ_07_13 <- read_excel("/Volumes/Seagate/Tesis/obs_julio/ALTZ_07_13.csv",
                         header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)
View(ALTZ_07_13)

library(forecast)
x <- zoo(ALTZ_07_13$TempAire,df$fecha)
x <- as.ts(x)
x <- na.interp(x)
print(x)

pp1 <- INTERPOLATION(ALTZ_07_13)

IND <- function(t) {
  x <- dim(length(t))
  x[which(!is.na(t))] = 1
  x[which(is.na(t))] = 0
  return(x)
}

ALTZ_07_13$I <- IND(ALTZ_07_13$TempAire)

prueba <- INTERPOLATION(ALTZ_07_13)

ALTZ_linearinterpol <- LinearInterpol(ALTZ_07_13)
ATLC_linearinterpol <- LinearInterpol(ATLC_07_13)
CRCA_linearinterpol <- LinearInterpol(CRCA_07_13)
ECOG_linearinterpol <- LinearInterpol(ECOG_07_13)


ColFecha <- ALTZ_07_13$fecha
ALTZ_linerainterpol$fecha <- ColFecha

##Get hourly precipitation and temperature
HourlyData<-function(Data){
  HourSumPrec <- rollapply(Data[,1],6,(sum),by=6,by.column=TRUE,align='right')
  #length(HourSumPrec)##8760 correct
  HourTemAve <- rollapply(Data[,2],6,(mean),by=6,by.column=TRUE,align='right')
  #length(HourTemAve)##8760 correct
  WeatherHour<-cbind(HourSumPrec,HourTemAve)
  return(WeatherHour)
}




ALTZ_Hourly <- HourlyData(ALTZ_linearinterpol)
ATLC_Hourly <- HourlyData(ATLC_linearinterpol)
CRCA_Hourly <- HourlyData(CRCA_linearinterpol)
ECOG_Hourly <- HourlyData(ECOG_linearinterpol)



#Generamos una tabla como dataframe con todas las estaciones interpoladas. 
Estaciones <- as.data.frame(rbind(ALTZ_Hourly, ATLC_Hourly, CRCA_Hourly, ECOG_Hourly))


plot1 <- plot(LCHI_07_13$HumRelativa, LCHI_07_13$Precipitacion)


#INTENTO 200385859


library(ggplot2)

# create example dataset
df = data.frame(value = ALTZ_07_13$TempAire,
                time = 1:nrow(ALTZ_07_13))

# build linear model based on existing data (model ignores rows with NAs)
m = lm(value ~ time, data = df)

# add predictions as a column
df$pred_value = predict(m, newdata = df)

# replace (only) NAs with predictions
df$interp_value = ifelse(is.na(df$value), df$pred_value, df$value)

# plot existing and interpolated data
ggplot()+
  geom_point(data=df, aes(time, value), size=5)+
  geom_point(data=df, aes(time, interp_value), col="red")


# create example dataset
dfp = data.frame(value = ALTZ_07_13$Precipitacion,
                time = 1:nrow(ALTZ_07_13))

# build linear model based on existing data (model ignores rows with NAs)
m = lm(value ~ time, data = dfp)

# add predictions as a column
dfp$pred_value = predict(m, newdata = dfp)

# replace (only) NAs with predictions
dfp$interp_value = ifelse(is.na(dfp$value), dfp$pred_value, dfp$value)

# plot existing and interpolated data
ggplot()+
  geom_point(data=dfp, aes(time, value), size=1)+
  geom_point(data=dfp, aes(time, interp_value), col="red")

na.approx(df$value)

hourly_df <- HourlyData(df)


library(readxl)
VLBR_07_13 <- read_excel("/Volumes/Seagate/Tesis/obs_julio/VLBR_07_13.xlsx")
View(VLBR_07_13)

#JAN9

tmp <- wrfout_D03_2013_07[-which(wrfout_D03_2013_07$V1 == "2013-05-31"),]
tmp

Estaciones_WRF1 <- as.data.frame (wrfout_D03_2013_07[3:71])
Estaciones_WRF1

WRF_temp1 <- stack(Estaciones_WRF1, 
                  select=c(V3, V6, V9, V12, V15, V18, V21, V24, V27, V30, V33, V36, V39, V42, V45, V48, V51, V54, V57, V60, V63, V66, V69))
colnames(WRF_temp1) <- c("Temp_WRF", "Estaciones")

WRF_P11 <- stack(Estaciones_WRF1,
                select = c(V4, V7, V10, V13, V16, V19, V22, V25, V28, V31, V34, V37, V40, V43, V46, V49, V52, V55, V58, V61, V64, V67, V70))
colnames(WRF_P11) <- c("P1_WRF", "Estaciones")

WRF_P22 <- stack(Estaciones_WRF1,
                select = c(V5, V8, V11, V14, V17, V20, V23, V26, V29, V32, V35, V38, V41, V44, V47, V50, V53, V56, V59, V62, V65, V68, V71))
colnames(WRF_P22) <- c("P2_WRF", "Estaciones")

WRF_out <- cbind.data.frame(WRF_temp1[,1], WRF_P11[,1], WRF_P22[,1])

WRF_out

###JAN11
##Grafica

#Split the date into three different columns

datetxt <- c(Obs_WRF$Fecha)
datetxt <- as.Date(datetxt)
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))

###

splitfecha <-  as.character(Obs_WRF$Fecha) 

destring <- function(x){
  a <- strsplit(x, "-")[[1]]
  a[2]
}


destringdia <- function(x){
  a <- strsplit(x, " ")[[1]]
  b <- strsplit(a, "-")[[1]]
  b[3]
}

destringhora <- function(x){
  a <- strsplit(x, " ")[[1]]
  a[2]
}

#nuevo
destringhora3 <- function(x){
  a <- strsplit(x, ":")[[1]]
  a[3]
} 


Mes <- rep(0,17112)
Dia <- rep(0,17112)
Hora <- rep(0,17112)

for (i in 1:17112){
  Mes[i] <- destring(splitfecha[i])
  Dia[i] <- destringdia(splitfecha[i])
  Hora[i]<- destringhora(splitfecha[i])
} 

Obs_WRF$Mes <- Mes
Obs_WRF$Dia <- Dia
Obs_WRF$Hora <- Hora




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

agregacion_wrf <- function(x){
  tmp <- aggregate(x[,7] ~ x[,9]+x[,1], FUN = sum)
}

prec_agregacion_wrf <- agregacion_wrf (Obs_WRF)
colnames(prec_agregacion_wrf) <- c("Dia", "Estacion", "PrecObs")


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

###La precipitacion de wrf es acumulada, entonces hay que hacer una nueva columna en donde no este acumulada. 


###esto quien sabe qué es 

plot(res_temp2)

ggplot(res_temp2, aes(x=Mes, y=Promedio)) +
  geom_point(shape=1)

Temp <- as.data.frame(temp_promedio$Promedio)
colnames(Temp) <- c("Prom")

Variables <- c(prec_promedio, Temp)
temp_prec <- as.data.frame(Variables)

ggplot(temp_prec, aes(x=Dia, y=Promedio,  color=Prom)) +
  geom_point(shape=1)





ggplot(temp_prec, aes(x=Mes, y=Promedio, Promedio.1)) +
  geom_point(shape=1)



###n-(n+1)

Obs_WRF$ID <- seq.int(nrow(Obs_WRF))

Obs_WRF$diff <- ave(Obs_WRF$P2WRF, FUN=function(x) c(0, diff(x)))

sum(Obs_WRF$diff < 0)

Obs_WRF[,c(11)][Obs_WRF[, c(11)] < 0] <- 0


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

