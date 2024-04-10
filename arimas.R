library(forecast)
library(ggplot2)
options(scipen=999)

# Datos -------------------------------------------------------------------


df <- read.csv("C:/Users/Mario/Desktop/paro.csv")
df <- df[1:451,]
datos <- ts(df[,2], start = c(1986,4), frequency = 12)
autoplot(datos,
         xlab="",
         ylab= "%",
         main="Tasa de paro")+ theme_bw()
 
start(datos)
end(datos)
frequency(datos) 'mensual'

head(time(datos), n = 48)  #Mostramos sólo los 4 primeros años

# Manipulacion de la serie ------------------------------------------------



summary(ets(datos, damped = FALSE)) # esto es AAN (A, A, N): Alisado de Holt


autoplot(ets(datos, damped = FALSE))
## Extracción de la tendencia "comportamiento a largo plazo
datos_agregados <- aggregate(datos, FUN = sum)
autoplot(datos_agregados,
         xlab = "",
         ylab = "",
         main = "")


## Extracción de la estacionalidad
ggsubseriesplot(datos) +
  ylab("") +
  xlab("") +
  ggtitle("") 



mean(a[1:12])
ComponenteEstacional <- tapply(datos / mean(datos),cycle(datos),FUN = mean)
round(ComponenteEstacional,4)
"
Febrero, marzo, agosto, septiembre y octubre son los meses en los que el paro es inferior a la media anual.
Enero, abril, mayo, junio, julio, noviembre y diciembre estan por encima.
"


# Tipo de esquema

CasosY = aggregate(datos, FUN = sum)
DesviacionY = aggregate(datos, FUN = sd)

ggplot() +
  geom_point(aes(x = CasosY, y = DesviacionY), size = 2) +
  xlab("Paro anual") + 
  ylab("Des. Tip. intra-anual") # Esquema Aditivo


# Decomposición de la serie -----------------------------------------------

ParoDesAdi <- decompose(datos, 
                       type = "addi")
autoplot(ParoDesAdi,
         xlab = "",
         main = "")

ParoDesAdi$figure

autoplot(datos, 
         series="Tasa de paro",
         xlab = "",
         ylab = "%",
         main = "") +
  autolayer(trendcycle(ParoDesAdi), 
            series="Tendencia") +
  scale_colour_manual(values=c("Paro"="black","Tendencia"="red"),
                      breaks=c("Paro","Tendencia"))


# Métodos sencillos -------------------------------------------------------

(mediaParo <- meanf(datos, h = 8))
(naiveParo <- naive(datos, h = 8))
(derivaParo <- rwf(datos,  h = 8, drift = TRUE))

autoplot(datos, 
         series = "Paro",
         xlab = "",
         ylab = "% Paro",
         main = "") +
  autolayer(mediaParo, series="Media", PI = FALSE) +
  autolayer(naiveParo, series="Ingenuo", PI = FALSE) +
  autolayer(derivaParo, series="Deriva", PI = FALSE) +
  scale_colour_discrete(limits=c("Paro", "Media", "Ingenuo", "Deriva")) +
  labs(colour="Métodos") + 
  theme(legend.position=c(0.1,0.8))


accuracy(mediaParo)
accuracy(naiveParo)
accuracy(derivaParo)



# Definimos las observaciones intra- y extramuestrales
datosIntra <- subset(datos, end = length(datos) - 12)
datosExtra <- subset(datos, start = length(datos) - 11)

# Estimamos el modelo con todos los datos menos los 7 ultimos y
# predecimos los 7 años que hemos quitado de la serie 
datosExtraPre <- rwf(datosIntra,  h = 12, drift = TRUE)

# Vemos la calidad del ajuste. Primero la predicción y luego los datos reales
accuracy(datosExtraPre, datosExtra)  "Error de 4.3% en el Training y del 11.10 en el Test"



# Medias móviles ----------------------------------------------------------

mmf <- function(x, r = 3, h = 5) {
  z <- NULL
  z$x <- x
  z$orden = r
  
  TT <- length(x)
  inicio <- start(x)
  frecuencia <-frequency(x)
  
  z$mm <- stats::filter(x, 
                        rep(1/r, r), 
                        side = 1)
  
  z$fitted <- ts(c(NA, z$mm[-TT]), 
                 start = inicio, 
                 freq = frecuencia)
  
  z$mean <- ts(rep(z$mm[TT], h), 
               start = time(x)[TT] + 1/frecuencia, 
               freq = frecuencia)
  
  z$residuals <- x - z$fitted
  
  class(z) <- "forecast"
  z
}


mmdatos <- mmf(datos, r = 1, h = 5)
autoplot(mmdatos,
         xlab = "",
         ylab = "%",
         main = "") +
  autolayer(mmdatos$fitted) + 
  theme(legend.position="none")
accuracy(mmdatos) # MAPE DE 9.64% o lo que es lo mismo 1.8 puntos porcentuales se equivoca.
#Bastante bueno el modelo



#Selección usando previsiones intramuestrales a un periodo vista
for(r in 1:4) {
  error <- accuracy(mmf(datos, r = r))[5]
  cat("\nPara un orden de", 
      r, 
      "el error es", 
      formatC(error, format = "f", digits = 2),
      "%")
}# Lo mejor orden 1 

#Selección usando origen de predicción móvil
k <- 20              
h <- 8   #horizonte de predicción             
TT <- length(datos) 
s <- TT - k - h      

for(r in 1:4){
  
  mapemm <- matrix(NA, s + 1, h)
  for (i in 0:s) {
    
    train.set <- subset(datos, start = i + 1, end = i + k)
    test.set <-  subset(datos, start = i + k + 1, end = i + k + h)
    
    mmdatos<- mmf(train.set, r = r, h = h)
    mapemm[i + 1, ] <- 100*abs(test.set - mmdatos$mean)/test.set
  }
  mapemm <- colMeans(mapemm)
  
  cat("\nPara un orden de", 
      r, 
      "los errores son", 
      formatC(mapemm, format = "f", digits = 2)) 
  
} 
# Lo mejor orden r= 1  (Ingenuo I) es el más adecuado. 
# En todo caso, en este ejemplo la calidad de las previsiones es relativamente independiente del orden de la media móvil.




# ARIMA -------------------------------------------------------------------
"Decir que tiene que ser estacionario, ergodico"


autoplot(datos, xlab = "", ylab = "", main = "")
autoplot(diff(datos), xlab = "", ylab = "", main = "")
ggAcf(datos, xlab = "", ylab = "", main = "")
ggAcf(diff(datos), xlab = "", ylab = "", main = "")

ndiffs(datos) #"una diferencia "
"Podemos decir que la primera diferencia de la serie del paro es estacionaria y ergodica"

auto.arima(datos, d= 1, trace = TRUE)

arima <-Arima(datos,
          order = c(1,1,1),
          seasonal = c(0,0,1),
          include.constant = FALSE)

library(gt)
ac <- accuracy(arima)
gt(ac)

