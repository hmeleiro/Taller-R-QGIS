############################
########  SPATIAL ANALYSIS
############################

library(tidyverse)
library(spdep)
library(rgdal)

ruta <- ""   ##### ESTABLECER RUTA AL DIRECTORIO DE TRABAJO
setwd(ruta)


############################################################################## 
### EJECUTO EL SCRIPT 01 Y EL 02 PARA CREAR EL SHP FUSIONADO CON LOS DATOS
##############################################################################

source("01 import and merge.R")
source("03 regressions.R")


####################################
# HIPOTESIS SIN RETARDO ESPACIAL
####################################

# Creo un objeto con la fórmula de la regresión y la calculo
H1 <- VOX ~ tipo + Africana + derecha

# Creo un objeto con la regresión y visualizo los resultados
model <- lm(H1, p@data)
summary(model)


#######################################
### HIPÓTESIS CON RETARDO ESPACIAL
#######################################

# Modelo espacial
# Creamos la matriz de pesos espaciales
w <- poly2nb(pl = p, queen = T) 
ww <- nb2listw(neighbours = w, style = "W")   

# Aplicamos el test de Moran a los residuales de una regresión
lm.morantest(model, ww)  


## Creo las variables de retardo espacial
p@data$lag_Africana <- NA  # Creo la variable de retardo espacial
p@data$lag_Africana <- lag.listw(ww, p$Africana)
p@data$lag_derecha <- NA  # Creo la variable de retardo espacial
p@data$lag_derecha <- lag.listw(ww, p$derecha)

# Creo un objeto con la fórmula de la regresión
H1.spatial <- VOX ~ tipo + Africana + lag_Africana + derecha + lag_derecha

# Creo un objeto con la regresión y visualizo los resultados
spmodel2 <- lm(H1.spatial, p@data)
summary(spmodel2)


### Si queremos los impactos directos e indirectos podemos usar directamente la funcion lmSLX() de la librería spdep
H1 <- VOX ~ tipo + Africana + derecha
spmodel <- lmSLX(H1, data = p@data, listw = ww)
summary(spmodel)

impacts(spmodel)

#####################


##########################################
### MODELO ESPACIAL AVANZADO
##########################################

# Si os da un error diciendo "Variable contains non-finite values" tiene que ver con que la variable tiene NAs
# Para solucionarlo o bien se imputa la media de la variable a los casos perdidos, o se emplean los argumentos zero.policy y NAOK p.e: lag.listw(ww, p$izquierda, zero.policy = T, NAOK = T)

p@data$lag_VOX15 <- NA  # Creo la variable de retardo espacial
p@data$lag_VOX15 <- lag.listw(ww, p$VOX15, zero.policy = T, NAOK = T)
p@data$lag_izquierda <- NA  # Creo la variable de retardo espacial
p@data$lag_izquierda <- lag.listw(ww, p$izquierda)
p@data$lag_derecha <- NA  # Creo la variable de retardo espacial
p@data$lag_derecha <- lag.listw(ww, p$derecha)
p@data$lag_alquiler <- NA  # Creo la variable de retardo espacial
p@data$lag_alquiler <- lag.listw(ww, p$alquiler)
p@data$lag_masde64 <- NA  # Creo la variable de retardo espacial
p@data$lag_masde64 <- lag.listw(ww, p$masde64)


H1.spatial.complex <- VOX ~ Africana + lag_Africana + renta_disponible_media16 + tipo + alquiler + 
  lag_alquiler + masde64 + lag_masde64 + VOX15 +lag_VOX15 + derecha + lag_derecha + izquierda + 
  paro.hom + paro.hom.diff06.18 + 
  tipo:paro.hom.diff06.18 + tipo:Africana


spmodel2 <- lm(H1.spatial.complex, p@data)
summary(spmodel2)

plot_coefs(spmodel2)


interact_plot(model = spmodel2, pred = Africana, modx = tipo, 
              data = p@data, interval = T, robust = "HC1",
              int.width = 0.95) + labs(title = "La presencia de población africana está \nasociada de forma positiva al voto Vox\nsolo en las zonas rurales")

interact_plot(model = spmodel2, pred = paro.hom.diff06.18, modx = tipo, 
              data = p@data, interval = T, robust = "HC1", 
              int.width = 0.95) + labs(title = "El aumento del paro está más asociado \nal voto a Vox en las ciudades")

plot_coefs(spmodel2, robust = T) + labs(title = "Modelo espacial")

