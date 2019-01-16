#######################################
########  REGRESIONES CON UN SHP
#######################################

library(tidyverse)
library(spdep)
library(rgdal)

ruta <- ""   ##### ESTABLECER RUTA AL DIRECTORIO DE TRABAJO
setwd(ruta)

### EJECUTO EL SCRIPT 01 PARA CREAR EL SHP FUSIONADO CON LOS DATOS
source("01 import and merge.R")


options(scipen = 10)  # PARA QUE IMPRIMA MÁS DECIMALES EN VEZ DE LA NOTACIÓN CIENTIFICA

###############
### MODELOS ###
###############

## Hipotesis

H0.paro <- VOX ~ renta_disponible_media16 + tipo + alquiler + 
  masde64 + VOX15 + derecha + izquierda + paro.muni + paro.muni.diff06.18 +
  subsidiados + Africana

H0.paro.hom <- VOX ~ renta_disponible_media16 + tipo + alquiler + 
  masde64 + VOX15 + derecha + izquierda + paro.hom + paro.hom.diff06.18

H1.paro.izq <- VOX ~ renta_disponible_media16 + tipo + alquiler + 
  masde64 + VOX15 + derecha + izquierda + 
  paro.hom + paro.hom.diff06.18 + paro.hom.diff06.18:derecha

H2.inmigracion <- VOX ~ renta_disponible_media16 + tipo + alquiler +  Africana +
  masde64 + VOX15 + derecha + izquierda + paro.hom + paro.hom.diff06.18 + 
  tipo:paro.hom.diff06.18 + tipo:Africana

model0 <- lm(H0.paro, data = p@data)
model0.hom <- lm(H0.paro.hom, data = p@data)
model1 <- lm(H1.paro.izq, data = p@data)
model2 <- lm(H2.inmigracion, data = p@data, na.action = "na.exclude")


library(jtools)

# Crea una tabla con los resultados de varios modelos
export_summs(model0, model0.hom, model1, model2, statistics = NULL, robust = TRUE, scale = T)

# Visualiza las interacciones 
interact_plot(model2, pred = paro.hom.diff06.18, modx = tipo, data = p@data, 
              interval = T, robust = "HC1")

interact_plot(model2, pred = Africana, modx = tipo, data = p@data, 
              interval = T, robust = "HC1")


#################################
### VISUALIZAR LOS RESIDUALES ###
#################################

# Aquí tengo los residuales
residuals(model2)
length(residuals(model2))

# Creo una columna en el shapefile con los valores residuales
p@data$residuales <- NA
p@data$residuales <- as.numeric(residuals(model2))

sd(p@data$residuales, na.rm = T)  # Usaremos la desviación típica como punto de corte


### Exportamos el shapefile para visualizarlo en QGIS
#writeOGR(p, "DATOS/TALLERES/R Y QGIS/shapefiles/condatos/",
#         driver = "ESRI Shapefile", layer = "andalucia2018", overwrite_layer = T)





######################################################################################################
# BOLA EXTRA: ¿Si Vox saca mejores resultados en las ciudades porqué el coeficiente para la variable 
# tipo de municipio da negativo para las ciudades y positivo para las zonas rurales?
#######################################################################################################
mean(p@data$VOX[p@data$tipo == "Ciudades"])
mean(p@data$VOX[p@data$tipo == "Zona de densidad intermedia"])
mean(p@data$VOX[p@data$tipo == "Zona rural"])

# Aquí una regresión solo con el tipo de municipio como variable predictora
m1 <- lm(VOX ~ tipo, data = p@data)
summary(m1)

# Aquí controlando por otras dos más y con una interacción ya cambia el signo
m2 <- lm(VOX ~ tipo + izquierda + paro.hom.diff06.18 + tipo:paro.hom.diff06.18, data = p@data)
summary(m2)
