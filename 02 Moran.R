#########################
##### I DE MORAN
#########################

library(tidyverse)
library(spdep)
library(rgdal)

ruta <- ""  ##### ESTABLECER RUTA AL DIRECTORIO DE TRABAJO
setwd(ruta)

### EJECUTO EL SCRIPT 01 PARA CREAR EL SHP FUSIONADO CON LOS DATOS
source("01 import and merge.R")


###############################################
#### ESTABLECEMOS LOS VECINOS DE CADA ZONA ####
###############################################
# Creo la matriz de pesos espaciales
# Estas funciones son de la librería spdep
w <- poly2nb(pl = p, queen = T) # Matriz de contiguidad binaria (1 si es vecino, 0 si no lo es)
# Crea, en base a una matriz de contiguidad, una matriz de pesos espaciales. 
# style = "W" sirve para estandarizar los pesos de las filas (esto es importante para calcular la I de Moran)
ww <- nb2listw(neighbours = w, style = "W", zero.policy = T)  

# Visualicemos los vecinos
#png("imagenes/contiguidadR.png", width = 1600*3, height = 900*3, res = 300)
plot(p)
plot.nb(x = w, coords = coordinates(p), points = F, col = "red", arrows = F, add = T)
#dev.off()

# Conviene no dejar valores perdidos en las variables de interés. 
# En este caso hay 26 secciones censales donde no tenemos valores de % de voto a Vox, así que les imputaremos la media.
# Pero para ser más precisos, imputaremos la media de la provincia en vez de la media de toda Andalucía
#### Hay que hacer esta imputación para cada variable que tenga valores NA.
#### Si no se hace, al calcular el retardo dará un error.
### Otra opción sería dejar los NAs y especificar los argumentos na.action y zero.policy que sirven lidiar con NAs y con zonas sin vecinos
###############################################################
##################################################################
length(p@data$VOX[is.na(p@data$VOX)])

p@data$cod.prov <- substr(p@data$COD_MUN.x, 1, 2)
p@data$VOX[is.na(p@data$VOX) & p@data$cod.prov == "04"] <- mean(p@data$VOX[p@data$cod.prov == "04"], na.rm = T) #Almeria
p@data$VOX[is.na(p@data$VOX) & p@data$cod.prov == "11"] <- mean(p@data$VOX[p@data$cod.prov == "11"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$cod.prov == "14"] <- mean(p@data$VOX[p@data$cod.prov == "14"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$cod.prov == "18"] <- mean(p@data$VOX[p@data$cod.prov == "18"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$cod.prov == "21"] <- mean(p@data$VOX[p@data$cod.prov == "21"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$cod.prov == "23"] <- mean(p@data$VOX[p@data$cod.prov == "23"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$cod.prov == "29"] <- mean(p@data$VOX[p@data$cod.prov == "29"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$cod.prov == "41"] <- mean(p@data$VOX[p@data$cod.prov == "41"], na.rm = T)



# Calculamos la I de Moran global
moran.mc(p@data$VOX, ww, nsim = 599, 
           na.action = na.exclude, zero.policy = T) # Esto es lo que habría que poner en las funciones para calcular retardo espacil e I de Moran

# Nos sale una I de Moran bastante elevada (0.673) y significativa al 99%.

# La visualizamos en un gráfico de dispersión 

png("imagenes/moranplotVOX.png", width = 1600*3, height = 900*3, res = 300)
moran.plot(x = p@data$VOX, 
           listw = ww, 
           xlab = "% de voto a VOX", 
           ylab = "Retardo espacial del % de voto a Vox", 
           labels = p@data$MUNICIPIO.x, 
           pch = 19, cex = 0.13)
dev.off()




##################################################################
##### MAPA LISA (Local Indicator of Spatial Autocorrelation) #####
##################################################################


# Importo una función que calcula el retardo espacial y su significativad, 
# lo estandariza, y asigna los cuadrantes a cada observación
source("visualmoran.R") 

# Uso la función para crear conseguir el retardo espacial y los cuadrantes del % de voto a Vox
p <- visualmoran(p, ww, "VOX", NA.impute = F)


### EXPORTAR EL SHP

#writeOGR(p, "shapefiles/condatos/",
#         driver = "ESRI Shapefile", layer = "andalucia20182", overwrite_layer = T)

