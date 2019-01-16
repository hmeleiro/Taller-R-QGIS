########################################################################################
### ESTE CÓDIGO INSTALA LAS LIBRERÍAS REQUERIDAS EN EL CASO DE QUE NO ESTÉN INSTALADAS ###
########################################################################################
list.of.packages <- c("tidyverse", "spdep", "rgdal", "jtools", "sp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}



library(tidyverse)
library(spdep)
library(rgdal)

ruta <- ""   ##### ESTABLECER RUTA AL DIRECTORIO DE TRABAJO
setwd(ruta)


### IMPORTO LOS DATOS Y EL SHP

# Cargo el shapefile de las secciones de Andalucía
p <- readOGR("shapefiles/and_secc/da08_seccion_censal.shp")

# Cargo los datos
andalucia <- read_csv("data/andalucia.csv")
head(andalucia)


##### FUSIONO LOS DATOS CON EL SHP

# Observamos la estructura del dato

glimpse(p@data)
glimpse(andalucia[, 1:10])

# Hace falta una columna con valores únicos y que coincidan en el dataset y en el shp
# Parece que la primera columna de datos del shp es la concatenacion del codigo de la provincia, el del municipio, el del distrito y el de la seccion censal.
# Esos datos los tengo en el dataset de Andalucia en columnas separadas. Pues Creemos una columan con esos valores concatenados.

p$RT_CODIGO <- as.character(p$RT_CODIGO)
p$RT_CODIGO[300]
paste0(andalucia$COD_MUN, andalucia$COD_DIST, andalucia$COD_SEC)[300]


andalucia$RT_CODIGO <- paste0(andalucia$COD_MUN, andalucia$COD_DIST, andalucia$COD_SEC)  # Creo la columna en el dataset de Andalucia
andalucia$Codcir <- substring(andalucia$COD_MUN, 1, 2)  # Creo una columna id de la provincia

p <- sp::merge(p, andalucia, by = "RT_CODIGO")  # Hago merge entre los datos del shp y el dataset de Andalucía

### Ahora, una vez tenemos unido el shapefile y los datos podemos trabajar desde el shapefile para hacer los análisis
### Recordad que para acceder a los datos almacenados en un shapefile hay que añadir la terminación @data al shapefile.

glimpse(p) # Esto es el shapefile en sí, pero ahora en el apartado de datos del shp aparecen las variables del dataset de Andalucía
glimpse(p@data) # Esto son los datos almacenados en el shapefile

# Por lo tanto, para acceder a una variable de los datos almacenados en el shapefile hay que hacer lo siguiente
p@data$PSOE # % de voto al PSOE en las elecciones andaluzas 2018
summary(p@data$PSOE)

###############################


######################################
### FORMATEO UN POCO MÁS LOS DATOS ###
######################################

### Empezamos creando algunas variables que pueden ser de interés.
### Creo las variables voto a la izquierda y a la derecha
p@data$izquierda <- p@data$PSOE15 + p@data$AA15  
p@data$derecha <- p@data$PP15 + p@data$Cs15

# Creo la variable de caida de la participación de 2004 a 2015
p@data$caidaparti15 <- p@data$participacion15 - p@data$participacion04 
p@data$abstencion15 <- 100 - p@data$participacion15

# Calculo variciones entre 2004 y 2015 de algunas variables
p@data$PSOE0415diff <- NA
p@data$PSOE0415diff <- p@data$PSOE15 - p@data$PSOE04
p@data$PP0415diff <- NA
p@data$PP0415diff <- p@data$PP15 - p@data$PP04
p@data$euroeste <- p@data$NoEuropeos - p@data$Africana


##########################################
#### LIDIAMOS CON LOS CASOS PERDIDOS #####
##########################################
# Imputo medias a los valores perdidos
# Conviene no dejar valores perdidos en las variables de interés. 
# En el primer caso hay 26 secciones censales donde no tenemos valores de % de voto a Vox, así que les imputaremos la media.
# Pero para ser más precisos, imputaremos la media de la provincia en vez de la media de toda Andalucía
# Hay que hacer esta imputación para cada variable que tenga valores NA.
# Si no se hace, al calcular el retardo dará un error.
# Otra opción sería dejar los NAs y especificar los argumentos na.action y zero.policy que sirven lidiar con NAs y con zonas sin vecinos
##################################################################
length(p@data$VOX[is.na(p@data$VOX)])  # Cuento el número de secciones censales con valores NA
p@data$VOX[is.na(p@data$VOX) & p@data$Codcir == "04"] <- mean(p@data$VOX[p@data$Codcir == "04"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$Codcir == "11"] <- mean(p@data$VOX[p@data$Codcir == "11"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$Codcir == "14"] <- mean(p@data$VOX[p@data$Codcir == "14"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$Codcir == "18"] <- mean(p@data$VOX[p@data$Codcir == "18"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$Codcir == "21"] <- mean(p@data$VOX[p@data$Codcir == "21"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$Codcir == "23"] <- mean(p@data$VOX[p@data$Codcir == "23"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$Codcir == "29"] <- mean(p@data$VOX[p@data$Codcir == "29"], na.rm = T)
p@data$VOX[is.na(p@data$VOX) & p@data$Codcir == "41"] <- mean(p@data$VOX[p@data$Codcir == "41"], na.rm = T)
length(p@data$VOX[is.na(p@data$VOX)])  # Cuento el número de secciones censales con valores NA. Ahora debería ser 0


length(p@data$derecha[is.na(p@data$derecha)]) # Cuento el número de secciones censales con valores NA

p@data$derecha[is.na(p@data$derecha) & p@data$Codcir == "04"] <- mean(p@data$derecha[p@data$Codcir == "04"], na.rm = T)
p@data$derecha[is.na(p@data$derecha) & p@data$Codcir == "11"] <- mean(p@data$derecha[p@data$Codcir == "11"], na.rm = T)
p@data$derecha[is.na(p@data$derecha) & p@data$Codcir == "14"] <- mean(p@data$derecha[p@data$Codcir == "14"], na.rm = T)
p@data$derecha[is.na(p@data$derecha) & p@data$Codcir == "18"] <- mean(p@data$derecha[p@data$Codcir == "18"], na.rm = T)
p@data$derecha[is.na(p@data$derecha) & p@data$Codcir == "21"] <- mean(p@data$derecha[p@data$Codcir == "21"], na.rm = T)
p@data$derecha[is.na(p@data$derecha) & p@data$Codcir == "23"] <- mean(p@data$derecha[p@data$Codcir == "23"], na.rm = T)
p@data$derecha[is.na(p@data$derecha) & p@data$Codcir == "29"] <- mean(p@data$derecha[p@data$Codcir == "29"], na.rm = T)
p@data$derecha[is.na(p@data$derecha) & p@data$Codcir == "41"] <- mean(p@data$derecha[p@data$Codcir == "41"], na.rm = T)
length(p@data$derecha[is.na(p@data$derecha)]) # Cuento el número de secciones censales con valores NA


length(p@data$izquierda[is.na(p@data$izquierda)]) # Cuento el número de secciones censales con valores NA
p@data$izquierda[is.na(p@data$izquierda) & p@data$Codcir == "04"] <- mean(p@data$izquierda[p@data$Codcir == "04"], na.rm = T)
p@data$izquierda[is.na(p@data$izquierda) & p@data$Codcir == "11"] <- mean(p@data$izquierda[p@data$Codcir == "11"], na.rm = T)
p@data$izquierda[is.na(p@data$izquierda) & p@data$Codcir == "14"] <- mean(p@data$izquierda[p@data$Codcir == "14"], na.rm = T)
p@data$izquierda[is.na(p@data$izquierda) & p@data$Codcir == "18"] <- mean(p@data$izquierda[p@data$Codcir == "18"], na.rm = T)
p@data$izquierda[is.na(p@data$izquierda) & p@data$Codcir == "21"] <- mean(p@data$izquierda[p@data$Codcir == "21"], na.rm = T)
p@data$izquierda[is.na(p@data$izquierda) & p@data$Codcir == "23"] <- mean(p@data$izquierda[p@data$Codcir == "23"], na.rm = T)
p@data$izquierda[is.na(p@data$izquierda) & p@data$Codcir == "29"] <- mean(p@data$izquierda[p@data$Codcir == "29"], na.rm = T)
p@data$izquierda[is.na(p@data$izquierda) & p@data$Codcir == "41"] <- mean(p@data$izquierda[p@data$Codcir == "41"], na.rm = T)
length(p@data$izquierda[is.na(p@data$izquierda)]) # Cuento el número de secciones censales con valores NA


length(p@data$paro.hom[is.na(p@data$paro.hom)]) # Cuento el número de secciones censales con valores NA
p@data$paro.hom[is.na(p@data$paro.hom) & p@data$Codcir == "04"] <- mean(p@data$paro.hom[p@data$Codcir == "04"], na.rm = T)
p@data$paro.hom[is.na(p@data$paro.hom) & p@data$Codcir == "18"] <- mean(p@data$paro.hom[p@data$Codcir == "18"], na.rm = T)
p@data$paro.hom[is.na(p@data$paro.hom) & p@data$Codcir == "29"] <- mean(p@data$paro.hom[p@data$Codcir == "29"], na.rm = T)
length(p@data$paro.hom[is.na(p@data$paro.hom)]) # Cuento el número de secciones censales con valores NA

length(p@data$paro.hom.diff06.18[is.na(p@data$paro.hom.diff06.18)]) # Cuento el número de secciones censales con valores NA
p@data$paro.hom.diff06.18[is.na(p@data$paro.hom.diff06.18) & p@data$Codcir == "04"] <- mean(p@data$paro.hom.diff06.18[p@data$Codcir == "04"], na.rm = T)
p@data$paro.hom.diff06.18[is.na(p@data$paro.hom.diff06.18) & p@data$Codcir == "18"] <- mean(p@data$paro.hom.diff06.18[p@data$Codcir == "18"], na.rm = T)
p@data$paro.hom.diff06.18[is.na(p@data$paro.hom.diff06.18) & p@data$Codcir == "29"] <- mean(p@data$paro.hom.diff06.18[p@data$Codcir == "29"], na.rm = T)
length(p@data$paro.hom.diff06.18[is.na(p@data$paro.hom.diff06.18)]) # Cuento el número de secciones censales con valores NA

length(p@data$renta_disponible_media16[is.na(p@data$renta_disponible_media16)]) # Cuento el número de secciones censales con valores NA
p@data$renta_disponible_media16[is.na(p@data$renta_disponible_media16) & p@data$Codcir == "29"] <- mean(p@data$renta_disponible_media16[p@data$Codcir == "29"], na.rm = T)
p@data$renta_disponible_media16[is.na(p@data$renta_disponible_media16) & p@data$Codcir == "18"] <- mean(p@data$renta_disponible_media16[p@data$Codcir == "18"], na.rm = T)
length(p@data$renta_disponible_media16[is.na(p@data$renta_disponible_media16)]) # Cuento el número de secciones censales con valores NA


length(p@data$caidaparti15[is.na(p@data$caidaparti15)]) # Cuento el número de secciones censales con valores NA
p@data$caidaparti15[is.na(p@data$caidaparti15) & p@data$Codcir == "04"] <- mean(p@data$caidaparti15[p@data$Codcir == "04"], na.rm = T)
p@data$caidaparti15[is.na(p@data$caidaparti15) & p@data$Codcir == "11"] <- mean(p@data$caidaparti15[p@data$Codcir == "11"], na.rm = T)
p@data$caidaparti15[is.na(p@data$caidaparti15) & p@data$Codcir == "14"] <- mean(p@data$caidaparti15[p@data$Codcir == "14"], na.rm = T)
p@data$caidaparti15[is.na(p@data$caidaparti15) & p@data$Codcir == "18"] <- mean(p@data$caidaparti15[p@data$Codcir == "18"], na.rm = T)
p@data$caidaparti15[is.na(p@data$caidaparti15) & p@data$Codcir == "21"] <- mean(p@data$caidaparti15[p@data$Codcir == "21"], na.rm = T)
p@data$caidaparti15[is.na(p@data$caidaparti15) & p@data$Codcir == "23"] <- mean(p@data$caidaparti15[p@data$Codcir == "23"], na.rm = T)
p@data$caidaparti15[is.na(p@data$caidaparti15) & p@data$Codcir == "29"] <- mean(p@data$caidaparti15[p@data$Codcir == "29"], na.rm = T)
p@data$caidaparti15[is.na(p@data$caidaparti15) & p@data$Codcir == "41"] <- mean(p@data$caidaparti15[p@data$Codcir == "41"], na.rm = T)
length(p@data$caidaparti15[is.na(p@data$caidaparti15)]) # Cuento el número de secciones censales con valores NA


length(p@data$participacion15[is.na(p@data$participacion15)]) # Cuento el número de secciones censales con valores NA
p@data$participacion15[is.na(p@data$participacion15) & p@data$Codcir == "04"] <- mean(p@data$participacion15[p@data$Codcir == "04"], na.rm = T)
p@data$participacion15[is.na(p@data$participacion15) & p@data$Codcir == "11"] <- mean(p@data$participacion15[p@data$Codcir == "11"], na.rm = T)
p@data$participacion15[is.na(p@data$participacion15) & p@data$Codcir == "14"] <- mean(p@data$participacion15[p@data$Codcir == "14"], na.rm = T)
p@data$participacion15[is.na(p@data$participacion15) & p@data$Codcir == "18"] <- mean(p@data$participacion15[p@data$Codcir == "18"], na.rm = T)
p@data$participacion15[is.na(p@data$participacion15) & p@data$Codcir == "21"] <- mean(p@data$participacion15[p@data$Codcir == "21"], na.rm = T)
p@data$participacion15[is.na(p@data$participacion15) & p@data$Codcir == "23"] <- mean(p@data$participacion15[p@data$Codcir == "23"], na.rm = T)
p@data$participacion15[is.na(p@data$participacion15) & p@data$Codcir == "29"] <- mean(p@data$participacion15[p@data$Codcir == "29"], na.rm = T)
p@data$participacion15[is.na(p@data$participacion15) & p@data$Codcir == "41"] <- mean(p@data$participacion15[p@data$Codcir == "41"], na.rm = T)
length(p@data$participacion15[is.na(p@data$participacion15)]) # Cuento el número de secciones censales con valores NA




### Las variables del censo es preferible asignar un 0 porque cuando no hay valores es porque no había suficientes casos muestrales 
### y por lo tanto el valor esté seguramente más cerca de 0 que de la media (ver especificaciones del censo)
p@data$masde64[is.na(p@data$masde64)] <- 0
p@data$alquiler[is.na(p@data$alquiler)] <- 0
p@data$big.viv[is.na(p@data$big.viv)] <- 0
p@data$separados[is.na(p@data$separados)] <- 0

p@data$tipo <- factor(p@data$tipo, levels = c("Zona de densidad intermedia", "Zona rural", "Ciudades"))



#######################
### EXPORTAR EL SHP ###
#######################
#writeOGR(p, "shapefiles/condatos/",
#         driver = "ESRI Shapefile", layer = "andalucia2018", overwrite_layer = T)




