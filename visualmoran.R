## Calcula la I de Moran local de una variable para todos los poligonos de un shapefile.
## Devuelve el mismo shapefile pero con tres columnas más:
### 1) la variable especificada como col en la función escalada para que la media sea 0 y la desviación tipica 1
### 2) El retardo espacial de la variable escalada
### 3) El cuadrante al que corresponde cada linea (zona) siempre que sea significativo, si no lo es el valor será No Signif.

visualmoran <- function(shp, listw, col, NA.impute = F) {
  
  ww <- listw

  # Cargo la librería necesaria
  library(spdep)
  
  
  if (NA.impute == TRUE) {
    p@data[, col][is.na(p@data[, col])] <- mean(p@data[, col], na.rm = T)  # Asigno la media las secciones censales sin valor
  }
  
  ### LISA MAP
  # create row-standardized Queens contiguity weights matrix
  
  locm <- localmoran(p@data[, col], ww, zero.policy = T, na.action = na.exclude, alternative = "two.sided")  # Calculo la I de Moran local
  
  nscal <- ncol(p) + 1
  scal <- paste(col, "scale", sep = "_")
  p@data[, nscal] <- NA
  colnames(p@data)[nscal] <- scal
  p@data[, nscal] <- as.numeric(scale(p@data[, col]))  # Estandarizo la variable
  scal <- p@data[, nscal]
  
  nlag <- ncol(p) + 1
  lag <- paste(col, "lag", sep = "_")
  p@data[,nlag] <- NA
  colnames(p@data)[nlag] <- lag
  try(p@data[,nlag] <- lag.listw(ww, scal, zero.policy = T, NAOK = T), silent = T)   # Creo la variable de retardo espacial
  lag <- p@data[,nlag]
  
  
  # Identifico el cuadrante para cada observacion
  nquad_sig <- ncol(p) + 1
  quad_sig <- paste("quad", col, sep = "_")
  p@data[, nquad_sig] <- NA
  colnames(p@data)[nquad_sig] <- quad_sig
  
  p@data[(scal >= 0 & lag >= 0) & (locm[, 5] <= 0.05), nquad_sig] <- 1
  p@data[(scal <= 0 & lag <= 0) & (locm[, 5] <= 0.05), nquad_sig] <- 2
  p@data[(scal >= 0 & lag <= 0) & (locm[, 5] <= 0.05), nquad_sig] <- 3
  p@data[(scal <= 0 & lag >= 0) & (locm[, 5] <= 0.05), nquad_sig] <- 4
  p@data[(locm[, 5] > 0.05), nquad_sig] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS
  
  quad_sig <- p@data[, nquad_sig]
  
  # Creo las etiquetas para cada uno de los cuadrantes
  labels <- c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No Signif.")
  
  p@data[, nquad_sig] <- factor(quad_sig, 
                                levels = c(1,2,3,4,5), 
                                labels = labels)
  
  return(p)
}
