#######   CREO LA FUNCION
## Calcula la I de Moran local de una variable para todos los poligonos de un shapefile
visualmoran <- function(p, col, morancol, manyNA = FALSE) {
  
  # Cargo la librería necesaria
  library(spdep)

  
  if (manyNA == TRUE) {
    p@data[, col][is.na(p@data[, col])] <- mean(p@data[, col], na.rm = T)  # Asigno la media las secciones censales sin valor
  } else {
    p <- p[!is.na(p@data[, col]),]  # Elimino las secciones censales sin valor
  }  
  
  w <- poly2nb(p, snap = 1)  # Creo la matriz de adyacencia
  ww <-  nb2listw(w, zero.policy = TRUE, style = "W")  # Transoformo la matriz en una lista
  
  
  ### LISA MAP
  # create row-standardized Queens contiguity weights matrix
  
  locm <- localmoran(p@data[, col], ww, zero.policy = T)  # Calculo la I de Moran local
  summary(locm)
  
  
  nscal <- ncol(p) + 1
  scal <- paste("scale", morancol, sep = "_")
  p@data[, nscal] <- NA
  colnames(p@data)[nscal] <- scal
  p@data[, nscal] <- as.numeric(scale(p@data[, col]))  # Estandarizo la variable
  scal <- p@data[, nscal]
  
  nlag <- ncol(p) + 1
  lag <- paste("lag", morancol, sep = "_")
  p@data[,nlag] <- NA
  colnames(p@data)[nlag] <- lag
  p@data[,nlag] <- lag.listw(ww, scal, zero.policy = T)   # Creo la variable de retardo espacial
  lag <- p@data[,nlag]
  
  
  # Identifico el cuadrante para cada observacion
  nquad_sig <- ncol(p) + 1
  quad_sig <- paste("quad", morancol, sep = "_")
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
  
  nmorancol <- ncol(p) + 1
  p@data[, nmorancol] <- NA
  colnames(p@data)[nmorancol] <- morancol
  
  p@data[, nmorancol] <- factor(quad_sig, 
                                levels = c(1,2,3,4,5), 
                                labels = labels)
  
  return(p)
}