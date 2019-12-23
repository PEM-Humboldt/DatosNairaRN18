# Script para unir los archivos de excel generados luego de la revision de las fotografias de las camaras trampa instaladas para el proyecto de 
# riqueza natural en Montes de Maria el 2018, dado que no fue estandar el proceso de generacion de estos archivos se deben excluir unos especificos
# Toda variable que comience con v es vector, d dataframe, l lista

library(readxl)
library(purrr)
library(tidyr)
library(dplyr)

#obtengo ruta
vHexFile <- list.files("./data",full.names = T)
#creo vector de nombres de hexagonos
vHexName <- list.files("./data/")

#genero lista con las rutas de todos los archivos dentro de cada carpeta
lHexFiles <- map(vHexFile, function(x) list.files(x,full.names = T,recursive=T))
#asigno nombre del hexágono a cada elemento de la lista
names(lHexFiles) <- vHexName
#elimino carpetas vacías
lHexFiles2 <- compact(lHexFiles)
#remuevo archivos pendientes por revision
lHexFiles2$Hexagono87[9] <- NA 
lHexFiles2$Hexagono68[14] <- NA
lHexFiles2$Hexagono23[1] <- NA
lHexFiles2 <- map(lHexFiles2, function(x) x[!is.na(x)])

#leo los archivos de excel por carpeta
lHexData <- map(lHexFiles2, function(x) map(x, function(y) read_excel(y)))
#genero vector con los nombres de las columnas con las que me voy a quedar
col_n <- c(names(data.frame(lHexData[[2]][1])))

funRenameICount <- function(y){
  y <- data.frame(y)
  names(y) <- gsub("[Ii]ndividual[Cc]ount","Numero",names(y))
  y <- y %>% select(col_n)
  y <- data.frame(apply(y,2,as.character))
  return(y)
}

lHexData2 <- map(lHexData, function(x) map(x, funRenameICount))
lHexData3 <- map(lHexData2, bind_rows, .id="ID_1")
lHexData4 <- lHexData3 %>% bind_rows(.id="Hexagono")

write.csv(lHexData4,"./Salidas/data_hexagonos.csv")
