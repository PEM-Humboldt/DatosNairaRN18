# Script para unir los archivos de excel generados luego de la revision de las fotografias de las camaras trampa instaladas para el proyecto de 
# riqueza natural en Montes de Maria el 2018, dado que no fue estandar el proceso de generacion de estos archivos se deben excluir unos especificos
# Toda variable que comience con v es vector, d dataframe, l lista

library(readxl)
# library(purrr)
# library(tidyr)
library(lubridate)
library(tidyverse)

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
dHexData <- lHexData3 %>% bind_rows(.id="Hexagono")

write.csv(dHexData,"./Salidas/data_hexagonos.csv")

#Crear variable de fecha en formato
dHexData$eventDate <- ymd(dHexData$eventDate)
dHexData$eventTimeDate <- ymd_hms(paste(dHexData$eventDate, substr(dHexData$eventTime,12,19)))
# format(as.POSIXct(Time,format="%H:%M:%S"),"%H")

#Crear ID especies
dHexData$Species <- gsub("\  ","\ ",dHexData$Species)
dSpecies <- dHexData %>% group_by(Species) %>% summarise(n=n())

dHexData$eventID <- gsub("Station: ","",dHexData$eventID) # dejo solo codigo de estacion

dHexDataGen <- dHexData %>% separate(Species, c("Genero","Epiteto","Subesp"),sep="\ ")
dGeneros <- dHexDataGen %>% group_by(Genero) %>% summarise(n=n()) %>% rowid_to_column("ID_sp") # creo el ID por genero

dHexDataGen <- left_join(dHexDataGen,dGeneros %>% select(-n),by="Genero") # asigno ID por genero a datos

#Crear grupos de fechas segun cantidad de dias
dCamaraFecha <- dHexDataGen %>% group_by(eventID,eventDate) %>% summarise(n=n()) # encuentro grupos de fecha por estacion
temp <- dCamaraFecha %>% group_by(eventID) %>% summarise(MinFecha=min(eventDate),MaxFecha=max(eventDate)) %>% na.omit()

vCantDias <- 7 # cantidad de dias en los que se van a agrupar las observaciones
lGruposDias <- list()
for(i in 1:nrow(temp)){
  # lGruposDias[[i]] <- c(seq(temp$MinFecha[i],temp$MaxFecha[i],vCantDias),temp$MaxFecha[i]) # usar si toca incluir la ultima fecha aunque no cumpla numero de dias
  lGruposDias[[i]] <- seq(temp$MinFecha[i],temp$MaxFecha[i],vCantDias)
}

#crear dataframe con columnas de rangos minimos y maximos y ID por grupo de fecha
#aprovechar este paso para chequear fechas raras
names(lGruposDias) <- temp$eventID
lGruposDias <- map(lGruposDias, function(x) data_frame(RangoMin=x))
lGruposDias <- map(lGruposDias, function(x) {
  x$RangoMax <- c(x$RangoMin[-1],NA)
  x <- x %>% rowid_to_column("ID_Rango")
  return(x[-nrow(x),])
})

dGruposDias <- lGruposDias %>% bind_rows(.id="eventID")

# asignar grupo de fecha a cada observacion   
dHexDataGen$ID_Rango <- NA
for(i in 1:nrow(dHexDataGen)){
  cameraID <- dHexDataGen$eventID[i]
  rangoMin <- dGruposDias$RangoMin[dGruposDias$eventID%in%cameraID]
  rangoMax <- dGruposDias$RangoMax[dGruposDias$eventID%in%cameraID]
  
  fecha <- dHexDataGen$eventDate[i]
  condi <- fecha >= rangoMin & fecha <= rangoMax
  if(sum(condi)==1){
    dHexDataGen$ID_Rango[i] <- dGruposDias$ID_Rango[dGruposDias$eventID%in%cameraID][condi]
  }
}

saveRDS(dHexDataGen,"./DatosFinales.R")

# ------------------------------------------
# Matriz de ocupacion por camara
# ------------------------------------------
test <- dHexDataGen %>% group_by(eventID,ID_sp,Genero, ID_Rango) %>% summarise(n=n())
test2 <- test %>% spread(ID_Rango, n)

dHexCam <- dHexDataGen %>% group_by(Hexagono,eventID,ID_Rango,Genero) %>% summarise(Cant=sum(as.numeric(Numero)))

ggplot(dHexCam %>% filter(Hexagono=="Hexagono23"), aes(x=ID_Rango,y=Cant))+geom_point()+facet_grid(eventID~Genero)

# ------------------------------------------
# Matriz de ocupacion por hexagono
# ------------------------------------------
dHexSp <- dHexDataGen %>% group_by(Hexagono,ID_Rango,Genero) %>% summarise(Cant=sum(as.numeric(Numero)))

ggplot(dHexSp %>% filter(Hexagono=="Hexagono23"), aes(x=ID_Rango,y=Cant))+geom_point()+facet_wrap(Genero~.,ncol=9)



# ------------------------------------------
# Salida DarwinCore
# ------------------------------------------

