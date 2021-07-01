# Script para unir los archivos de excel generados con Naira luego de la revision de las fotografias de las camaras trampa instaladas para el proyecto de 
# riqueza natural en Montes de Maria el 2018
# Toda variable que comience con v es vector, d dataframe, l lista y fun funciones

library(readxl)
library(lubridate)
library(tidyverse)
library(stringr)

#obtengo ruta de archivos de salida de Naira guardados en carpeta "data"
vHexFile <- list.files("./data",full.names = T)
#creo vector de nombres de hexagonos
vHexName <- list.files("./data/")

#genero lista con las rutas de todos los archivos dentro de cada carpeta por hexagono
lHexFiles <- map(vHexFile, function(x) list.files(x,full.names = T,recursive=T))
#asigno nombre del hexagono a cada elemento de la lista
names(lHexFiles) <- vHexName
#para evitar problemas en procesamiento elimino carpetas vacías (proceso realizado cuando faltaban datos en carpetas)
lHexFiles <- compact(lHexFiles)

# #En caso de haber archivos pendientes por revision y conflictivos por carpeta de hexagono usar este codigo para remover
lHexFiles$Hexagono68[14] <- NA # remueve el archivo de posicion 14 para el Hexagono 68
lHexFiles <- map(lHexFiles, function(x) x[!is.na(x)]) # crea nueva lista sin archivos conflictivos

#leo los archivos de excel por carpeta
lHexData <- map(lHexFiles, function(x) map(x, function(y) read_excel(y))) # tener en cuenta por facilidad de lectura llamar siempre igual a la hoja de datos si es asi agregar en read_excel "sheet= NOMBRE"
# para verificar que este leyendo todos los datos exploro las columnas de cada tabla
lCheck_cols <- map(lHexData, function(x) map(x, names)) # verifico columnas de conjuntos de datos
#manualmente elimino "hoja1" vacias de Hexagono24, Hexagono43, Hexagono 68, Hexagono93 que interfieren con la lectura
#Se elimina columna "Numero" duplicada de Hexagono70 archivo 7 y archivo 9 no tenia "FileName" por lo que se creo con un CONCATENATE de 0,identifier,format

#genero vector referencia con los nombres de las columnas con las que se va a trabajar en este caso se usara el primer conjunto de datos del hexagono24
vCol_n <- names(data.frame(lHexData$Hexagono24[[1]]))

map(lCheck_cols, function(x) map(x , function(y) vCol_n[!vCol_n%in%y])) # para ver que columnas de la referencia no tienen los datos
map(lCheck_cols, function(x) map(x , function(y) y[!y%in%vCol_n])) # para ver que columnas tienen los datos que no esten en la referencia
#en Hexagono23 archivo 1 faltaban columnas: verbatimLatitude	verbatimLongitude	decimalLatitude	decimalLongitude	geodeticDatum	verbatimElevation, se agregaron manualmente pero quedan vacias
rm(lCheck_cols) #elimino para librerar espacio

#Funcion para reemplazar las variantes de la columna individualcount por numero y seleccionar las mismas columnas en el mismo orden
funRenameICount <- function(y){
  y <- data.frame(y)
  names(y) <- gsub("[Ii]ndividual[Cc]ount","Numero",names(y))
  y <- y %>% select(all_of(vCol_n))
  y <- data.frame(apply(y,2,as.character))
  return(y)
}

lHexData2 <- map(lHexData, function(x) map(x, funRenameICount))
lHexData3 <- map(lHexData2, bind_rows, .id="ID_1") # creo dataframes por hexagono con identificador de archivo como ID
dHexData4 <- lHexData3 %>% bind_rows(.id="Hexagono") %>% filter(!is.na(eventID))# creo dataframe total con identificador de hexagono elimino registros NA por lectura de archivos

#Habian diferencias entre los nombres registrados en las tablas y los nombres finales de la especie por lo que fue necesario cargar otra tabla con estos nombres finales
sp_fin <- read.csv("./lista_sp_rv2.csv") # Cargo nombres finales especies
dHexData4$Species <- gsub("  ", " ",dHexData4$Species) # se corrigio adicionalmente espacios innecesarios
dHexData4$Species <- gsub("Sciurillus pusillus pusillus", "Sciurillus pusillus",dHexData4$Species) # se cambio la subespecie por la especie

#Uno tabla de datos integrados con tabla de nombres finales
dHexData <- left_join(dHexData4,sp_fin,by=c("Species"))

#comparo nombres nuevos con viejos
# temp <- data.frame(a=dHexData$Species,b=dHexData$Species_c)
# temp2 <- temp %>% group_by(a,b) %>% summarise(n=n())
# rm(temp,temp2)

#Guardo conjunto de datos unido en tabla final
write.csv(dHexData,"./Salidas/data_hexagonos.csv")

#Crear variable de fecha en formato y variable con fecha y hora correctas pues la hora por default viene para 1989-12-31
dHexData$eventDate <- ymd(dHexData$eventDate)
dHexData$eventTimeDate <- ymd_hms(paste(dHexData$eventDate, substr(dHexData$eventTime,12,19)))

#Lista de especies
dSpecies <- dHexData %>% group_by(Species_c) %>% summarise(n=n())

dHexData$eventID <- gsub("_","-",dHexData$eventID) # corrijo diferencias
dHexData$Station <- dHexData$eventID
dHexData$eventID <- substring(dHexData$eventID,10,19) # dejo solo codigo de estacion
# dHexData$eventID <- gsub("Station: ","",dHexData$eventID) # dejo solo codigo de estacion


#Para crear identificador por especie o por genero
# dHexDataGen <- dHexData %>% separate(Species_c, c("Genus","specific_name","subespecific_name"),sep="\ ") # si se quiere crear identificador por genero y reemplazar abajo en group_by(Genus)
dIDsp <- dHexData %>% group_by(Species_c) %>% summarise(n=n()) %>% filter(!is.na(Species_c)) %>% rowid_to_column("ID_sp") # creo el ID por especie

dHexDataGen <- left_join(dHexData,dIDsp %>% select(-n),by="Species_c") # asigno ID por especie a datos

#--------------------------------------------------------
#Crear grupos de fechas segun cantidad de dias (replicas)
#--------------------------------------------------------
dHexDataGen <- dHexDataGen %>% filter(ymd(eventDate)>ymd("2018-02-27")) # para eliminar fechas raras que esten antes de la fecha del monitoreo
dCamaraFecha <- dHexDataGen %>% group_by(eventID,eventDate) %>% summarise(n=n()) # referencia de grupos de fecha por estacion
dCamaraFechaMinMax <- dCamaraFecha %>% group_by(eventID) %>% summarise(MinFecha=min(eventDate),MaxFecha=max(eventDate)) %>% na.omit() # referencia de fechas minimas y maximas por estacion

#creo lista con rangos de fechas para grupos de dias segun cantidad de dias que se va a trabajar
vCantDias <- 7 # cantidad de dias en los que se van a agrupar las observaciones
lGruposDias <- list()
for(i in 1:nrow(dCamaraFechaMinMax)){
  # lGruposDias[[i]] <- c(seq(dCamaraFechaMinMax$MinFecha[i],dCamaraFechaMinMax$MaxFecha[i]+1,vCantDias),dCamaraFechaMinMax$MaxFecha[i]) # usar si toca incluir la ultima fecha aunque no cumpla numero de dias
  lGruposDias[[i]] <- seq(dCamaraFechaMinMax$MinFecha[i],dCamaraFechaMinMax$MaxFecha[i]+1,vCantDias)
}

#crear dataframe con columnas de rangos minimos y maximos y ID para grupo de fecha en consecutivo
#aprovechar este paso para chequear fechas raras
names(lGruposDias) <- dCamaraFechaMinMax$eventID
lGruposDias <- map(lGruposDias, function(x) data_frame(RangoMin=x))
lGruposDias <- map(lGruposDias, function(x) {
  x$RangoMax <- c(x$RangoMin[-1]-1,NA) #para poner final de grupo anterior
  x <- x %>% rowid_to_column("ID_Rango")
  return(x[-nrow(x),])
})

dGruposDias <- lGruposDias %>% bind_rows(.id="eventID") # creo dataframe con rango de fechas para la cantidad de dias requeridos con identificador de grupo para cada eventID

# asignar grupo de fecha a cada observacion   
dHexDataGen$ID_Rango <- NA
for(i in 1:nrow(dHexDataGen)){
  cameraID <- dHexDataGen$eventID[i]
  rangoMin <- dGruposDias$RangoMin[dGruposDias$eventID%in%cameraID]
  rangoMax <- dGruposDias$RangoMax[dGruposDias$eventID%in%cameraID]
  
  fecha <- dHexDataGen$eventDate[i]
  condi <- fecha >= rangoMin & fecha <= rangoMax
  if(sum(condi)==1){
    dHexDataGen$ID_Rango[i] <- dGruposDias$ID_Rango[dGruposDias$eventID%in%cameraID][condi] #asigna ID de rango al grupo dentro del cual esta la fecha de la observacion
  }
}

saveRDS(dHexDataGen,"./DatosFinales.R")


# ------------------------------------------
# Matriz nula
# ------------------------------------------
dHexagonos <- dHexDataGen %>% group_by(eventID) %>% summarise(n=n())
dHexGen <- expand.grid(c(dHexagonos$eventID),c(dIDsp$ID_sp))
names(dHexGen) <- c("eventID","ID_sp")
dHexGen <- left_join(dHexGen,dIDsp%>%select(-n),by="ID_sp") # listado de todas las posibles combinaciones de especies por hexagono

dCamIDRango <- dHexDataGen %>% group_by(eventID,ID_Rango) %>% summarise(n=n()) %>% select(-n) %>% filter(!is.na(ID_Rango)) %>% mutate(Occ=0)
prueba2 <- full_join(dHexGen,dCamIDRango,by=c("eventID")) %>% filter(!is.na(ID_Rango)) #%>% spread(ID_Rango, Occ) #%>% select(-n)
prueba3 <- prueba2 %>% spread(ID_Rango, Occ) %>% select(-ID_sp)

# ------------------------------------------
# Matriz de ocupacion por camara
# ------------------------------------------
dHexDataGen2 <- dHexDataGen %>% filter(!is.na(ID_Rango))
dHexCamSp <- dHexDataGen %>% group_by(eventID,ID_sp,Species_c, ID_Rango) %>% summarise(n=sum(as.numeric(Numero),na.rm=T)) %>% mutate(Occ=as.numeric(n>0)) 
dHexCamSp <- dHexCamSp %>% filter(!is.na(ID_Rango),!is.na(Species_c))
dHexCamSp$n[is.na(dHexCamSp$n)]

min(dHexCamSp$ID_Rango)
max(dHexCamSp$ID_Rango)

dOccMatCamSp0 <- bind_rows(dHexCamSp %>% select( -n ) , prueba2)
#matriz de ocupacion con NA en grupos de dias que no tuvieron monitoreo
dOccMatCamSp <- dOccMatCamSp0 %>% group_by(eventID,ID_sp,Species_c,ID_Rango) %>% summarise(Occ=sum(Occ)) %>% pivot_wider(names_from = ID_Rango, values_from=Occ)
#Si se quiere convertir todos los NA en 0
dOccMatCamSp2 <- dOccMatCamSp0 %>% group_by(eventID,ID_sp,Species_c,ID_Rango) %>% summarise(Occ=sum(Occ)) %>% pivot_wider(names_from = ID_Rango, values_from=Occ) %>% mutate_all(~replace(., is.na(.), 0))

write.csv(dOccMatCamSp,"./OccMatCamSp.csv")

# #Verificar datos
# dOccMatCamSp %>% group_by(eventID) %>% summarise(Uno=sum(`1`,na.rm=T),Dos=sum(`2`,na.rm=T),Tres=sum(`3`,na.rm=T),Cuat=sum(`4`,na.rm=T),Cinc=sum(`5`,na.rm=T),Seis=sum(`6`,na.rm=T),Siet=sum(`7`,na.rm=T))

# ------------------------------------------
# Salida DarwinCore
# ------------------------------------------
temp1 <- map(lHexData,function(x) map(x,nrow))
temp2 <- map(temp1,unlist)
sum(unlist(temp2)) # total number of data

dHexData$Order <- str_to_sentence(dHexData$Order)

#Tabla para completar informacion para formato DwC
TablaEspecies <- dHexData %>% group_by(Class,Order,Family,Species_c) %>% summarise(n=n()) %>% separate(Species_c, c("Genus","specific_name","subespecific_name"),sep="\ ",remove=F) %>% 
  mutate(kingdom="Animalia",phylum="Chordata",taxonRank=NA)

write.csv(TablaEspecies,"./Salidas/TablaEspecies.csv")

#informacion de reino, phylum, clase y orden
info_pendiente <- read.csv("./Salidas/TablaEspecies_filled.csv")
head(info_pendiente)

names(info_pendiente) <- paste0("New_",names(info_pendiente))

TablaEspecies_pend <- cbind(TablaEspecies %>% select(Class:Species_c),info_pendiente %>% select(-New_n))

#Agrego informacion faltante por especie para formato DwC
dDwC_0 <- left_join(dHexData, TablaEspecies_pend, by=c("Class","Order","Family","Species_c"))

#estandarizo nombres
dDwC_0$recordedBy [grep("Elkin",dDwC_0$recordedBy)] <- "Elkin A. Noguera-Urbano"
dDwC_0$recordedBy [grep("Ivan",dDwC_0$recordedBy)] <- "Iván González"
dDwC_0$recordedBy [grep("Angélica",dDwC_0$recordedBy)] <- "Angélica Paola Díaz Pulido"
dDwC_0$recordedBy [grep("Yenifer",dDwC_0$recordedBy)] <- "Yenifer Herrera Varón"


dDwC_1 <- dDwC_0 %>% select(-Class,-Order,-Family,-Species_c) %>% mutate(catalogNumber="NO DOCUMENTAR",created=eventDate,creator=recordedBy,
                                                                         dateIdentified=NA,occurenceID="NO DOCUMENTAR",
                                                                         organismQuantityType="individuos",
                                                                         preparations="Fotografía", #samplingProtocol="Fototrampeo",
                                                                         scientificName=New_Especies,scientificNameAuthorship=NA,
                                                                         sex=NA,verbatimTaxonRank=NA,vernacularName=NA,organismQuantity=Numero,
                                                                         min_date=min(eventDate),max_date=max(eventDate),samplingEffort=as.numeric(max_date-min_date))
dDwC_1$identificationQualifier <- gsub("revisar","aff.",dDwC_0$MembershipDegree)

dSamplingEffort <- dHexData %>% group_by(eventID) %>% mutate(min_date=min(eventDate),max_date=max(eventDate),samplingEffort=as.numeric(max_date-min_date))
dDwC_1$samplingEffort <- dSamplingEffort$samplingEffort
dDwC_1$eventTime <- substr(dDwC_0$eventTime,12,19)

names(dDwC_1)
unique(dDwC_1$recordedBy)

dDwC_2 <- dDwC_1 %>% select(occurenceID, basisOfRecord, institutionCode, collectionCode, catalogNumber, recordNumber, recordedBy, sex, preparations,
                            eventDate, eventID, samplingProtocol, samplingEffort, measurementType0, measurementType1, measurementType2,
                            measurementValue0, measurementValue1, measurementValue2, country, stateProvince, county, municipality, locality,
                            verbatimElevation, verbatimLatitude, verbatimLongitude, geodeticDatum, identifiedBy, identificationQualifier,
                            verbatimTaxonRank, scientificName, kingdom=New_kingdom, phylum=New_phylum, class= New_Class, order=New_Order,
                            family=New_Family, genus=New_Genus, specificEpithet=New_specific_name, infraspecificEpithet=New_subespecific_name,
                            taxonRank=New_taxonRank, scientificNameAuthorship, vernacularName, organismQuantity, organismQuantityType, eventTime,
                            occurrenceRemarks, type, format,identifier,creator,created)

write.csv(dDwC_2,"./Salidas/DwC_MdM_2018.csv",row.names = F)
