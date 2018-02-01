###########################################################################################################
#
#   Paso 1, Verificación de la serie
#
###########################################################################################################

#Funciones
source("fondos.R",local=FALSE)

#Paquete
library(readxl)
library(dplyr)
library(FundTools)
#Directorio de trabajo
archivo <- read_excel("OrdenPrueba.xls")
archivo$Serie <- gsub("'","",archivo$Serie)

diainhabil <-  function(fecha){
  fechabase0 <- as.Date("2017-08-06")
  fechabase1 <- as.Date("2017-08-07")
  if(as.integer(fecha - fechabase0 ) %% 7 == 6){
    fecha = fecha + 3 
  }
  if(as.integer(fecha - fechabase1 ) %% 7 == 6){
    fecha = fecha + 2 
  }
  return(fecha)
}

####################################################################################################
#                                      Fondos a Reclasificar                                       #
####################################################################################################

fondos <- c('+CIGUB','+CIGUMP','+CIGULP','+CIUSD','+CIEQUS','+CIBOLS','AXESEDM')
fondosp <- c('51-+CIGUB-C-0','51-+CIGUB-C-1','51-+CIGUB-C-2','51-+CIGUB-C-3','51-+CIGUB-C-4','51-+CIGUB-BE-3',
             '51-+CIGUB-BE-4','51-+CIGUMP-C-0','51-+CIGUMP-C-1','51-+CIGUMP-C-2','51-+CIGUMP-C-3','51-+CIGUMP-C-4',
             '51-+CIGUMP-BE-3','51-+CIGUMP-BE-4','51-+CIGULP-C-0','51-+CIGULP-C-1','51-+CIGULP-C-2','51-+CIGULP-C-3',
             '51-+CIGULP-C-4','51-+CIGULP-BE-3','51-+CIGULP-BE-4','51-+CIUSD-C-0','51-+CIUSD-C-1','51-+CIUSD-C-2',
             '51-+CIUSD-C-3','51-+CIUSD-C-4','51-+CIUSD-BE-3','51-+CIUSD-BE-4','52-+CIEQUS-C-0','52-+CIEQUS-C-1',
             '52-+CIEQUS-C-2','52-+CIEQUS-C-3','52-+CIEQUS-C-4','52-+CIEQUS-BE-3','52-+CIEQUS-BE-4','52-+CIBOLS-C-0',
             '52-+CIBOLS-C-1','52-+CIBOLS-C-2','52-+CIBOLS-C-3','52-+CIBOLS-C-4','52-+CIBOLS-BE-3','52-+CIBOLS-BE-4',
             '52-AXESEDM-F1','52-AXESEDM-F3','52-AXESEDM-M1','52-AXESEDM-M3')

####################################################################################################
#                                 Data frame con las viejas series                                 #
####################################################################################################

datosventa <- archivo %>%
  filter(Emisora %in% fondos & Importe > 0 & Serie != 'BE-0') %>%
  group_by(CContrato, Emisora, Serie) %>%
  summarise(Titulos = sum(Títulos), Importe = sum(Importe))

####################################################################################################
#                                 Data frame con las nuevas series                                 #
####################################################################################################

datoscompra <- archivo %>% 
  filter(Emisora %in% fondos & Importe > 0 & Serie != 'BE-0') %>%
  group_by(CContrato, Emisora) %>%
  summarise(Titulos = sum(Títulos), Importe = sum(Importe))

#Serie nueva 

seriew <- c()
for(i in seq(1,length(datoscompra$Importe),1)){
  ind1 <- datosventa$CContrato == datoscompra$CContrato[i]
  ind2 <- datosventa$Emisora == datoscompra$Emisora[i]
  indices <- ifelse(ind1 == TRUE,ind2,ind1)
  tipo <- datosventa$Serie[indices]
  if(nchar(tipo) <= 2){
    tipo <- substr(tipo,1,1)
  } else {
    tipo <- strsplit(tipo,"-")[[1]][1]
  }
  seriew <- c(seriew,serie(datoscompra$Importe[i],as.character(tipo)))
}
datoscompra$Serie <- seriew

####################################################################################################
#                                         Reclasificacion                                          #
####################################################################################################

datos <- merge(datosventa,datoscompra,by.x = c('CContrato','Emisora'),by.y = c('CContrato','Emisora'))
ventas <- ifelse(datos$Serie.x == datos$Serie.y, "No Reclasificar","Reclasificar")
datos$Venta <- ventas
datos <- datos %>% filter(Venta == 'Reclasificar')
datos$Titulos.y <- NULL
datos$Importe.y <- NULL
colnames(datos) <- c('Contrato','Fondo','SerieAnterior','Titulos','Importe','SerieNueva','Accion a realizar')

#Contratos a omitir
omitir <- read_excel('contratos.xlsx')
quitar <- array(match(omitir$Contrato,datos$Contrato))
if(length(quitar) == 0){datos1 <- datos} else {datos1 <- datos[-quitar,]}
#### Creación del documento csv
datos2 <- datos1
datos2$Fondo <- paste0("'",datos1$Fondo)
write.csv(datos2,"NuevasPosiciones.csv",col.names=TRUE)

###########################################################################################################
#
#   Paso 2, Venta
#
###########################################################################################################
#Fondo
fondo <- datos1$Fondo

#Contratos
contratos <- datos1$Contrato

#Tipo de operacion
operacion <- rep("VTA-SI",length(contratos))

#Serie de la venta
serie1 <- datos1$SerieAnterior

#Tipo de valor
tipo <- ifelse(fondo =="+CIEQUS",52,ifelse(fondo =="+CIBOLS",52,ifelse(fondo == "AXESEDM",52,51)))

#Precio
precios <- get_prices(Sys.Date()-1,fondosp)
prices <- function(tipo,fund,serie){
  namesp <- colnames(precios)
  namesf <- paste0(tipo,"-",fund,"-",serie)
  price <- precios[1,which(namesp == namesf)]
  return(as.numeric(price))
}
precio <- mapply(prices,tipo,fondo,serie1)

#Títulos
titulos <- as.character(datos1$Titulos)

#Importe de la operacion
importe1 <- as.numeric(titulos)*precio
importe <- as.character(importe1)

#Fecha de Operacion
foperacion <- format(Sys.Date(), "%d/%m/%Y")

#Fecha de liquidacion (en días)
liq <- cbind(c("+CIGUB","+CIPLUS","+CIGUMP","+CIGULP","+CIUSD","+CIEQUS","+CIBOLS","AXESEDM"),c(0,0,2,2,2,2,2,3))
liquidacion <- function(valor){
  vector <- match(valor,liq)
  fliq <- liq[vector,2]
  fechas <- Sys.Date()+as.numeric(fliq)
  fechas <- lapply(fechas,diainhabil)
  fechas <- do.call("c",fechas)
  fliquidacion <- format(fechas,"%d/%m/%Y")
  return(fliquidacion)
}
fliquidacion <- liquidacion(fondo)

#Fecha de Captura
#numero <- ifelse(fondo=="+CIGUB",0,ifelse(fondo=="+CIPLUS",0,-1))
fcaptura <- format(Sys.Date(), "%d/%m/%Y")

#### Creacion del documento txt
zero <- as.character(integer(length(fondo)))
documento <- c("",paste0(operacion,"|",contratos,"|",fondo,"|",serie1,"|",tipo,"|",titulos,"|",precio,"|",zero,"|",zero,"|",zero,"|",zero,"|",zero,"|",importe,"|",fliquidacion,"|",zero,"|",fcaptura,"|",zero,"|",zero,"|",importe,"|",foperacion,"|",precio,"|",zero))
#write.table(documento,"ventas.txt",quote = FALSE,row.names=FALSE,col.names=FALSE)
x <- capture.output(write.table(documento, row.names = FALSE, col.names = FALSE, quote = FALSE))
cat(paste(x, collapse = "\n"), file = "venta.txt")


###########################################################################################################
#
#   Paso 3, Compra
#
###########################################################################################################

#Tipo de operacion
operacion <- rep("Compra Sociedades Inversio",length(contratos))

#Serie
serie2 <- datos1$SerieNueva

#Precio
precio <- mapply(prices,tipo,fondo,serie2)

#Títulos
titulos <- as.character(importe1%/%precio)

#Importe
importe <- as.character(precio*as.numeric(titulos))

#### Creacion del documento txt
zero <- as.character(integer(length(fondo)))
documento <- c("",paste0(operacion,"|",contratos,"|",fondo,"|",serie2,"|",tipo,"|",titulos,"|",precio,"|",zero,"|",zero,"|",zero,"|",zero,"|",zero,"|",importe,"|",fliquidacion,"|",zero,"|",fcaptura,"|",zero,"|",zero,"|",importe,"|",foperacion,"|",precio,"|",zero))
#write.table(documento,"compra.txt",quote = FALSE,row.names=FALSE,col.names=FALSE)
x <- capture.output(write.table(documento, row.names = FALSE, col.names = FALSE, quote = FALSE))
cat(paste(x, collapse = "\n"), file = "compra.txt")
