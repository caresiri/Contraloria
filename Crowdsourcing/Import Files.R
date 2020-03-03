setwd(easycsv::choose_dir()) ## seleccione el directorio con el que va a trabajar
library(readxl)
library(dplyr)
Adj_2015 <- read_excel("Reporte adjudicaciones x codigo catalogo 2015.xlsx")
Adj_2016 <- read_excel("Reporte adjudicaciones x codigo catalogo 2016.xlsx")
Adj_2017 <- read_excel("Reporte adjudicaciones x codigo catalogo 2017.xlsx")
Adj_2018 <- read_excel("Reporte adjudicacioenes x codigo catalogo 2018.xlsx")
Adj_2019 <- read_excel("Reporte adjudicacioenes x codigo catalogo 2019.xlsx")


Of_2015 <- read_excel("Reporte ofertas presentadas x codigo catalogo 2015.xlsx")
Of_2016 <- read_excel("Reporte ofertas presentadas x codigo catalogo 2016.xlsx")
Of_2017 <- read_excel("Reporte ofertas presentadas x codigo catalogo 2017.xlsx")
Of_2018_19 <- read_excel("Reporte ofertas x codigo catalogo 2018-2019.xlsx")

SIAC_15_17 <- read_excel("SIAC 2015-2017.xlsx")
SIAC_18_19 <- read_excel("ComprasSIAC.xls")

SIAC_18_19$`Año de adjudicación` <- as.numeric(SIAC_18_19$`Año de adjudicación`)

Adj_Total <- bind_rows(Adj_2015, Adj_2016, Adj_2017, Adj_2018, Adj_2019)
Of_Total <- bind_rows(Of_2015, Of_2016, Of_2017, Of_2018_19)
SIAC_Total <-bind_rows(SIAC_15_17,SIAC_18_19)

###Prepare for join 
SIAC_Total$`Clave de la línea del procedimiento`<- substr(SIAC_Total$`Clave de la línea del procedimiento`,1,10)
SIAC_Total$OBJ_GASTO <- substr(SIAC_Total$`Subpartida (COG)(AC)`,1,7)
SIAC_Total$`Subpartida (COG)(AC)`<- substr(SIAC_Total$`Subpartida (COG)(AC)`,10,100)
SIAC_Total <- SIAC_Total %>% rename(INSTITUCION = `Nombre de la entidad madre`,
                                    DESC_PROCEDIMIENTO = `Objeto contractual`,
                                    DESC_BIEN_SERVICIO = `Descripción del bien o servicio`,
                                    CEDULA_PROVEEDOR = `Cédula del adjudicatario`,
                                    NOMBRE_PROVEEDOR = `Nombre del adjudicatario`,
                                    CANTIDAD = `Cantidad licitada`,
                                    FECHA_ADJUD_FIRME = `Fecha de adjudicación`,
                                    MONTO_ADJU_CRC = `Monto adjudicados`,
                                    CED_INSTITUCION = `Clave de la línea del procedimiento`,
                                    DESC_GASTO = `Subpartida (COG)(AC)`)
SIAC_Total$CEDULA_PROVEEDOR <- as.character(SIAC_Total$CEDULA_PROVEEDOR)

Adj_Of <- bind_rows(Adj_Total, Of_Total)
Adj_Of$CAT_BIEN_SERVICIO <- ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="8111","Servicios informáticos, de computación, audio y video",
                                   ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,2)=="43","Telecomunicaciones y radiofusión de de tecnología de la información",
                                          ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="8116","Entrega de Servicios de Tecnología de Información",
                                                 ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4319","Dispositivos de comunicaciones y accesorios",
                                                        ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4320","Componentes para tecnología de la información, difusión o telecomunicaciones",
                                                               ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4321","Equipo informático y accesorios",
                                                                      ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4322","Equipos de redes de voz, multimedia, o plataformas y accesorios",
                                                                             ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4323","Software",NA))))))))


Adj_Of$'Año de adjudicación'<- as.numeric(paste("20",substr(Adj_Of$FECHA_ADJUD_FIRME,7,8), sep=""))
SIAC_Total$FECHA_ADJUD_FIRME <- as.character(SIAC_Total$FECHA_ADJUD_FIRME)

SIACSICOP1519  <-bind_rows(Adj_Of, SIAC_Total)



