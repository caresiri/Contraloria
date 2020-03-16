setwd("./Archivos_Fuente")
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

setwd('..')



