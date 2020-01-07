#install packages only once 
#install.packages('dplyr')
#install.packages('tidytext')
#install.packages('tidyr')
#install.packages('widyr')
#install.packages('ggplot2')
list.of.packages <- c("dplyr", "tidytext", "tidyr", "widyr","ggplot2", "readxl", "easycsv", "NLP")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(dplyr)
library(tidytext)
library(tidyr)
library(widyr)
library(ggplot2)
library(readxl)
library(easycsv)

setwd(easycsv::choose_dir()) ## seleccione el directorio con el que va a trabajar
#### Procedimiento ####

library(NLP)

#### Eliminated data ####
palabras_unicas <- read_excel("text_mining.xlsx", sheet = "revisadas")
correlaciones <- read_excel("text_mining.xlsx", sheet = "correlaciones")


SIACSICOP1519$tech = "No_Se"
lower <- tolower(SIACSICOP1519$DESC_BIEN_SERVICIO)
SIACSICOP1519$tech[grepl(paste(filter(palabras_unicas, eliminate == "Si")$word, collapse="|"), lower, perl = TRUE)] = "No"
SIACSICOP1519$tech[grepl(paste(filter(palabras_unicas, eliminate == "No")$word, collapse="|"), lower, perl = TRUE)] = "Si"

## Especiales
SIACSICOP1519$tech[grepl('(^(?=.*\\bmarcador\\b)(?!.*\\breloj\\b))', lower, perl = TRUE)] = "No"#MARCADOR SIN RELOJ

for(i in 1:nrow(correlaciones))
{
  SIACSICOP1519$tech[grepl(paste("(^(?=.*\\b",as.String(correlaciones[i,1]), "\\b)(?=.*\\b", as.String(correlaciones[i,2]), "\\b))", sep = "")
                           , lower, perl = TRUE)] = "No"
}

#### Categorización de Bienes y Servicios segun SICOP ####
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED <- lower
for(i in 1:nrow(palabras_unicas))
{
  SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl(paste("(^(?=.*\\b",as.String(palabras_unicas[i,1]),"\\b))"), lower, perl = TRUE)] = as.String(palabras_unicas[i,9])
}
sum(SIACSICOP1519$tech == 'No_Se')
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\bportatil\\b))', lower, perl = TRUE)] = "Equipo informático y accesorios"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\blicencia\\b))', lower, perl = TRUE)] = "Software"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\blicencias\\b))', lower, perl = TRUE)] = "Software"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\bsoftware\\b))', lower, perl = TRUE)] = "Software"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\bmantenimiento\\b))', lower, perl = TRUE)] = "Servicios informáticos, de computación, audio y video"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\bmantenimiento y reparacion de equipo de computo y sistemas\\b))', 
                                               lower, perl = TRUE)] = "Servicios informáticos, de computación, audio y video"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\servicio\\b))', 
                                               lower, perl = TRUE)] = "Servicios informáticos, de computación, audio y video"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\servicios\\b))', 
                                               lower, perl = TRUE)] = "Servicios informáticos, de computación, audio y video"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\impresora multifuncional(fax, copiadora,escaner)\\b))', 
                                               lower, perl = TRUE)] = "Equipo informático y accesorios"
save(SIACSICOP1519, file = "SIACSICOP1519.Rda")
