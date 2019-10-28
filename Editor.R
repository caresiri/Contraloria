#### Install new packages if necesary ####
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
#### Load Packages ####
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(haven) # for reading stata files
library(stringi)
library(dplyr)


load("~/Dropbox/Contraloria/Text Mining R/CGR/SIACSICOP1519.Rda")
#### Seccion de trabajo---- opcionales ####
term <- "sello"
Check <- SIACSICOP1519[grepl(term, SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE),] # check words in database
Check  <- Check %>% select(DESC_BIEN_SERVICIO, tech)
findAssocs(dtm, terms = term, corlimit = 0.2)

#### add data to d
d[6,4] <-"Si"
write.xlsx(d, "d.xlsx")

####Primera iteración  ####
#para decidir que terminos eliminar y con que condiciones 
### Eliminated data ###
SIACSICOP1519$tech <- ifelse(grepl(paste(filter(eliminate, eliminate == "Si")$word, collapse="|"), SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE),"No","No_Se")
## Especiales
SIACSICOP1519$tech[grepl('(^(?=.*\\bmarcador\\b)(?!.*\\breloj\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"#MARCADOR SIN RELOJ

for(i in 1:nrow(mining))
{
  SIACSICOP1519$tech[grepl(paste("(^(?=.*\\b",as.String(mining[i,1]), "\\b)(?=.*\\b", as.String(mining[i,2]), "\\b))", sep = "")
                           , SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"
}

#### Categorización de Bienes y Servicios segun SICOP ####
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED <- SIACSICOP1519$CAT_BIEN_SERVICIO
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.impresora\\b))',  SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "Equipo informático y accesorios"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\busb\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "Equipo informático y accesorios"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\bportatil\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "Equipo informático y accesorios"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\blicencia\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "Software"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\blicencias\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "Software"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\bsoftware\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "Software"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\bmantenimiento\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "Servicios informáticos, de computación, audio y video"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\bmantenimiento y reparacion de equipo de computo y sistemas\\b))', 
                                               SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "Servicios informáticos, de computación, audio y video"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\servicio\\b))', 
                                               SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "Servicios informáticos, de computación, audio y video"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\servicios\\b))', 
                                               SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "Servicios informáticos, de computación, audio y video"
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\impresora multifuncional(fax, copiadora,escaner)\\b))', 
                                               SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "Equipo informático y accesorios"
save(SIACSICOP1519, file = "SIACSICOP1519.rda")

# Word Cloud (optional vizualization)
set.seed(1234) 
wordcloud(words = slice(d, -1:-155)$word, freq = slice(d, -1:-155)$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

