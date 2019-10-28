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
#### Extraer Datos ####

#SIACSICOP1519 <- read_dta("") # Read Stata data into R when needed
SIACSICOP1519$DESC_BIEN_SERVICIO <- tolower(SIACSICOP1519$DESC_BIEN_SERVICIO) #convert to lower case
SIACSICOP1519$DESC_BIEN_SERVICIO <- stri_trans_general(SIACSICOP1519$DESC_BIEN_SERVICIO, id = "Latin-ASCII") # eliminate accents

corpus <- VCorpus(VectorSource(SIACSICOP1519$DESC_BIEN_SERVICIO)) #Generación de corpus de Descripción de Bienes y Servicios del SIAC
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) #Eliminar caracteres especiales
corpus <- corpus %>% tm_map(toSpace, "/") %>%
  tm_map(toSpace, "@") %>%#Eliminar caracteres especiales
  tm_map(toSpace, "\\|") %>%#Eliminar caracteres especiales
  tm_map(removeNumbers)  %>%# Remover numeros
  tm_map(removeWords, stopwords("spanish"))  %>%# Remover stopwords de español
  tm_map(removePunctuation)  %>% # Remove puntuación
  tm_map(stripWhitespace) # Eliminate extra white spaces  
  
#### Term document Matrix ####
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm) # as matrix  # saved m as m0.rdata
v <- sort(rowSums(m),decreasing=TRUE) # sort by sum  # saved v as v0.rdata
d <- data.frame(word = names(v),freq=v) # frequency data frame  # saved d as d0.rdata
head(d, 10) #display top 10

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


#### Seccion de trabajo---- opcionales ####
term <- "sello"
Check <- SIACSICOP1519[grepl(term, SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE),] # check words in database
Check  <- Check %>% select(DESC_BIEN_SERVICIO, tech)
findAssocs(dtm, terms = term, corlimit = 0.2)

SIACSICOP1519$tech[grepl(paste("(^(?=.*\\b",as.String(mining[3,1]), "\\b)(?=.*\\b", as.String(mining[3,2]), "\\b))", sep = "")
                         , SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"

ifelse(stri_detect(SIACSICOP1519$DESC_BIEN_SERVICIO, 
                   regex = (paste(c(mining[3,1], mining[3,2]), collapse="|"))),SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED == "No", SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED == "No_Se")
#paste('(^(?=.*\\b,]')

#'(^(?=.*\\bsello\\b)(?=.*\\bprinter\\b))'

#str_detect(payload, paste(c("create", "drop", "select"),collapse = '|')



#### add data to d
d[6,4] <-"Si"

# Word Cloud (optional vizualization)
set.seed(1234) 
wordcloud(words = slice(d, -1:-155)$word, freq = slice(d, -1:-155)$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#### NUEVAS VARIABLES####

SIACSICOP1519 <- SIACSICOP1519 %>% mutate(CODIGO_CLASIFICACION = substr(COD_BIEN_SERVICIO, 1, 8))
SIACSICOP1519 <- SIACSICOP1519 %>% mutate(CODIGO_IDENTIFICACION = substr(COD_BIEN_SERVICIO, 9, 16))
#GET reporte Catalogo de productos table
CODIGO_CLASIFICACION <- Reporte_Catálogo_de_Productos[, c("CODIGO_CLASIFICACION", "NOMBRE_CLASIFICACIÓN")]
CODIGO_CLASIFICACION <- CODIGO_CLASIFICACION %>% distinct()
SIACSICOP1519 <- merge(x = SIACSICOP1519, y = CODIGO_CLASIFICACION[, c("CODIGO_CLASIFICACION", "NOMBRE_CLASIFICACIÓN")], by = "CODIGO_CLASIFICACION", all.x = TRUE)

CODIGO_IDENTIFICACION <- Reporte_Catálogo_de_Productos[, c("CODIGO_IDENTIFICACION", "NOMBRE_IDENTIFICACIÓN")]
CODIGO_IDENTIFICACION <- CODIGO_IDENTIFICACION %>% distinct()
SIACSICOP1519 <- merge(x = SIACSICOP1519, y = CODIGO_IDENTIFICACION[, c("CODIGO_IDENTIFICACION", "NOMBRE_IDENTIFICACIÓN")], by = "CODIGO_IDENTIFICACION", all.x = TRUE)

write.xlsx(d, "d.xlsx")
