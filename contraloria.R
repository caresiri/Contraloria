#### Install new packages if necesary ####
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
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
cleancorpus <- function(a) {
  corpus <- VCorpus(VectorSource(a$DESC_BIEN_SERVICIO)) #Generación de corpus de Descripción de Bienes y Servicios del SIAC
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) #Eliminar caracteres especiales
  corpus <- tm_map(corpus, toSpace, "/")#Eliminar caracteres especiales
  corpus <- tm_map(corpus, toSpace, "@")#Eliminar caracteres especiales
  corpus <- tm_map(corpus, toSpace, "\\|")#Eliminar caracteres especiales
  corpus <- tm_map(corpus, removeNumbers) # Remover numeros
  corpus <- tm_map(corpus, removeWords, stopwords("spanish")) # Remover stopwords de español
  corpus <- tm_map(corpus, removePunctuation) # Remove puntuación
  corpus <- tm_map(corpus, stripWhitespace) # Eliminate extra white spaces
  # Remove your own stop word
  # specify your stopwords as a character vector
  #corpus <- tm_map(corpus, removeWords, c("blabla1", "blabla2")) 
  # estos comandos ya fueron ejecutados desde la base de datos, no es necesario repetirlo
  #corpus <- tm_map(corpus, content_transformer(tolower)) # Convertir texto a minuscula
  #removeAccents <- content_transformer(function(x) chartr("áéíóú", "aeiou", x))# Remove Accents
  #corpus <- tm_map(corpus, removeAccents)# Remove Accents
  # Text stemming
  #corpus <- tm_map(corpus, stemDocument, language = "spanish")
  #### Term document Matrix ####
  dtm <- TermDocumentMatrix(corpus)
  m <- as.matrix(dtm) # as matrix
  v <- sort(rowSums(m),decreasing=TRUE) # sort by sum
  d <- data.frame(word = names(v),freq=v) # frequency data frame
  head(d, 10) #display top 10
  return(d)
} #function broken FIX

corpus <- VCorpus(VectorSource(SIACSICOP1519$DESC_BIEN_SERVICIO)) #Generación de corpus de Descripción de Bienes y Servicios del SIAC
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) #Eliminar caracteres especiales
corpus <- tm_map(corpus, toSpace, "/")#Eliminar caracteres especiales
corpus <- tm_map(corpus, toSpace, "@")#Eliminar caracteres especiales
corpus <- tm_map(corpus, toSpace, "\\|")#Eliminar caracteres especiales
corpus <- tm_map(corpus, removeNumbers) # Remover numeros
corpus <- tm_map(corpus, removeWords, stopwords("spanish")) # Remover stopwords de español
corpus <- tm_map(corpus, removePunctuation) # Remove puntuación
corpus <- tm_map(corpus, stripWhitespace) # Eliminate extra white spaces
# Remove your own stop word
# specify your stopwords as a character vector
#corpus <- tm_map(corpus, removeWords, c("blabla1", "blabla2")) 
# estos comandos ya fueron ejecutados desde la base de datos, no es necesario repetirlo
#corpus <- tm_map(corpus, content_transformer(tolower)) # Convertir texto a minuscula
#removeAccents <- content_transformer(function(x) chartr("áéíóú", "aeiou", x))# Remove Accents
#corpus <- tm_map(corpus, removeAccents)# Remove Accents
# Text stemming
#corpus <- tm_map(corpus, stemDocument, language = "spanish")
#### Term document Matrix ####
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm) # as matrix
v <- sort(rowSums(m),decreasing=TRUE) # sort by sum
d <- data.frame(word = names(v),freq=v) # frequency data frame
head(d, 10) #display top 10

####Primera iteración  ####
#para decidir que terminos eliminar y con que condiciones 
d$ID <- seq.int(nrow(d))
d$eliminate<-""
d$comentarios<- ""
d[c(17,22,51,74,92,111,112,130,137,140,163,177,181,203,214,293,598),4] <- "Si"
d[c(13,6,10,29,39,40,43,53,50,62,91,97,99,104,114,116,128,132,134,136,146),4] <- "Talvez"
### Comentarios ###
d[6,5]<- "Corr thhn(0.48) & awg(0.42)"
d[163,5]<- "Corr pizarra(0.69)"
d[17,5]<- "Corr adhesiva(0.53)[eliminado] trasparente(0.31)"
d[34,5]<- "Corr almohadilla(0.48)[eliminado] trodat(0.42)[eliminado] automatico(0.33)[eliminado condicionalmente] grm(0.31)[eliminado]"

### Eliminated data ###
SIACSICOP1519$tech <- ifelse(grepl(paste(filter(d, eliminate == "Si")$word, collapse="|"), SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE),"No","No_Se")
## Especiales
SIACSICOP1519$tech[grepl('(^(?=.*\\bmarcador\\b)(?!.*\\breloj\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"#MARCADOR SIN RELOJ
SIACSICOP1519$tech[grepl('(^(?=.*\\bsello\\b)(?=.*\\bautomatico\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"
SIACSICOP1519$tech[grepl('(^(?=.*\\bsello\\b)(?=.*\\bfechador\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"
SIACSICOP1519$tech[grepl('(^(?=.*\\bsello\\b)(?=.*\\bprinter\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"
SIACSICOP1519$tech[grepl('(^(?=.*\\bsello\\b)(?=.*\\brepuesto\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"

####Segunda Iteración####
d0<-d
#cleancorpus(techdata) FUNCTION NOT WORKING DO IT MANUALLY FOR NOW
techdata <- SIACSICOP1519 %>% filter(tech=="No_Se")
save(techdata, file = "techdata.RData")
corpus <- VCorpus(VectorSource(techdata$DESC_BIEN_SERVICIO)) #Generación de corpus de Descripción de Bienes y Servicios del SIAC
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) #Eliminar caracteres especiales
corpus <- tm_map(corpus, toSpace, "/")#Eliminar caracteres especiales
corpus <- tm_map(corpus, toSpace, "@")#Eliminar caracteres especiales
corpus <- tm_map(corpus, toSpace, "\\|")#Eliminar caracteres especiales
corpus <- tm_map(corpus, removeNumbers) # Remover numeros
corpus <- tm_map(corpus, removeWords, stopwords("spanish")) # Remover stopwords de español
corpus <- tm_map(corpus, removePunctuation) # Remove puntuación
corpus <- tm_map(corpus, stripWhitespace) # Eliminate extra white spaces

#### Term document Matrix ####
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm) # as matrix
v <- sort(rowSums(m),decreasing=TRUE) # sort by sum
d <- data.frame(word = names(v),freq=v) # frequency data frame
head(d, 10) #display top 10
d$ID <- seq.int(nrow(d))
d <- merge(d, d0[,c("word", "freq", "eliminate", "comentarios", "ID")], by = "word", all.x = TRUE)
d <- d[order(-d[,2]),] # order by frequency
rownames(d) <- NULL # reset column order

#### Eliminated data ####
d[c(196,225,298,602,247,975,1748,1812),5] <- "Si"
d[c(1,2,3,4,5,6,7,8,9,10,11,13,14,16,17,18,19,21,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,41,42,43,44,45,46,47,49,50,51,52,54),5] <- "No"
d[c(15,20,39,40,48,152),5] <- "Talvez"

SIACSICOP1519$tech[grepl('(^(?=.*\\bpapel\\b)(?!.*\\bimpresora\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No" # dejar Papel e impresora puede ser tecnico
SIACSICOP1519$tech[grepl('(^(?=.*\\bplastico\\b)(?=.*\\badhesivo\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"
SIACSICOP1519$tech[grepl('(^(?=.*\\bplastico\\b)(?=.*\\brollo\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"
SIACSICOP1519$tech[grepl('(^(?=.*\\bplastico\\b)(?=.*\\bforro\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"
SIACSICOP1519$tech[grepl('grepl(paste(filter(d, eliminate == "Si")$word, collapse="|")', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"

#techdata <- techdata[!grepl(paste(filter(d, eliminate == "Si")$word, collapse="|"), techdata$DESC_BIEN_SERVICIO),]

#### Seccion de trabajo---- opcionales ####
term <- "led"
Check <- techdata[grepl(term, techdata$DESC_BIEN_SERVICIO, perl = TRUE),] # check words in database
Check <- as.data.frame(Check$DESC_BIEN_SERVICIO)
findAssocs(dtm, terms = term, corlimit = 0.2)

# Word Cloud (optional vizualization)
set.seed(1234) 
wordcloud(words = slice(d, -1:-155)$word, freq = slice(d, -1:-155)$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


