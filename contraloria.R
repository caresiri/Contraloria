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
d[c(13,17,22,51,74,92,111,112,130,137,140,163,177,181,203,214,293,598),4] <- "Si"
d[c(6,10,29,39,40,43,53,50,62,91,97,99,104,114,116,128,132,134,136,146),4] <- "Talvez"
### Comentarios ###
d[6,5]<- "Corr thhn(0.48) & awg(0.42)"
d[163,5]<- "Corr pizarra(0.69)"
d[17,5]<- "Corr adhesiva(0.53)[eliminado] trasparente(0.31)"
d[34,5]<- "Corr almohadilla(0.48)[eliminado] trodat(0.42)[eliminado] automatico(0.33)[eliminado condicionalmente] grm(0.31)[eliminado]"

### Eliminated data ###
techdata <- SIACSICOP1519[!grepl(paste(filter(d, eliminate == "Si")$word, collapse="|"), SIACSICOP1519$DESC_BIEN_SERVICIO),]
## Especiales
#MARCADOR SIN RELOJ
techdata <- techdata[!grepl('(^(?=.*\\bmarcador\\b)(?!.*\\breloj\\b))', techdata$DESC_BIEN_SERVICIO, perl = TRUE),] 

#Hay algunas descripciones que tienen la palabra sello que podrian ser tecnicas
techdata <- techdata[!grepl('(^(?=.*\\bsello\\b)(?=.*\\bautomatico\\b))', techdata$DESC_BIEN_SERVICIO, perl = TRUE),] 
techdata <- techdata[!grepl('(^(?=.*\\bsello\\b)(?=.*\\bfechador\\b))', techdata$DESC_BIEN_SERVICIO, perl = TRUE),] 
techdata <- techdata[!grepl('(^(?=.*\\bsello\\b)(?=.*\\bprinter\\b))', techdata$DESC_BIEN_SERVICIO, perl = TRUE),] 
techdata <- techdata[!grepl('(^(?=.*\\bsello\\b)(?=.*\\brepuesto\\b))', techdata$DESC_BIEN_SERVICIO, perl = TRUE),] 


####Segunda Iteración####
d0<-d
#cleancorpus(techdata) FUNCTION NOT WORKING DO IT MANUALLY FOR NOW

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
d[c(602),5] <- "Si"
#Papel e impresora puede ser tecnico
techdata <- techdata[!grepl('(^(?=.*\\bpapel\\b)(?!.*\\bimpresora\\b))', techdata$DESC_BIEN_SERVICIO, perl = TRUE),] 
techdata <- techdata[!grepl(paste(filter(d, eliminate == "Si")$word, collapse="|"), techdata$DESC_BIEN_SERVICIO),]


#### Seccion de trabajo---- opcionales ####
term <- "papel"
Check <- techdata[grepl(term, techdata$DESC_BIEN_SERVICIO, perl = TRUE),] # check words in database
Check <- as.data.frame(Check$DESC_BIEN_SERVICIO)
findAssocs(dtm, terms = term, corlimit = 0.2)

# Word Cloud (optional vizualization)
set.seed(1234) 
wordcloud(words = slice(d, -1:-155)$word, freq = slice(d, -1:-155)$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


