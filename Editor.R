#install packages only once 
#install.packages('dplyr')
#install.packages('tidytext')
#install.packages('tidyr')
#install.packages('widyr')
#install.packages('ggplot2')
library(dplyr)
library(tidytext)
library(tidyr)
library(widyr)
library(ggplot2)

#### Procedimiento ####
#  - instalar paquetes de nuevo -  son unos nuevos que funcionan para todos 
#  - guardar SIACSICOP1519.Rda, d.csv, mining.xlsx, y spanish_stop_words.Rds en su folder de trabajo
#  - 1.     Revisión de tabla “d” diario para ver palabras por categorizar. 
#  - 2.     Filtrar por letra asignada
#  - 3.     Comenzar por letra asignada a revisar la tabla (Si terminan con sus letras asignadas, pueden seguir, la duplicación en este caso es positiva, el lunes hablamos más de esto)
#Letras asignadas
#A - Fabio Darío Zarza Vera
#D - Franco Pérez
#G - Juan Pablo Espinoza Cabrera
#J - Andrés Emilio Martínez Landaverde
#M- Laura Rebeca Monge Alvarado
#P - Pablo Argueta Juárez
#S – Rubén Ramírez
#V- Vladimir Eduardo Luna

#4. Correr el codigo hasta la sección de #Correlate among sections#
#5. cambie la palabra word_1 para evaluar correlaciones de palabras seleccionadas
#6. Revisar las correlaciones y la tabla Check para entender el contexto
#7. Si le sale el error (Faceting variables must have at least one value), significa que esa palabra no se correlaciona con mas de 20 palabras, revisar la tabla check solamente
#8. Categorizar las palabras según criterio en la columna [Eliminate] como {No, Tal vez, Si}
#9. Categorizar las palabras según criterio en la columna [CAT_BIEN_SERVICIO]

####Load data####
load("~/Dropbox/Contraloria/Text Mining R/CGR/SIACSICOP1519.Rda")
spanish_stop_words <- readRDS("~/Dropbox/Contraloria/Text Mining R/CGR/spanish_stop_words.Rds")
#### Generación de Corpus ####
corpus <- SIACSICOP1519 %>%
  mutate(text = gsub(x = DESC_BIEN_SERVICIO, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>%
  select(text) %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(spanish_stop_words) 

#### Cuenta de palabras ####
count_corpus <- corpus %>%
  count(word, sort = TRUE) 

####Counting words  among sections ####
word_pairs <- corpus  %>%
  pairwise_count(word, id, sort = TRUE)
word_pairs
#### Correlate among sections ####
word_1 ="suspension"
word_cors <- corpus %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, id, sort = TRUE) %>%
  filter(item1 == word_1)
#### Correlate Specific terms ####

word_cors %>%
  filter(item1 %in% c(word_1)) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()
Check <- SIACSICOP1519[grepl(word_1, SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE),] # check words in database
Check  <- Check %>% select(DESC_BIEN_SERVICIO, tech)

#### Eliminated data ####
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

#### word cloud ####
set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


#### Bigram analysis  #####
servicio_bigrams <- corpus %>%
  unnest_tokens(bigram, word, token = "ngrams", n = 2)

servicio_bigrams <- servicio_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- servicio_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# new bigram counts:
bigram_counts <- bigrams_separated %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

#analyse Bygrams
bigrams_separated %>%
  filter(word2 == "servicio")

