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
library(readxl)

setwd("/Volumes/GoogleDrive/My Drive/INCAE Work Drive/Investigador/Proyectos/Contraloria/Text Mining/workstudy")


#### Procedimiento ####
#  - instalar paquetes de nuevo -  son unos nuevos que funcionan para todos 
#  - guardar SIACSICOP1519.Rda, d.csv, mining.xlsx, y spanish_stop_words.Rds en su folder de trabajo
#  - 1.     Revisión de tabla “d” diario para ver palabras por categorizar. 
#  - 2.     Filtrar por letra asignada
#  - 3.     Comenzar por letra asignada a revisar la tabla (Si terminan con sus letras asignadas, pueden seguir, la duplicación en este caso es positiva, el lunes hablamos más de esto)
#Letras asignadas
#A-C Andrés Emilio Martínez Landaverde
#D-F Pablo Argueta Juárez
#G-I Fabio Darío Zarza Vera 
#J-L Juan Pablo Espinoza Cabrera
#M-O Laura Rebeca Monge Alvarado
#P-R Franco Pérez
#S–U Rubén Ramírez
#V-Z Vladimir Eduardo Luna

#4. Correr el codigo hasta la sección de #Correlate among sections#
#5. cambie la palabra word_1 para evaluar correlaciones de palabras seleccionadas
#6. Revisar las correlaciones y la tabla Check para entender el contexto
#7. Si le sale el error (Faceting variables must have at least one value), significa que esa palabra no se correlaciona con mas de 20 palabras, revisar la tabla check solamente
#8. Categorizar las palabras según criterio en la columna [Eliminate] como {No, Tal vez, Si}
#9. Categorizar las palabras según criterio en la columna [CAT_BIEN_SERVICIO]

####Load data####
load("SIACSICOP1519.Rda")
spanish_stop_words <- readRDS("spanish_stop_words.Rds")
reduccion <- SIACSICOP1519 %>%
  filter(tech == "No_Se")
#### Generación de Corpus ####
corpus <- reduccion %>%
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
word_1 ="color"
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
Check <- SIACSICOP1519[grepl(word_1, reduccion$DESC_BIEN_SERVICIO, perl = TRUE),] # check words in database
Check  <- Check %>% select(DESC_BIEN_SERVICIO, tech)

#### Eliminated data ####
library(NLP)
palabras_unicas <- read_excel("text_mining.xlsx", sheet = "revisadas")
correlaciones <- read_excel("text_mining.xlsx", sheet = "correlaciones")

SIACSICOP1519$tech = "No_Se"
SIACSICOP1519$tech[grepl(paste(filter(palabras_unicas, eliminate == "Si")$word, collapse="|"), SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"
SIACSICOP1519$tech[grepl(paste(filter(palabras_unicas, eliminate == "No")$word, collapse="|"), SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "Si"

## Especiales
SIACSICOP1519$tech[grepl('(^(?=.*\\bmarcador\\b)(?!.*\\breloj\\b))', SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"#MARCADOR SIN RELOJ

for(i in 1:nrow(correlaciones))
{
  SIACSICOP1519$tech[grepl(paste("(^(?=.*\\b",as.String(correlaciones[i,1]), "\\b)(?=.*\\b", as.String(correlaciones[i,2]), "\\b))", sep = "")
                           , SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = "No"
}

#### Categorización de Bienes y Servicios segun SICOP ####
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED <- SIACSICOP1519$CAT_BIEN_SERVICIO
for(i in 1:nrow(palabras_unicas))
{
SIACSICOP1519$CAT_BIEN_SERVICIO_MODIFIED[grepl(paste("(^(?=.*\\b",as.String(palabras_unicas[i,1]),"\\b))"), SIACSICOP1519$DESC_BIEN_SERVICIO, perl = TRUE)] = as.String(palabras_unicas[i,9])
}
sum(SIACSICOP1519$tech == 'No_Se')
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




#### Network Analysis ####
#https://rpubs.com/pjmurphy/317838
library(igraph)
Tech_Data <- SIACSICOP1519 %>%
  filter(tech == "Si",
         base == "Adjudicaciones SICOP") %>%
  select(NOMBRE_PROVEEDOR, INSTITUCION)

#Generate ids for Proveedores
Proveedores <- as.data.frame(unique(Tech_Data$NOMBRE_PROVEEDOR))
id <- paste("P",rownames(Proveedores), sep ="")
Proveedores <- cbind(ID_Proveedor=id, Proveedores)
names(Proveedores)[2]<-"NOMBRE_PROVEEDOR"

#Generate ids for Instituciones
Institucion <- as.data.frame(unique(Tech_Data$INSTITUCION))
id <- paste("I",rownames(Institucion), sep ="")
Institucion <- cbind(ID_Institucion=id, Institucion)
names(Institucion)[2]<-"INSTITUCION"

Tech_Data <- merge(Tech_Data, Proveedores, by = "NOMBRE_PROVEEDOR")
Tech_Data <- merge(Tech_Data, Institucion, by = "INSTITUCION")

Tech_Data <- Tech_Data %>% select(ID_Institucion, ID_Proveedor)

g <- graph.data.frame(Tech_Data, directed = FALSE)
bipartite_mapping(g)
V(g)$type <- bipartite_mapping(g)$type
plot(g)

V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"

plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")


# bipartite-specific layout.
plot(g, layout=layout.bipartite, vertex.size=5, vertex.label.cex=0.6)


# bipartite-specific layout sizing by centrality 
types <- V(g)$type                 ## getting each vertex `type` let's us sort easily
deg <- degree(g)
bet <- betweenness(g)
clos <- closeness(g)
eig <- eigen_centrality(g)$vector

cent_df <- data.frame(types, deg, bet, clos, eig)

cent_df[order(cent_df$type, decreasing = TRUE),] ## sort w/ `order` by `type`

V(g)$eig <- eig
V(g)$bet <- bet
V(g)$size <- betweenness(g)/10000
V(g)$label.cex <- betweenness(g)/10000000

plot(g, layout=layout.bipartite, vertex.label = NA)


#Bipartitate_matrix
bipartite_matrix <- as_incidence_matrix(g)
t(bipartite_matrix)

event_matrix_prod <- t(bipartite_matrix) %*% bipartite_matrix 
## crossprod() does same and scales better, but this is better to learn at first at first so you understand the method

diag(event_matrix_prod) <- 0

proveedores <- graph_from_adjacency_matrix(event_matrix_prod, 
                                              mode = "undirected", 
                                              weighted = TRUE)

E(proveedores)$weight


types <- V(proveedores)$type                 ## getting each vertex `type` let's us sort easily
deg <- degree(proveedores)
bet <- betweenness(proveedores)
eig <- eigen_centrality(proveedores)$vector

cent_df <- data.frame(types, deg, bet, eig)

cent_df[order(cent_df$type, decreasing = TRUE),] ## sort w/ `order` by `type`

V(proveedores)$eig <- eig
V(proveedores)$bet <- bet
V(proveedores)$size <- betweenness(proveedores)/2000
E(proveedores)$size <- ifelse(E(proveedores)$weight>2000,1,0.0002)

#https://stackoverflow.com/questions/32857024/increasing-the-distance-between-igraph-nodes
plot(proveedores, vertex.label = NA, ylim=c(-0.7,-0.3),xlim=c(-0.7,-0.4))

