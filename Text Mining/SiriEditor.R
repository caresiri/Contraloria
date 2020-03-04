#### Eliminated data ####
library(NLP)
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




#### Divide data for class

write.csv(SIACSICOP1519, "SIACSICOP.csv")

SIAC <- SIACSICOP1519 %>% 
  filter (base == "Compras SIAC") %>%
  select(Añodeadjudicación, INSTITUCION,  DESC_PROCEDIMIENTO, DESC_BIEN_SERVICIO, CEDULA_PROVEEDOR, NOMBRE_PROVEEDOR, CANTIDAD, Cantidadadjudicada, Fechadeadjudicación, MONTO_ADJU_CRC, CED_INSTITUCION, OBJ_GASTO, DESC_GASTO, DESC_GASTO_0, DESC_GASTO_1, tech, CAT_BIEN_SERVICIO_MODIFIED)

write.csv(SIAC, "SIAC.csv")

AdjudicacionesSICOP <- SIACSICOP1519 %>% 
  filter( base == "Adjudicaciones SICOP") %>%
  select(Añodeadjudicación, INSTITUCION, DESC_PROCEDIMIENTO, DESC_BIEN_SERVICIO, CEDULA_PROVEEDOR, NOMBRE_PROVEEDOR, 
         CANTIDAD, MONTO_ADJU_CRC, CED_INSTITUCION, CARTEL_NO, INST_CARTEL_NO, PARTIDA, LINEA, FECHA_ADJUD_FIRME, COD_BIEN_SERVICIO, MONEDA_ADJUDICADA, MONTO_ADJUDICADO, CAT_BIEN_SERVICIO,  DESC_GASTO, DESC_GASTO_0, DESC_GASTO_1, tech, CAT_BIEN_SERVICIO_MODIFIED)

write.csv(AdjudicacionesSICOP, "AdjudicacionesSICOP.csv")


OfertasSICOP <- SIACSICOP1519 %>% 
  filter( base == "Ofertas SICOP")  %>%
  select(Añodeadjudicación, INSTITUCION, DESC_PROCEDIMIENTO, DESC_BIEN_SERVICIO, CEDULA_PROVEEDOR, NOMBRE_PROVEEDOR, CANTIDAD, CED_INSTITUCION,
         CARTEL_NO, INST_CARTEL_NO, PARTIDA, LINEA, COD_BIEN_SERVICIO, CAT_BIEN_SERVICIO,  DESC_GASTO, DESC_GASTO_0, DESC_GASTO_1, FECHA_PRESENTACION, MONEDA_PRESENTADA, MONTO_PRESENTADO, MONTO_PRESENT_CRC, tech, CAT_BIEN_SERVICIO_MODIFIED)
write.csv(OfertasSICOP, "OfertasSICOP.csv")

