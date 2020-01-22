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
#library(readxl)
#SIACSICOP1519 <- read_excel("adjudicacionesycompras.xlsx", 
#                                          col_types = c("text", "text", "text", 
#                                                                  "text", "text", "text", "text", "numeric", 
#                                                                 "numeric", "numeric", "text", "text", 
#                                                                "text", "text", "numeric", "numeric", 
#                                                               "date", "numeric", "text", "text", 
#                                                              "text", "text", "text", "text", "numeric", 
#                                                             "date", "text", "text", "numeric", 
#                                                            "date", "text", "numeric", "numeric", 
#                                                           "text", "text", "text", "text", "text", 
#                                                          "numeric", "numeric"))
#1.	Bajar R y R Studio (Instrucciones en anexo)
#2.	instalar paquetes de nuevo -  son unos nuevos que funcionan para todos
#3.	guardar SIACSICOP1519.Rda, categorización.xlsx, y spanish_stop_words.Rds en su folder de trabajo
#4.	Abrir categorización.xlsx
#5.	Filtrar por letra asignada
#6.	Ver video explicativo

#Letras asignadas
#A-B Ximena Rodriguez
#C-D Daniel Corrales
#E-F Ruben Ramirez
#G-H Hans Fritas 
#I-J Andres Emilio
#K-L Vladimir
#M-N Xavier Muñoz 
#O-P Sergio Castro
#Q-R Marcia
#S-T Juan Pablo
#U-V Pablo Argueta
#W-Z PRebecca Monge

#4. Correr el codigo hasta la sección de #Correlate among sections#
#5. cambie la palabra word_1 para evaluar correlaciones de palabras seleccionadas
#6. Revisar las correlaciones y la tabla Check para entender el contexto
#7. Si le sale el error (Faceting variables must have at least one value), significa que esa palabra no se correlaciona con mas de 20 palabras, revisar la tabla check solamente
#8. Categorizar las palabras según criterio en la columna [Eliminate] como {No, Tal vez, Si}
#9. Categorizar las palabras según criterio en la columna [CAT_BIEN_SERVICIO]

#### Letras de categorizacion tech o no tech
#Letras asignadas
#A-C Andrés Emilio Martínez Landaverde
#D-F Pablo Argueta Juárez
#G-I Fabio Darío Zarza Vera 
#J-L Juan Pablo Espinoza Cabrera
#M-O Laura Rebeca Monge Alvarado
#P-R Franco Pérez
#S–U Rubén Ramírez
#V-Z Vladimir Eduardo Luna


####Load data####
load("SIACSICOP1519.Rda")
spanish_stop_words <- readRDS("spanish_stop_words.Rds")
reduccion <- SIACSICOP1519 %>%
  filter(tech == "Si",
         is.na(CAT_BIEN_SERVICIO)) 
#### Generación de Corpus ####
corpus <- reduccion %>%
  mutate(text = gsub(x = DESC_BIEN_SERVICIO, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>%
  select(text) %>%
  mutate(id = as.character(row_number())) %>%
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
word_1 ="adobe"
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
Check <- reduccion[grepl(word_1, reduccion$DESC_BIEN_SERVICIO, perl = TRUE),] # check words in database
Check  <- Check %>% select(DESC_BIEN_SERVICIO, tech)

