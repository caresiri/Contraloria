Crowdsourcing Public Value
================

``` r
list.of.packages <- c("dplyr", "tidytext", "tidyr", "widyr","ggplot2", "readxl", "easycsv", "NLP")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidytext)
library(tidyr)
library(widyr)
library(ggplot2)
library(readxl)
library(writexl)
library(easycsv)
```

    ## Loading required package: data.table

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
library(NLP)
```

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

``` r
library(stringr)
setwd(easycsv::choose_dir()) ## seleccione el directorio con el que va a trabajar
load("SIACSICOP1519.Rda")
spanish_stop_words <- readRDS("spanish_stop_words.Rds")
palabras_unicas <- read_excel("text_mining.xlsx", sheet = "revisadas")
correlaciones <- read_excel("text_mining.xlsx", sheet = "correlaciones")
places <- read_excel("Places.xlsx")
Stats <- read_excel("Stats.xlsx")
```

\<\<\<\<\<\<\< HEAD

``` r
####Load data####
spanish_stop_words <- readRDS("spanish_stop_words.Rds")
reduccion <- SIACSICOP1519
reduccion$DESC_BIEN_SERVICIO <- tolower(reduccion$DESC_BIEN_SERVICIO)
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
word_1 ="caja"
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
```

## Text-Mining

\#\#\#Categorización de palabras

El proceso de categorización de palabras fue el siguiente:

  - El proceso inicia por desagregar cada palabra de una descripción de
    servicio en palabras individuales con un número correlativo que las
    identifica.

  - Posteriormente se realiza un trabajo de clasificación para cada
    palabra. Se relacionan bases de datos de términos que son
    reconocidos como palabras asociadas a tecnología que \[No\] se
    eliminan de la base datos, otras que \[Si\] se eliminan y otras que
    \[Talvez\] se pueden eliminar pero no se tiene un criterio
    definitivo aún. Otras palabras se clasifican como numéricas o
    palabras genéricas del idioma castellano.

  - Además, se identifican palabras que se encuentran en una base de
    datos de provincias, cantones y distritos. Estas palabras carecen de
    sentido para el análisis y se tratan como lugares.

  - Ya con la clasificación de las palabras que componen una descripción
    del bien o servicio que se desea adquirir. Para cada línea se genera
    el porcentaje de palabras como Tecnológico, No Tecnológico y Otros.

<!-- end list -->

``` r
head(Stats)   
```

    ## # A tibble: 3 x 3
    ##   tech       n percentage
    ##   <chr>  <dbl>      <dbl>
    ## 1 Si    106439      0.533
    ## 2 No     50937      0.255
    ## 3 No_Se  42212      0.211
