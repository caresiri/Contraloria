Crowdsourcing Public Value
================

## Abstract

This exercise was designed as part of an ongoing collaboration between
INCAE Business School and Contraloría General de la República de Costa
Rica. It presents to the student with a concrete data challenge: As a
result of an open government policy, the Costa Rican Republic has made
its procurement data publicly available. The student is presented with
some context information, a brief introduction to a basic framework on
public value creation, a set of questions to help as a guide to
understand the underlying technical infrastructure allowing this
ability, and finally with a subset of data (over a hundred lines) of
public data. The dataset contains five years of information on
information technology that was acquired or rented using public funds.
The challenge is to propose to public officials concrete ways to add
value based on their analysis of these data.

## Objectives

  - Test and develop the basic skills for data management and
    exploratory data analysis
  - Recognize the benefits and drawbacks of being a power user as
    opposed to information consumers from predefined searches in a
    Business Intelligence platform
  - Recognize the challenges for Open Government Initiatives in terms of
    design, implementation and adoption

Extra-Curricular Objectives \* Students papers, insights and discussion
will contribute to the advancement of current efforts to promote a
data-driven discussion about open data in general and procurement in
particular, in Costa Rica and beyond. Assumptions The student has
succesfully completed a Quantitative Methods course and therefore
understands the following subjects: \* Utilizes visualization techniques
(Tableau, Rstudio) for an exploratory data analysis \* Can perform a
linear regression against dummy variables to identify significant
relationships. \* Can perform a logistic regression to evaluate
proportions when using categorical variables.

The student has critical thinking and written communication skills,
necessary for: \* Identify evidence (can recognize the difference
between working and critical assumptions provided the lack of data) \*
Evaluate Arguments \* Develop well-constructed conclusions.

## Contextual Information

Costa Rica has been considered one of the 20th more robust Democracies
in the world (The EIU, 2018). But as most of them, it is facing
important challenges as citizens demand more transparency, efficiency
and public participation.

According to the World Bank, “Open government is built on the idea that
citizens have the right to access government information, to actively
participate in government decisions that affect their livelihoods, and
to hold government officials and/or service providers to account when
they fail to govern properly (World Bank Group, 2017)”.

Public procurement, therefore, is a key element in any Open Data
initiative, but in this regard, the country is lacking citizen
involvement, even though, data has been available through Contraloría
General de la República. A recent assessment stated:

“The absence of an institutionalized mechanism that allows for the
participation of civil society in the public procurement system. In
general terms, there is little citizen participation and almost no
supervision from civil society over the contracting processes, and
little consideration of public opinion and of interested parties, both
in the design stages and in the contractual execution stages.” (Gobierno
de la República de Costa Rica, 2019)

Costa Rica is a member of the Open Government Partnership since 2012 and
its current action plan list 12 commitments to be accomplished by the
year 2019: “Poner a disposición de la ciudadanía los datos generados por
el SICOP en formato abierto, neutral e interoperable, siguiendo los
estándares de contratación abierta de Open Contracting Partnership”.
(Gobierno de la República de Costa Rica, 2019)

Data savvy MBA students, and future business leaders, can and should
play a role in Public or Private Value creation and this exercise
provide an opportunity to test knowledge, ingenuity and creativity.

## A Gentle Introduction to Public Value creation

Public value can be described in terms of six general types that capture
the range of possible results of government in the ways of interest…
(Harrison et al., 2011)

  - Financial – impacts on current or future income, asset values,
    liabilities, entitlements, or other aspects of wealth or risks to
    any of the above.
  - Political – impacts on a person’s or group’s influence on government
    actions or policy, on their role in political affairs, influence in
    political parties or prospects for public office.
  - Social – impacts on family or community relationships, social
    mobility, status, and identity.
  - Strategic – impacts on person’s or group’s economic or political
    advantage or opportunities, goals, and resources for innovation or
    planning.
  - Ideological – impacts on beliefs, moral or ethical commitments,
    alignment of government actions or policies or social outcomes with
    beliefs, or moral or ethical positions.
  - Stewardship – impacts on the public’s view of government officials
    as faithful stewards or guardians of the value of the government in
    terms of public trust, integrity, and legitimacy.

…Actions to effect transparency, participation, and collaboration belong
within this group of value generators. Taken as a whole, the set of
value generators consists of (Harrison et al., 2011):

  - efficiency – obtaining increased outputs or goal attainment with the
    same resources or obtaining the same outputs or goals with lower
    resource consumption.
  - effectiveness – increasing the quality of the desired outcome.
  - intrinsic enhancements – changing the environment or circumstances
    of a stakeholder in ways that are valued for their own sake.
  - transparency – access to information about the actions of government
    officials or operation of government programs that enhances
    accountability or influence on government.
  - participation – frequency and intensity of direct involvement in
    decision making about or operation of government programs or in
    selection of or actions of officials.
  - collaboration – frequency or duration of activities in which more
    than one set of stakeholders share responsibility or authority for
    decisions about operation, policies, or actions of government.

<!-- end list -->

``` r
setwd("./Tablas")
library(readxl)
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
Adj_2015 <- read_excel("Reporte adjudicaciones x codigo catalogo 2015.xlsx")
Adj_2016 <- read_excel("Reporte adjudicaciones x codigo catalogo 2016.xlsx")
Adj_2017 <- read_excel("Reporte adjudicaciones x codigo catalogo 2017.xlsx")
Adj_2018 <- read_excel("Reporte adjudicacioenes x codigo catalogo 2018.xlsx")
Adj_2019 <- read_excel("Reporte adjudicacioenes x codigo catalogo 2019.xlsx")


Of_2015 <- read_excel("Reporte ofertas presentadas x codigo catalogo 2015.xlsx")
Of_2016 <- read_excel("Reporte ofertas presentadas x codigo catalogo 2016.xlsx")
Of_2017 <- read_excel("Reporte ofertas presentadas x codigo catalogo 2017.xlsx")
Of_2018_19 <- read_excel("Reporte ofertas x codigo catalogo 2018-2019.xlsx")

SIAC_15_17 <- read_excel("SIAC 2015-2017.xlsx")
SIAC_18_19 <- read_excel("ComprasSIAC.xls")

SIAC_18_19$`Año de adjudicación` <- as.numeric(SIAC_18_19$`Año de adjudicación`)

Adj_Total <- bind_rows(Adj_2015, Adj_2016, Adj_2017, Adj_2018, Adj_2019)
Of_Total <- bind_rows(Of_2015, Of_2016, Of_2017, Of_2018_19)
SIAC_Total <-bind_rows(SIAC_15_17,SIAC_18_19)

###Prepare for join 
SIAC_Total$`Clave de la línea del procedimiento`<- substr(SIAC_Total$`Clave de la línea del procedimiento`,1,10)
SIAC_Total$OBJ_GASTO <- substr(SIAC_Total$`Subpartida (COG)(AC)`,1,7)
SIAC_Total$`Subpartida (COG)(AC)`<- substr(SIAC_Total$`Subpartida (COG)(AC)`,10,100)
SIAC_Total <- SIAC_Total %>% rename(INSTITUCION = `Nombre de la entidad madre`,
                                    DESC_PROCEDIMIENTO = `Objeto contractual`,
                                    DESC_BIEN_SERVICIO = `Descripción del bien o servicio`,
                                    CEDULA_PROVEEDOR = `Cédula del adjudicatario`,
                                    NOMBRE_PROVEEDOR = `Nombre del adjudicatario`,
                                    CANTIDAD = `Cantidad licitada`,
                                    FECHA_ADJUD_FIRME = `Fecha de adjudicación`,
                                    MONTO_ADJU_CRC = `Monto adjudicados`,
                                    CED_INSTITUCION = `Clave de la línea del procedimiento`,
                                    DESC_GASTO = `Subpartida (COG)(AC)`)
SIAC_Total$CEDULA_PROVEEDOR <- as.character(SIAC_Total$CEDULA_PROVEEDOR)

Adj_Of <- bind_rows(Adj_Total, Of_Total)
Adj_Of$CAT_BIEN_SERVICIO <- ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="8111","Servicios informáticos, de computación, audio y video",
                                   ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,2)=="43","Telecomunicaciones y radiofusión de de tecnología de la información",
                                          ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="8116","Entrega de Servicios de Tecnología de Información",
                                                 ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4319","Dispositivos de comunicaciones y accesorios",
                                                        ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4320","Componentes para tecnología de la información, difusión o telecomunicaciones",
                                                               ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4321","Equipo informático y accesorios",
                                                                      ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4322","Equipos de redes de voz, multimedia, o plataformas y accesorios",
                                                                             ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4323","Software",NA))))))))


Adj_Of$'Año de adjudicación'<- as.numeric(paste("20",substr(Adj_Of$FECHA_ADJUD_FIRME,7,8), sep=""))
```

    ## Warning: NAs introduced by coercion

``` r
SIAC_Total$FECHA_ADJUD_FIRME <- as.character(SIAC_Total$FECHA_ADJUD_FIRME)

SIACSICOP1519  <-bind_rows(Adj_Of, SIAC_Total)
setwd('..')
```

``` r
list.of.packages <- c("dplyr", "tidytext", "tidyr", "widyr","ggplot2", "readxl", "easycsv", "NLP")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(dplyr)
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
<<<<<<< HEAD
library(stringr)
=======
>>>>>>> a8df5f1a89851c69c670a4869150838737036632
setwd(easycsv::choose_dir()) ## seleccione el directorio con el que va a trabajar
load("SIACSICOP1519.Rda")
spanish_stop_words <- readRDS("spanish_stop_words.Rds")
palabras_unicas <- read_excel("text_mining.xlsx", sheet = "revisadas")
correlaciones <- read_excel("text_mining.xlsx", sheet = "correlaciones")
<<<<<<< HEAD
places <- read_excel("Places.xlsx")
Stats <- read_excel("Stats.xlsx")
```

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
=======
>>>>>>> a8df5f1a89851c69c670a4869150838737036632
```

## Text-Mining

\#\#\#Categorización de palabras

El proceso de categorización de palabras fue el siguiente:
<<<<<<< HEAD

### 

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
    ##   tech      n percentage
    ##   <chr> <dbl>      <dbl>
    ## 1 Si    89445      0.448
    ## 2 No_Se 58777      0.294
    ## 3 No    51361      0.257
=======
>>>>>>> a8df5f1a89851c69c670a4869150838737036632
