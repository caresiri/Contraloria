


### Adding libraries ###
#library("dplyr") 
#library(data.table) # for the use of %like%
#library(stringr)


SIACSICOP1519 <- SIACSICOP1519 %>% 
  mutate(DESC = DESC_BIEN_SERVICIO)

#### Load human classification data ####
# classifications <- read.csv("/Users/ruben/Documents/Docs/INCAE/Work & Study/Text Mining/d_Ruben.csv") # read the d.csv file with "si, no, tal vez"
# places <- read.csv("/Users/ruben/Documents/Docs/INCAE/Work & Study/Text Mining/lugares.csv")

# keep only places to recognice and cleaning #
places <- places %>% filter(places$Place != "" & places$Useful == 1)
places <- places %>% mutate(cPlace = str_replace_all(places$Place," ",""))
places <- places %>% mutate(singleWord = ifelse(str_count(places$Place," ") > 0,0,1))


places_space <- places %>% filter(singleWord == 0)

n <- as.integer(count(places_space))

for (i in 1:n) {
  SIACSICOP1519 <- SIACSICOP1519 %>% 
    mutate(DESC = str_replace_all(
      DESC, 
      regex(as.character(places_space$Place[i]), ignore_case = TRUE),
      as.character(places_space$cPlace[i])))
}

# SIACSICOP1519 %>% filter(id == 4591) %>% select(DESC)


# cleaning different identifiers #
palabras_unicas %>%
distinct(eliminate)

palabras_unicas$eliminate <- ifelse(palabras_unicas$eliminate == "Tal_Vez", "Talvez", as.character(palabras_unicas$eliminate))
palabras_unicas$eliminate <- ifelse(palabras_unicas$eliminate == "Tal_vez", "Talvez", as.character(palabras_unicas$eliminate))
palabras_unicas$eliminate <- ifelse(palabras_unicas$eliminate == "Tal Vez", "Talvez", as.character(palabras_unicas$eliminate))
palabras_unicas$eliminate <- ifelse(palabras_unicas$eliminate == "Tal vez", "Talvez", as.character(palabras_unicas$eliminate))
palabras_unicas$eliminate <- ifelse(palabras_unicas$eliminate == "No sé", "No se", as.character(palabras_unicas$eliminate))
palabras_unicas$eliminate <- ifelse(palabras_unicas$eliminate == "No e", "No se", as.character(palabras_unicas$eliminate))
palabras_unicas$eliminate <- ifelse(palabras_unicas$eliminate == "No_Se", "No se", as.character(palabras_unicas$eliminate))
palabras_unicas$eliminate <- ifelse(palabras_unicas$eliminate == "No_se", "No se", as.character(palabras_unicas$eliminate))


### ID for SIACSICOP ###

SIACSICOP1519 <- SIACSICOP1519 %>% 
  mutate(id = row_number())

#### Generación de Corpus ####
new_corpus<- SIACSICOP1519 %>%
  mutate(text = DESC) %>%
  select(id, DESC_BIEN_SERVICIO, DESC, text) %>%
  unnest_tokens(word, text)

new_corpus <- new_corpus %>% 
  group_by(id) %>% 
  mutate(id_sentence = row_number())

### adding classifiers ###

palabras_unicas <- palabras_unicas %>% group_by(word) %>% summarise(eliminate = max(eliminate))

new_corpus <- 
  merge(new_corpus,palabras_unicas, by.x = "word", by.y = "word", all = TRUE) %>%
  select(`id`, id_sentence, DESC_BIEN_SERVICIO, DESC,`word`,`eliminate`)

new_corpus <- distinct(new_corpus, .keep_all = TRUE)
new_corpus$classification = NA

new_corpus$classification <- 
  ifelse(!is.null(new_corpus$eliminate) | !is.na(new_corpus$classification), 
         as.character(new_corpus$eliminate), as.character(new_corpus$classification))


# adding spanish words #

new_corpus <- merge(new_corpus,spanish_stop_words, by.x = "word", by.y = "word", all = TRUE)

new_corpus$classification <- 
  ifelse(is.null(new_corpus$classification) | is.na(new_corpus$classification) | new_corpus$classification == "", 
         as.character(new_corpus$lexicon), as.character(new_corpus$classification))


# classifiying numeric expressions #

new_corpus$classification <- 
  ifelse(grepl("^[0-9]$", new_corpus$word), 
         as.character("Numeric"), as.character(new_corpus$classification))


# classifiying places expressions #

new_corpus$upperWord <- toupper(new_corpus$word)
places$upperPlace<- toupper(places$cPlace)

new_corpus <- 
  merge(new_corpus, places, by.x = "upperWord", by.y = "upperPlace", all = TRUE) %>%
  select(`id`, DESC_BIEN_SERVICIO, DESC,  id_sentence, `word`, `classification`, `cPlace`)

new_corpus <- distinct(new_corpus, .keep_all = TRUE)

new_corpus$classification <- 
  ifelse(
    (is.null(new_corpus$classification) | is.na(new_corpus$classification) | new_corpus$classification == "") &
      !is.na(new_corpus$cPlace), 
    'Place', as.character(new_corpus$classification))



# new_corpus %>% filter(id == 13)

## Statistics Generation

base_stats <-
  new_corpus %>%
  count(id, name = "total") 

stats_class <-
  new_corpus %>%
  count(id, classification, sort = TRUE) 

base_stats <-
  merge(base_stats, stats_class, by.x = "id", by.y = "id", all = TRUE)

base_stats$weight <- base_stats$n / base_stats$total



## Adding the Tech percentage

No_errase <- base_stats %>% filter(classification == "No")

SIACSICOP1519 <- full_join(SIACSICOP1519,No_errase)

No_errase <- NULL

SIACSICOP1519$IsTech <- 
  as.double(ifelse(
    (is.null(SIACSICOP1519$weight) | is.na(SIACSICOP1519$weight) | SIACSICOP1519$weight == ""), 
    0, SIACSICOP1519$weight))

SIACSICOP1519$weight <- NULL
SIACSICOP1519$total <- NULL
SIACSICOP1519$classification <- NULL
SIACSICOP1519$n <- NULL

## Adding the Not Tech percentage

Do_errase <- base_stats %>% filter(classification == "Si")

SIACSICOP1519 <- full_join(SIACSICOP1519,Do_errase)

Do_errase <- NULL

SIACSICOP1519$NoTech <- 
  as.double(ifelse(
    (is.null(SIACSICOP1519$weight) | is.na(SIACSICOP1519$weight) | SIACSICOP1519$weight == ""), 
    0, SIACSICOP1519$weight))

SIACSICOP1519$weight <- NULL
SIACSICOP1519$total <- NULL
SIACSICOP1519$classification <- NULL
SIACSICOP1519$n <- NULL


## sumar las correlaciones de NO tech ---------------------------------------------------------

## sumar columna desconocido 

## revisar numerics que no esta clasificando 

## Others 

SIACSICOP1519$OtherClass <- (1 - (SIACSICOP1519$IsTech + SIACSICOP1519$NoTech))

## Classification on weights

SIACSICOP1519$tech <- 
  ifelse(
    SIACSICOP1519$NoTech > 0, 
    "No", "No_Se")

SIACSICOP1519$tech <- 
  ifelse(
    SIACSICOP1519$IsTech > SIACSICOP1519$NoTech, 
    "Si", SIACSICOP1519$tech)

## Full stats

full_stats_class <-
  SIACSICOP1519 %>%
  count(tech, sort = TRUE) 

conteo <- count(SIACSICOP1519)
full_stats_class$percentage <- as.numeric(conteo)

full_stats_class$percentage <- as.numeric(full_stats_class$n / full_stats_class$percentage)

full_stats_class

## Examples

base_stats %>% 
  filter(id == 9)

new_corpus %>% filter(classification == "No")

new_corpus[order(new_corpus$id_sentence) , ] %>% 
  filter(id == 9)

write_xlsx(full_stats_class,"Stats.xlsx")

