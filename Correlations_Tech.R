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
word_1 ="mouse"
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

