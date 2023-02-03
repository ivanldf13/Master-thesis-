library(spacyr)
library(tidytext)
library(dplyr)
spacy_initialize(model = "en_core_web_sm")
load("data.clean.Rda")

tokenised.sentences <- data.clean %>% 
  unnest_tokens("sentences", token = "sentences", "text") %>% 
  tibble() %>% 
  mutate(sentences = gsub("  ", "", sentences))

save(tokenised.sentences, file = "tokenised.sentence.Rda")

text <- tokenised.sentences$sentences
names(text) <- tokenised.sentences$doc_id
POS.spacyr <- spacy_parse(text)
