library(quanteda)
library(dplyr)
library(tidyr)
load("dc.np.final.Rda")
load("dc.p.final.Rda")
# locate a word in the corpus
corpus <- corpus(dc.np.final, docid_field = "Year")
corpus.tokenised <- tokens(corpus)
options(max.print = 5000)
kwic(corpus.tokenised, pattern = "econo mies")

top words at any given time

, valuetype = "regex")
berlin(?=\\sdahlem)

dc.np.final %>% 
  mutate(extracted = str_extract_all(text, "berlin(?=\\sdahlem)")) %>%
  unnest(extracted) %>% 
  select(-text) %>% 
  distinct(Year)
  
# getting the number of tokens
summary(corpus) %>% 
  pull(Types) %>% 
  sum()


kwic(corpus.tokenised, pattern="erice.*$", valuetype = "regex")
str_match()
grepl(" priations", data.clean$text)

dc.np.final %>% 
  unnest_tokens("words", text) %>% 
  filter(words == "cal")

