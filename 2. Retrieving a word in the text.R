library(quanteda)
library(dplyr)
library(tidyr)
load("dc.np.final.Rda")
load("dc.p.final.Rda")
# locate a word in the corpus
corpus <- corpus(dc.p.final, docid_field = "Year")
corpus.tokenised <- tokens(corpus)
options(max.print = 5000)
kwic(corpus.tokenised, pattern = "drought tolerant")


# , valuetype = "regex"
# berlin(?=\\sdahlem)
# TODO what to do with agriculturetural, ofthe, =
# TODO  schoololarships,  artistinresidence 

corpus.tokenised[[43]] %>% 
  .[str_detect(.,"\\bmem")]

dc.np.final %>% 
  mutate(extracted = str_extract_all(text, "berlin(?=\\sdahlem)")) %>%
  unnest(extracted) %>% 
  select(-text) %>% 
  distinct(Year)

# Checking
dc.np.final %>% 
  filter(Year == 1956) %>% 
  pull(text) %>% 
  str_split("\\s+|-") %>% 
  unlist() %>% 
  .[22820]

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

# REVISAR to open the R.profile
# usethis::edit_r_profile("user") 


