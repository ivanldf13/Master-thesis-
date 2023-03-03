library(quanteda)
library(dplyr)
library(tidyr)
load("dc.np.final.Rda")
load("dc.p.final.Rda")
# locate a word in the corpus
corpus <- corpus(dc.np.final, docid_field = "Year")
corpus.tokenised <- tokens(corpus)
options(max.print = 5000)
kwic(corpus.tokenised, pattern = "fmns")


# , valuetype = "regex"
# berlin(?=\\sdahlem)
# TODO what to do with "psy", agriculturetural, iiinternationall, coriceptual, fundingfor, lts, ofthe
# TODO guaianalysis, carreldakin, glycerin salt, |, schoololarships,  ■, louisianalysis, sciencebased, 
# TODO of a, +, ¥, and the, artistinresidence, at risk, cdinate, climatecconscious, climaterelated
# TODO previ, profes, deem, mem,  foundationnew, rresearch, bepobt, drought tolerant, artistinresidence
# TODO put the cleaning of the sw at the end of the cleaning?
anouriced

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

