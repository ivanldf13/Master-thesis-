# Loading the needed packages ----
library(udpipe)
library(furrr)
library(spacyr)
library(quanteda)
library(tidyr)
library(dplyr)
library(stringr)
library(udpipe)
library(future)
library(tidytext)

# Setting the WD ----
# setwd("~/Desktop/R Stuff/RProjects/Annual Reports")
# Loading the needed files
load("dc.p.final.Rda")

# Mandatory Step for the POS ----
# udpipe_download_model("english-ewt")

plan(multisession, workers = 4) 
pos.model <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# ftest <- function(text) {
#   res <- udpipe(x = text, "english-ewt", model_dir ="/Users/ivanlorenci/Desktop/R Stuff/RProjects/Annual Reports") %>% 
#     as.data.frame() %>% 
#     select(token,upos)
#   return(res)
# }

# POS with lemmanization ----
ftest <- function(text) {
  res <- udpipe(x = text, "english-ewt", model_dir ="/Users/ivanlorenci/Desktop/R Stuff/RProjects/Annual Reports") %>%
    as.data.frame() %>%
    select(token, lemma, upos)
  return(res)
}



POS.Rockefeller <- dc.p.final %>%
  unnest_sentences("text", text) %>% 
  mutate(text = str_squish(text)) %>%
  mutate(tag = future_map(text, ftest)) %>%
  unnest(tag) %>% 
  group_by(Year) %>% 
  count(token, upos) %>% 
  ungroup() 

save(POS.Rockefeller, file = "POS.Rockefeller.Rda")


# POS without punctuation
POS.Rockefeller <- POS.Rockefeller %>% 
  filter(upos != "PUNCT",
         !token %in% c("mr", "tures", "c.", "co.", "u", "ry", "p.", "p.", "univ.",
                       "rf", "v.", "fig", "r.f", "m.s.", "t", "s.", "\\-a", "st", 
                       "ss", "ls", "md", "m.d.", "inc.", "par", "pp.", "b."))
# TODO I: put here the list of the bankspeak doc


# Selecting only the nouns
POS.NOUNS <- POS.Rockefeller %>%
  filter(upos == "NOUN") %>% 
  select(-upos) %>% 
  group_by(Year) %>% 
  top_n(n = 500, n) %>% 
  ungroup()

save(POS.NOUNS, file = "POS.NOUNS.Rda")


