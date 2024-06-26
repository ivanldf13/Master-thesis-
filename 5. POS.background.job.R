# Loading the needed packages ----
library(pacman)
p_load(udpipe, furrr, spacyr, quanteda, tidyr, dplyr, stringr, future, tidytext, tictoc)

tic()

# Loading the needed files ----
load("dc.p.final.Rda")

# Mandatory Step for the POS ----
udpipe_download_model("english-ewt")

plan(multisession, workers = 4) 
pos.model <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
# TODO Explain what pos.model is for


# POS with lemmanization ----
ftest <- function(text) {
  res <- udpipe(x = text, "english-ewt", pos.model) %>%
    as.data.frame() %>%
    select(token, lemma, upos)
  return(res)
}

# POS tagging ----
POS.Rockefeller_tok_punct <- dc.p.final %>%
  unnest_sentences("text", text) %>% 
  mutate(text = str_squish(text)) %>%
  mutate(tag = future_map(text, ftest)) %>%
  unnest(tag) %>% 
  group_by(Year) %>% 
  count(token, upos) %>% 
  ungroup() 

save(POS.Rockefeller_tok_punct, file = "POS.Rockefeller_tok_punct.Rda")

POS.Rockefeller_punct_lemmatised <- dc.p.final %>%
  unnest_sentences("text", text) %>% 
  mutate(text = str_squish(text)) %>%
  mutate(tag = future_map(text, ftest)) %>%
  unnest(tag) %>% 
  group_by(Year) %>% 
  count(lemma, upos) %>% 
  ungroup() 

save(POS.Rockefeller_punct_lemmatised, file = "POS.Rockefeller_punct_lemmatised.Rda")

# TODO If I use in the final version the lemma, bear in mind to mention it¡¡¡

# POS without punctuation
POS.Rockefeller.no.punct <- POS.Rockefeller %>% 
  filter(upos != "PUNCT",
         !token %in% c("mr", "tures", "c.", "co.", "u", "ry", "p.", "p.", "univ.",
                       "rf", "v.", "fig", "r.f", "m.s.", "t", "s.", "\\-a", "st", 
                       "ss", "ls", "md", "m.d.", "inc.", "par", "pp.", "b."))
# TODO I: put here the list of the bankspeak doc

save(POS.Rockefeller.no.punct, file = "POS.Rockefeller.Rda")

# Selecting only the nouns
POS.NOUNS <- POS.Rockefeller_tok_punct %>%
  filter(upos == "NOUN") %>% 
  select(-upos) %>% 
  group_by(Year) %>% 
  top_n(n = 500, n) %>% 
  ungroup()

save(POS.NOUNS, file = "POS.NOUNS.Rda")

# Selecting nouns & verbs
POS.NOUNS <- POS.Rockefeller_tok_punct %>%
  filter(upos %in% c("NOUN", "VERB")) %>% 
  select(-upos) %>% 
  group_by(Year) %>% 
  top_n(n = 500, n) %>% 
  ungroup()

save(POS.NOUNS_VERBS, file = "POS.NOUNS_VERBS.Rda")

toc()