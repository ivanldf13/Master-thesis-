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
  res <- udpipe(x = text, "english-ewt") %>%
    as.data.frame() %>%
    select(token, lemma, upos)
  return(res)
}

# , model_dir = "~/Desktop/R Stuff/Master-thesis-"
# TODO try to save pos.model, load it 

# POS tagging ----
POS.Rockefeller_tok_punct <- dc.p.final %>%
  # slice(11:20) %>% # to check whether it works, select just the third row
  unnest_sentences("text", text) %>% 
  mutate(text = str_squish(text)) %>%
  mutate(tag = future_map(text, ftest, .progress = TRUE)) %>%
  unnest(tag) %>% 
  group_by(Year) %>% 
  count(token, upos) %>% 
  ungroup() 

save(POS.Rockefeller_tok_punct, file = "POS.Rockefeller_tok_punct.Rda")

POS.Rockefeller_lemmatised_punct <- dc.p.final %>%
  unnest_sentences("text", text) %>% 
  mutate(text = str_squish(text)) %>%
  mutate(tag = future_map(text, ftest)) %>%
  unnest(tag) %>% 
  group_by(Year) %>% 
  count(lemma, upos) %>% 
  ungroup() 

save(POS.Rockefeller_lemmatised_punct, file = "POS.Rockefeller_lemmatised_punct.Rda")

# TODO If I use in the final version the lemma, bear in mind to mention it¡¡¡

# POS without punctuation
POS.Rockefeller_tok_no_punct <- POS.Rockefeller_tok_punct %>% 
  filter(upos != "PUNCT",
         !token %in% c("mr", "tures", "c.", "co.", "u", "ry", "p.", "p.", "univ.",
                       "rf", "v.", "fig", "r.f", "m.s.", "t", "s.", "\\-a", "st", 
                       "ss", "ls", "md", "m.d.", "inc.", "par", "pp.", "b."))
# TODO I: put here the list of the bankspeak doc

save(POS.Rockefeller_tok_no_punct, file = "POS.Rockefeller_tok_no_punct.Rda")


# TODO wouldn't be better to do the filter with the file without the punctuation?
# Selecting only the nouns
POS.NOUNS <- POS.Rockefeller_tok_punct %>%
  filter(upos == "NOUN") %>% 
  select(-upos) %>% 
  group_by(Year) %>% 
  top_n(n = 500, n) %>% 
  ungroup()

save(POS.NOUNS, file = "POS.NOUNS.Rda")

# Selecting nouns & verbs
POS.NOUNS_VERBS <- POS.Rockefeller_tok_punct %>%
  filter(upos %in% c("NOUN", "VERB")) %>% 
  select(-upos) %>% 
  group_by(Year) %>% 
  top_n(n = 500, n) %>% 
  ungroup()

save(POS.NOUNS_VERBS, file = "POS.NOUNS_VERBS.Rda")

toc()