library(quanteda)
load("dc.np.final.Rda")
load("dc.p.final.Rda")
# locate a word in the corpus
corpus <- corpus(dc.np.final, docid_field = "Year")
corpus.tokenised <- tokens(corpus)
options(max.print = 5000)
kwic(corpus.tokenised, pattern = "ph.d")


kwic(corpus.tokenised, pattern="erice.*$", valuetype = "regex")
str_match()
grepl(" priations", data.clean$text)

dc.np.final %>% 
  unnest_tokens("words", text) %>% 
  filter(words == "cal")



                    
