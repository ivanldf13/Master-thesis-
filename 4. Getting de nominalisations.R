library(dplyr)
library(stringr)
library(ggplot2)
# Getting the nominalisations
load("tokenised.no.punct.nsw.Rda")

# Counting the number of nominalisations per year ----
tokenised.no.punct.nsw %>% 
  filter(str_detect(words, "tion\\b") | str_detect(words, "tions\\b") 
         | str_detect(words, "sion\\b") | str_detect(words, "sions\\b")
         | str_detect(words, "ment\\b") | str_detect(words, "ments\\b")
         | str_detect(words, "ness\\b") | str_detect(words, "nesses\\b")
         | str_detect(words, "ity\\b")  | str_detect(words, "ities\\b")
         # ! (words %in% c("liste de mots, )
                         ) %>%
  count(Year)

# Getting the 600 most used nominalisations ----
top.600.nominalisations <- tokenised.no.punct.nsw %>% 
  filter(str_detect(words, "tion\\b") |str_detect(words, "tions\\b") 
         | str_detect(words, "sion\\b") | str_detect(words, "sions\\b")
         | str_detect(words, "ment\\b") | str_detect(words, "ments\\b")
         | str_detect(words, "ness\\b") | str_detect(words, "nesses\\b")
         | str_detect(words, "ity\\b")  | str_detect(words, "ities\\b")) %>%
  count(words, sort = TRUE) %>% 
  slice(1:600) %>% 
  # filter(!(words %in% c("thesolution"))) %>% 
  .$words

words.per.doc <- tokenised.no.punct.nsw %>% 
  count(Year, name = "frequence.per.document")
  
freq.per.million.top.600.nominalisations <- tokenised.no.punct.nsw%>% 
  filter(words %in% top.600.nominalisations) %>% 
  filter(!(words %in% c("WORDS NOT NOMINALISATIONS"))) %>% 
  count(Year) %>% 
  left_join(words.per.doc) %>% 
  mutate(freqmill =((n/frequence.per.document)*1000000))
  

save(top.600.nominalisations, freq.per.million.top.600.nominalisations, 
     file = "top.600.nominalisations&freqs.Rda")
save(words.per.doc, file = "words.per.doc.Rda")

top.600.nominalisations %>% 
  grep("zation", . , value = TRUE)

# Transforming a vector into a data frame ----


# Nominalisations graph ----
freq.per.million.top.600.nominalisations %>% 
  ggplot(aes(Year, freqmill))+
  geom_line() + 
  labs(y = "Frequency per million words")+
  theme_bw()

# Exporting the list to a .doc file ----
writeLines(top.600.nominalisations, con = "nominalisation list.txt")
