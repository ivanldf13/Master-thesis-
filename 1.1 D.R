dc.np.final <- tokenised.no.punct.nsw %>% 
  group_by(Year) %>% 
  summarise(text = paste(words, collapse = " "))

dc.p.final <- tokenised.punct %>% 
  group_by(Year) %>% 
  summarise(text = paste(words, collapse = " "))

save(dc.np.final, file = "dc.np.final.Rda")
save(dc.p.final, file= "dc.p.final.Rda")


# Grouping by year and counting words ----
wordnumperyear <- tokenised.no.punct.nsw %>% 
  group_by(Year) %>% 
  count(words, sort=TRUE) %>% 
  top_n(n = 500, n) %>% 
  ungroup()

# To separate two words that have not been separated before
two_words <- wordnumperyear %>% 
  separate(words, c("FrstWord", "SndWord"), sep = "\\s") %>% 
  filter(!is.na(SndWord)) %>% 
  gather("order", "words", FrstWord, SndWord) %>% 
  select(Year, words, n)

wordnumperyear <- two_words %>% 
  bind_rows(wordnumperyear) %>% 
  filter(!str_detect(words, "\\s|\\-"))

save(wordnumperyear, file = "wordnumperyear.Rda")