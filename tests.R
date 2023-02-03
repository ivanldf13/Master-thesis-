text <- c("univers ity", "c ity", "ca sa", "univers ities")

str_replace_all(text, "([a-z]{1,13})\\s\\bit(y|ies)\\b", "\\1it\\2")


str_replace_all(text, "", "\\1ity\\2")


data.clean.punct$text %>% 
  str_match_all("well-?being")


data.clean.punct %>% 
  unnest_tokens("words", "text", token = "regex", pattern = "\\s+") %>% 
  filter(str_detect(words, "well-?being"))
