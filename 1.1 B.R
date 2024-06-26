# 2ndA, we do the tokenisation of the corpus WITHOUT PUNCTUATION and the 2nd layer ----
# of cleaning (removing fragment of words, pseudowords and acronyms 
tokenised.no.punct <- data.clean.no.punct %>% 
  unnest_tokens("words", "text", token = "regex", pattern = "\\s+") %>% 
  filter(!(words %in% c("york", "rockefeller", 1913:2013, 1:10, "rf", "univ", "Rf",
                        "c.m", "dr", "cent", "m.d", "mns", "ph.d", "tion", "par",
                        "agric", "inst", "ing", "ap", "pre", "r.r", "pro",
                        "ooo", "d.c", "san", "coll", "tions", "dc", "acad", 
                        "cont", "mex", "ry", "ser", "cap", "med", "natl", "phd",
                        "med", "meph", "viii", "ix", "x", "xi", "xii", "xiii", "xiv", "xv", 
                        "xvi","xvii", "xviii", "xix", "xx", "xxi", "xxii", "xxiii",
                        "mada", "bmr", "ingagr", "annual","report","ny", 
                        "wwwrockefellerfoundationorgwhat", "appointed", "programs", 
                        "table", "instalments", "remaining", "fig", "hc", "Ih", "|h", 
                        "lh", "percentage", "dh", "pc", "appropria", "mtg", "cort", 
                        "starr", "stoll", "vincent", "ova", "rr", "cum", "cons", "amino",
                        "rfs", "min", "sas", "xn", "los", 
                        "bsc", "shss", "qqq", "qqq", "qoo", "los", "iads", "nev", 
                        "inclen", "foi", "por", "Its", "llc", "rodin", 
                        "Ilc", "greene", "buttrick", "wickliffe", 
                        "ds", "dme", "apr", "barnard", "schillings", "vm", "xin", 
                        "neu", "sheldon", "pfeiffer", "gilardoni","ment", "ments", 
                        "opment", "sion", "tation", "ization", "struction", 
                        "istration", "velopment", "ation", "istration", "lation", 
                        "ration", "zation", "duction", "ganization", "mation",
                        "mentation", "mentation", "provement", "augment", "agement", 
                        "vention", "eration", "fection", "propriation",  "versity", 
                        "ments", "sity", "cation", "dation", "partment", "tration",
                        "sions", "munity", "ness", "ation", "ity","istration",
                        "lation", "divi", "nal", "popul", "educ", "tion", "ration", 
                        "ities","mention", "disserta", "tee", "ro", "cm", "per", 
                        "ref", "vn", "ls", "ga", "g", "c", "h", "dvm", "ms", 
                        "mrs", "w", "ca", "http", "ns", "cd", "pp", "investiga",  
                        "shall","ok", "forl", "backi", "fas", "fss", "appro", 
                        "foundationorg", "smns")))
# TODO include at this level what we have cleaned with the POS.background
save(tokenised.no.punct, file = "tokenised.no.punct.Rda")

# 2ndB, we do the tokenisation of the corpus WITH PUNCTUATION and the 2nd layer ----
# of cleaning (removing fragment of words, pseudowords and acronyms
tokenised.punct <- data.clean.punct %>% 
  unnest_tokens("words", "text", token = "regex", pattern = "\\s+") %>% 
  filter(!(words %in% c("york", "rockefeller", 1913:2013, 1:10, "rf", "univ", "Rf",
                        "c.m", "dr", "cent", "m.d", "mns", "ph.d", "tion", "par",
                        "agric", "inst", "ing", "ap", "pre", "r.r", "pro",
                        "ooo", "d.c", "san", "coll", "tions", "dc", "acad", 
                        "cont", "mex", "ry", "ser", "cap", "med", "natl", "phd",
                        "med", "meph", "viii", "ix", "x", "xi", "xii", "xiii", "xiv", "xv", 
                        "xvi","xvii", "xviii", "xix", "xx", "xxi", "xxii", "xxiii",
                        "mada", "bmr", "ingagr", "annual","report","ny", 
                        "wwwrockefellerfoundationorgwhat", "appointed", "programs", 
                        "table", "instalments", "remaining", "fig", "hc", "Ih", "|h", 
                        "lh", "percentage", "dh", "pc", "appropria", "mtg", "cort", 
                        "starr", "stoll", "vincent", "ova", "rr", "cum", "cons", "amino",
                        "rfs", "min", "sas", "xn", "los", 
                        "bsc", "shss", "qqq", "qqq", "qoo", "los", "iads", "nev", 
                        "inclen", "foi", "por", "Its", "llc", "rodin", 
                        "Ilc", "greene", "buttrick", "wickliffe", 
                        "ds", "dme", "apr", "barnard", "schillings", "vm", "xin", 
                        "neu", "sheldon", "pfeiffer", "gilardoni","ment", "ments", 
                        "opment", "sion", "tation", "ization", "struction", 
                        "istration", "velopment", "ation", "istration", "lation", 
                        "ration", "zation", "duction", "ganization", "mation",
                        "mentation", "mentation", "provement", "augment", "agement", 
                        "vention", "eration", "fection", "propriation",  "versity", 
                        "ments", "sity", "cation", "dation", "partment", "tration",
                        "sions", "munity", "ness", "ation", "ity","istration",
                        "lation", "divi", "nal", "popul", "educ", "tion", "ration", 
                        "ities","mention", "disserta", "tee", "ro", "cm", "per", 
                        "ref", "vn", "ls", "ga", "g", "c", "h", "dvm", "ms", 
                        "mrs", "w", "ca", "http", "ns", "cd", "pp", "investiga",  
                        "shall","ok", "forl", "backi", "fas", "fss", "appro", 
                        "foundationorg", "smns")))
save(tokenised.punct, file = "tokenised.punct.Rda")

# Frequencies ----
## Count nº of "and" & "the" 
freq.million <- tokenised.punct %>%
  group_by(Year) %>%
  count(words) %>% 
  mutate(freqmill =((n/sum(n))*1000000)) %>% 
  ungroup()

freq.million.no.punct <- tokenised.no.punct %>%
  group_by(Year) %>%
  count(words) %>% 
  mutate(freqmill =((n/sum(n))*1000000)) %>% 
  ungroup()

freq.million %>% 
  filter(words %in% c("and", "the"))

# Frequency per million for "and" & "the" per Year ----
freq.million.and <- freq.million %>% 
  filter(words %in% c("and")) %>% 
  summarise(freq.million.and = mean(freqmill)) 

freq.million.the <- freq.million %>% 
  filter(words %in% c("the")) %>% 
  summarise(freq.million.and = mean(freqmill)) 

save(freq.million, freq.million.and, freq.million.the, file = "freq.million.Rda")

# 3rd Cleaning stopwords ----
tokenised.no.punct.nsw <- anti_join(tokenised.no.punct, stop)
save(tokenised.no.punct.nsw, file = "tokenised.no.punct.nsw.Rda")