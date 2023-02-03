library(udpipe)
library(furrr)
library(spacyr)
library(quanteda)
library(tidyr)
library(dplyr)
library(stringr)
library(udpipe)
library(future)

# Loading the data packages ----
# load("data.clean.Rda")
load("tokenised.clean.Rda")

pos.clean <- tokenised.clean %>% 
  group_by(doc_id) %>% 
  summarise(words.per.document = paste(words, collapse = " "))


udpipe_download_model("english-ewt")

# Mandatory Step for the POS
pos.model <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# Mandatory step for the POS
ftest <- function(text) {
  res <- udpipe(x = text, "english-ewt", model_dir ="/Users/ivanlorenci/Desktop/R Stuff/RProjects/Annual Reports") %>% 
    as.data.frame() %>% 
    select(token,upos)
  
  return(res)
}

# Using all the computer power ----
plan(multisession, workers = 4)

# POS for the whole corpus ----
POS.Rockefeller <- data.clean %>% 
  mutate(text = str_squish(text)) %>% 
  mutate(tag = future_map(text,ftest)) %>% 
  unnest(tag)

save(POS.Rockefeller, file = "POS.Rockefeller.Rda")

# POS for a given year ----
POS.Rockefeller <- data.clean %>%
  filter(row_number() %in% seq(1,100, 5)) %>%
  mutate(text = str_squish(text)) %>%
  mutate(tag = future_map(text,ftest)) %>%
  unnest(tag)

# POS with unnest sentences ----
POS.Rockefeller <- data.clean %>%
  unnest_sentences("text", text)
  filter(row_number() %in% seq(1,100, 5)) %>%
  mutate(text = str_squish(text)) %>%
  mutate(tag = future_map(text,ftest)) %>%
  unnest(tag)

# POS for a given period ----
POS.Rockefeller <- data.clean %>%
  filter(row_number() %in% 50:55) %>%
  mutate(text = str_squish(text)) %>%
  mutate(tag = future_map(text,ftest)) %>%
  unnest(tag)


# View the most numerous "ADJ" in the whole corpus ----
POS.Rockefeller %>%
  filter(upos == "ADJ") %>%
  count(token, sort=TRUE) %>%  print(n=25)

POS.Rockefeller %>%
  count(upos, sort = TRUE)


corpus <- corpus(data.clean)

new.corpus <- summary(corpus)

save(new.corpus, file = "new.corpus.for.POS.Rda")

# POS in a given year ----
POS.Rockefeller %>% 
  filter(doc_id == "AR-1965", upos == "ADJ") %>% 
  count(token, sort = TRUE)

# POS in a given period
POS.Rockefeller.period <- POS.Rockefeller %>% 
  mutate(year = str_extract(doc_id, "\\d{4}"), 
         year = as.numeric(year),
         period = cut(year,c(0, 1919, 1929, 1939, 1945, 1955, 1965, 1975, 1985,
                             1995, 2005, 2014), 
                      labels = c("1913-1919", "1920-1929", "1930-1939", "1940-1945",
                                 "1946-1955", "1956-1965", "1966-1975", "1976-1985",
                                 "1986-1995", "1996-2005", "2006-2013"))) %>% 
  group_by(period, token, upos) %>% 
  summarise(n=sum(n))
  
POS.Rockefeller.period %>% 
  ungroup() %>% 
  group_by(period) %>% 
  filter(period %in% c("1913-1919", "1986-1995","2006-2013"), upos == "ADJ") %>% 
  slice_max(n,n=20) %>% 
  arrange(period, desc(n)) %>%  
  ggplot(aes(n, reorder_within(token,n , period)))+
  geom_col()+
  facet_wrap(vars(period), scales = "free")+
  scale_y_reordered()+
  labs(y="Top 20 ADJ")


