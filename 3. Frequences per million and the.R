load("freq.million.Rda")
library(dplyr)
library(ggplot2)
library(quanteda)
library(readtext)
library(spacyr)
library(stringr)
library(tidyr)
library(tidytext)
library(tm)
library(xlsx)


# Graphic for "and"
freq.million %>% 
  filter(words %in% c("and")) %>% 
  ggplot(aes(Year, freqmill)) +
  geom_line()

freq.million %>% 
  filter(words %in% c("the")) %>%
  ggplot(aes(Year, freqmill)) +
  geom_line(aes(color = "the")) +
  labs(y = "frequency per million words", 
       color = NULL)+
  theme(legend.position = c(0.8,0.8))


tokenised.punct %>%
  group_by(Year) %>%
  count(words) %>% 
  mutate(freqmill =((n/sum(n))*1000000)) %>% 
  ungroup() %>% 
  mutate(test = str_detect(words, "wellbeing")) %>% 
  group_by(Year) %>% 
  summarise(test = sum(test))

# Resilience ----
freq.million %>% 
  filter(words %in% c("resilience")) %>% 
  ggplot(aes(Year, freqmill)) +
  geom_line(aes(color = "resilience")) +
  labs(y = "frequency per million words", 
       color = NULL)+
  theme(legend.position = c(0.8,0.8))
# Well-being ----
freq.million %>% 
  filter(words %in%  c("development")) %>% 
  ggplot(aes(Year, freqmill, color = words)) +
  geom_line() +
  labs(y = "frequency per million words", 
       color = NULL) +
  theme(legend.position = c(0.8,0.8))

# Combined graphic ----
graphic.freq.mill.and <- freq.million %>% 
  filter(words %in% c("and")) %>% 
  rename("and" = freqmill) %>%
  select(-words,-n)

graphic.freq.mill.the <- freq.million %>% 
  filter(words %in% c("the")) %>% 
  rename("the" = freqmill) %>%
  select(-words,-n)

merge(graphic.freq.mill.and, graphic.freq.mill.the) %>% 
  gather("word","freq.mill", the, and) %>% 
  ggplot(aes(Year, freq.mill, linetype = word))+
  geom_line() + 
  labs(y = "frequency per million words", 
       color = NULL)+
  theme(legend.position = c(0.8,0.8))
ggsave("freq.the.and.pdf", units = "cm", width = 26, height = 14)
