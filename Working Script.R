
library(broom)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(quanteda)
library(pdftools)
library(readtext)
library(stm)
library(stringr)
library(tidyr)
library(tidytext)
library(tidyverse)
library(tm)

#Setting wd
setwd("/Users/ivanlorenci/Desktop/R Stuff/RProjects/Annual Reports/AR.TXT")
# setwd("/Users/ivanlorenci/Desktop/AR secretary and president's review")
# Faire la liste des documents pdf à utiliser 
files <- list.files(pattern = "pdf$")
data <- readtext("./AR.TXT/*.pdf") # la buena

# # Import in text pdf files
# texts <- lapply(files, pdf_text)
# # Have a look at the first element 
# texts[[1]]

# #Build a data frame
# data <- cbind(texts)
# In a given dataframe, the name of the files will be shown
# data$name <- files

# # Have a look at the dimensions of the dataframe
# dim(data)
# Build an intelligent dataframe
# data <- tibble(data)

# Save the results
save(data, file="CorpusTry.Rda")
# save(files, texts, file = "Vestin.Rda")

# Stopwords (en)
stop <- tibble(words = stopwords::data_stopwords_stopwordsiso$en)

# Cleaning data
# Test, first clean with the most salient things to erase
data.clean  <-
  data  %>% # if data alone, all the files; [1,] only the first item; [1:10,] first ten
  mutate(doc_id = str_replace_all(doc_id, "Annual-Report-", "AR-"), 
         doc_id = str_remove_all(doc_id, ".pdf")) %>% 
  mutate(text = str_remove_all(text,"\n"),
         text = str_remove_all(text,"\\we2003 The Rockefeller Foundation"),
         text = tolower(text))

data.clean <- 
  data[1,]  %>% 
  mutate(text = tolower(text),
         text = str_remove_all(text,"\n"),
         text = str_remove_all(text,"\\we2003 the rockefeller foundation"),
         text = str_remove_all(text, "rockefeller foundation"))
         

# Ajouter plus d'expressions pour le nettoyage'

# Unnest tokens
tokenised <- data.clean %>% 
  unnest_tokens("words", "text")
  

# Cleaning stopwords
tokenised <- anti_join(tokenised,stop)

# Counting words
count(tokenised, words, sort=TRUE)
count(tokenised, words, sort=TRUE) %>%  print(n=25)

tokenised.clean <- tokenised %>% 
  filter(!(words %in% c("york", "rockefeller", 1913:2013, 1:10)))


# visualisations
tokenised.clean %>% 
  ggplot(aes())



# Importing txt texts no funciona
tbl <- list.files(pattern = "AR.TXT.txt") %>% 
  +     map_chr(~ read_file(.)) %>% 
  +     data_frame(text = .)

readLines("Annual-Report-1960-1(2).txt")
AR1961 <- readLines("Annual-Report-1960-1(2).txt")
gsub(pattern = "\\W", replace = " ", AR1961) 


gsub(pattern="\\b[2003 The Rockefeller Foundation]", replace=" ", AR1961.v1)
tolower(AR1961.v2)
gsub(pattern="\\b[A-z]\\b{1}", replace=" ", AR1961.v2)
AR1961.v2 <- gsub(pattern="\\b[2003 The Rockefeller Foundation]", replace=" ", AR1961.v1)
removeWords(AR1961.v2, c("Inc", "inc", "xxiii", "xxii", "xxv", "xxiv", "xxi", "xix"))
AR1961.v2 <- stripWhitespace(AR1961.v2)
str(AR1961.v2)
str(str_split(AR1961.v2, pattern="\\s+"))

unlist(AR1961.v2)


read_lines("Annual-Report-1960.txt")
AR1960 <- readLines("Annual-Report-1960.txt")
AR1960.v1 <- gsub(pattern = "\\W", replace = " ", AR1960) 
AR1960.v2 <- gsub(pattern="\\b[2003 The Rockefeller Foundation]", replace=" ", AR1960.v1)
AR1960.v3 <- tolower(AR1960.v2)
AR1960.v4 <- gsub(pattern="\\b[A-z]\\b{1}", replace=" ", AR1960.v3)
AR1960.v5 <- removeWords(AR1960.v4, c("Inc", "inc", "xxiii", "xxii", "xxv", "xxiv", "xxi", "xix"))
AR1960.v6 <- stripWhitespace(AR1960.v5)
AR1960.v7 <- str_split(AR1960.v6, pattern="\\s+")


length(data[1, c("text")])
