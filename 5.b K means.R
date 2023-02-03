library(factoextra)
library(cluster)
library(tidytext)
library(ggwordcloud)
library(dplyr)
library(tidyr)
library(RColorBrewer)
load("wordnumperyear.Rda")

# CA per year ----
correspondance.matrix <- wordnumperyear %>% 
  filter(n > 100) %>% 
  spread(words, n) %>%
  tibble::column_to_rownames("Year")

correspondance.matrix[is.na(correspondance.matrix)] <- 0

kmeans.clust <- eclust(correspondance.matrix, k = 7)
cluster.km <- kmeans.clust$cluster %>% 
  data.frame(cluster = .) %>% 
  tibble::rownames_to_column("Year")

wordnumperyear %>% 
  left_join(cluster.km) %>% 
  group_by(cluster, words) %>% 
  summarise(n = sum(n)) %>% 
  filter(cluster == 5) %>% 
  # filter(! (words %in% c("university"))) %>% 
  top_n(23, n) %>% 
  with(ggwordcloud(words,n,scale=c(4,1),
                 colors=brewer.pal(8, "Dark2"),
                 rot.per=0.35,
                 random.order=FALSE)) +
  labs(title="Wordcloud des nouveaux")
