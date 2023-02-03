library(widyr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidylo)
library(ggraph)
library(tidygraph)

load("POS.Rockefeller.Rda")

word_cors <-tokenised.no.punct %>% 
  count(Year, words) %>% 
  filter(n >= 50) %>% 
  select(-n) %>% 
  pairwise_cor(words, Year, sort = TRUE)

word_cors %>%
  filter(item1 == "development")


word_cors %>%
  filter(item1 == "well-being")


facet_bar <- function(df, y, x, by, nrow = 2, ncol = 2, scales = "free") {
  mapping <- aes(y = reorder_within({{ y }}, {{ x }}, {{ by }}), 
                 x = {{ x }}, 
                 fill = {{ by }})
  
  facet <- facet_wrap(vars({{ by }}), 
                      nrow = nrow, 
                      ncol = ncol,
                      scales = scales) 
  
  ggplot(df, mapping = mapping) + 
    geom_col(show.legend = FALSE) + 
    scale_y_reordered() + 
    facet + 
    ylab("")
} 

word_cors %>%
  filter(item1 %in% c("development", "well-being", "economic", "worms")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  facet_bar(y = item2, x = correlation, by = item1)


word_cors %>%
  filter(correlation > .30) %>%
  slice_max(correlation, n = 25) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE)
