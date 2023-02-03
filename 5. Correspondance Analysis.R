library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(corrplot)
library(FactoInvestigate)
library(Factoshiny)
library(factoextra)
library(ggcorrplot)
load("wordnumperyear.Rda")

# CA per year ----
correspondance.matrix <- wordnumperyear %>% 
  filter(n > 100) %>% 
  spread(words, n) %>%
  tibble::column_to_rownames("Year")

correspondance.matrix[is.na(correspondance.matrix)] <- 0

CA <- CA(correspondance.matrix, ncp = 2)

# Correlation matrix ----
CA$col$cos2[ ,-3:-5] %>%
  .[order(.[,2], decreasing=TRUE),] %>% 
  .[.[,1]>=0.3|.[,2]>=0.3,  ] %>% 
  corrplot() 

# Just the 1st dimension
pdf("Dim1.aw.pdf")
Dim1 <- CA$col$cos2[ ,1] %>%
  .[order(., decreasing=TRUE)] %>% 
  .[.>=0.3] %>% 
  .[1:20] %>% 
  as.matrix()
colnames(Dim1) <- "Dim 1"
Dim1 %>% 
  ggcorrplot(lab = TRUE, lab_size = 2.5)
dev.off()

## Just the 2nd dimension ----
pdf("Dim2.aw.pdf")
Dim2 <- CA$col$cos2[, 2] %>%
  .[order(., decreasing=TRUE)] %>% 
  .[. >=0.3] %>% 
  as.matrix()
colnames(Dim2) <- "Dim 2"
Dim2 %>% 
  ggcorrplot(lab = TRUE, lab_size = 2.5) 
dev.off()
  
fviz_ca_biplot(CA)
fviz_ca_row(CA)
fviz_ca_col(CA)
correspondance.matrix$dim1 <- CA$svd$U[,1]

fviz_nbclust(correspondance.matrix, kmeans, method = "gap_stat")
fviz_nbclust(correspondance.matrix, hcut, method = "gap_stat")

# Hierarchical Cluster ----
HCPC <- HCPC(CA, nb.clust = 5)

# Which documents are present in topic NºX ----
clusters <- HCPC$data.clust %>% 
  select(clust) %>% 
  tibble::rownames_to_column("Year")

merge(wordnumperyear, clusters) %>% 
  group_by(clust, words) %>% 
  summarize(n = sum(n)) %>% 
  arrange(desc(n)) %>% 
  bind_tf_idf(words, clust, n) %>% 
  slice_max(tf_idf , n = 20) %>% 
  ggplot(aes(tf_idf, reorder_within(words, tf_idf, clust))) +
  geom_col() +
  facet_wrap(vars(clust), scales = "free") +
  scale_y_reordered()
 

# Exploring within the clusters ----
# Tableau de données with an additional column with the cluster nº
HCPC$data.clust

# Which are the documents the most important in each cluster? ----
HCPC$desc.ind

# Which are the words the most frequent in each cluster? ----
HCPC$desc.var
HCPC$desc.var$`4` %>% 
  as.data.frame() %>% 
  filter(`Intern freq` > 0)


# Description des cluster par les dimensions
HCPC$desc.axes


# Résumé statistique
HCPC$call
# Which documents are present in topic NºX ----
HCPC$data.clust %>% 
  filter(clust == 1) %>% 
  rownames()