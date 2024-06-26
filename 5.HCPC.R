library(pacman)
p_load(FactoMineR, factoextra, tidiverse, tidyr, corrplot, Factoinvestigate, Factoshiny, ggcorrplot)

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


# Dendogram 
fviz_dend(HCPC, show_labels = FALSE)
# Individuals factor map
fviz_cluster(HCPC, geom = "point", main = "Factor map")

# why are not working
# HCPC.NOUNS$desc.var$test.chi2
# HCPC.NOUNS$desc.var$category
# HCPC.NOUNS$desc.axes


# Which are the most representative reports of the clusters? ----
HCPC$desc.ind$para$`1`
HCPC$desc.ind$dist$`1`

HCPC$desc.ind$para$`2`
HCPC$desc.ind$dist$`2`

HCPC$desc.ind$para$`3`
HCPC$desc.ind$dist$`3`

HCPC$desc.ind$para$`4`
HCPC$desc.ind$dist$`4`

HCPC$desc.ind$para$`5`
HCPC$desc.ind$dist$`5`

## Which are the reports for the X clust? ----
HCPC$data.clust %>% 
  tibble::rownames_to_column("Year") %>% 
  group_by(clust) %>% distinct(Year) %>% 
  filter(clust == 4) %>% 
  print(n= 51)


# Checking the words over and under-represented ----
clus1 <- HCPC$desc.var$`1`
clus1[c(1:20, 169:189),]
clus2 <- HCPC$desc.var$`2`
clus2[c(1:20, 179:199),]
clus3 <- HCPC$desc.var$`3`
clus3[c(1:30, 179:199),]
clus4 <- HCPC$desc.var$`4`
clus4[c(1:20, 182:202),]
clus5 <- HCPC$desc.var$`5`
clus5[c(1:20, 147:167),]

HCPC$desc.ind$para
HCPC$desc.ind$dist
HCPC$call

