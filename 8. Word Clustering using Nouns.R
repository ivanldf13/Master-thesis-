# Loading the needed packages ----
library(future)
library(ggplot2)
library(dplyr)
library(shiny)
library(udpipe)
library(furrr)
library(spacyr)
library(quanteda)
library(tidyr)
library(stringr)
library(FactoMineR)
library(factoextra)
library(tidytext)
library(corrplot)
library(FactoInvestigate)
library(Factoshiny)
library(NbClust)
library(ggcorrplot)
library(kableExtra)
library(grid)
library(vcd)
library(janitor)
library(rstatix)

# Loading Files ----

load("POS.Rockefeller.Rda")
load("wordnumperyear.Rda")
load("dc.np.final.Rda")

load("POS.NOUNS.Rda")
load("CA.NOUNS.Rda")

# retirer les loads de POS.Nouns et CA.NOUNS
# View the most numerous "NOUNS" in the file ----
# POS.Rockefeller %>%
  # filter(upos == "NOUN") %>%
#   count(token, sort=TRUE) %>%  print(n=25)

POS.NOUNS <- POS.Rockefeller %>%
  filter(upos == "NOUN") %>% 
  select(-upos)

save(POS.NOUNS, file = "POS.NOUNS.Rda")

# Word clustering with just nouns ----
##CA per year 
correspondance.matrix.POS <- 
  POS.NOUNS %>%
  filter(n > 40) %>%
  spread(key = token, value = n) %>% 
  tibble::column_to_rownames("Year")

# We recode the NAs in the correspondance.matrix.POS for 0s
correspondance.matrix.POS[is.na(correspondance.matrix.POS)] <- 0

# Correspondance matrix image
knitr::kable(correspondance.matrix.POS[1:5,1:5]) %>% 
  save_kable(file = "CA_ppt.pdf")

# Looking whether there is a problem with a row ----
POS.NOUNS %>% 
  filter(Year == 2008) %>% 
  arrange(desc(n))

# Getting the names of the rows to check which AR is missing
rownames(correspondance.matrix.POS)

CA.NOUNS$row$contrib[,2] %>% 
  sort(., decreasing = TRUE) %>% 
  head(., n= 5)

# Correspondance Analysis
CA.NOUNS <- CA(correspondance.matrix.POS, ncp = 2)
save(CA.NOUNS, file = "CA.NOUNS.Rda")

# Getting the chi-square ----
chi.test <- chisq.test(correspondance.matrix.POS) 
chi.test
dim(correspondance.matrix.POS)

assocstats(correspondance.matrix.POS)

correspondance.matrix.POS %>% 
  cramer_v()

# Getting the eigen values ----
get_eigenvalue(CA.NOUNS)

pdf("eigen.values.screeplot.pdf")
fviz_eig(CA.NOUNS, addlabels = TRUE, ylim = c(0,50))
dev.off()

# Words correlated with both dimensions ----
CA.NOUNS$col$cos2[ ,-3:-5] %>%
  .[order(.[,2], decreasing=TRUE),] %>% 
  .[.[,1]>=0.3|.[,2]>=0.3,  ] %>% 
  ggcorrplot(lab = TRUE, lab_size = 2.5)

## Just the 1st dimension ----
pdf("Dim1.pdf")
Dim1 <- CA.NOUNS$col$cos2[ ,1] %>%
  .[order(., decreasing=TRUE)] %>% 
  .[.>=0.3] %>% 
  .[1:20] %>% 
  as.matrix()
colnames(Dim1) <- "Dim 1"
Dim1 %>% 
  ggcorrplot(lab = TRUE, lab_size = 2.5)
dev.off()

## Just the 2nd dimension ----
pdf("Dim2.pdf")
Dim2 <- CA.NOUNS$col$cos2[, 2] %>%
  .[order(., decreasing=TRUE)] %>% 
  .[. >=0.3] %>% 
  as.matrix()
colnames(Dim2) <- "Dim 2"
Dim2 %>% 
  ggcorrplot(lab = TRUE, lab_size = 2.5) 
dev.off()

# Plots ----
pdf("biplot.pdf")
fviz_ca_biplot(CA.NOUNS)
dev.off()

pdf("row.points.CA.pdf")
fviz_ca_row(CA.NOUNS)
dev.off()

pdf("col.points.CA.pdf")
fviz_ca_col(CA.NOUNS)
dev.off()

# correspondance.matrix.POS$dim1 <- CA.NOUNS$svd$U[,1]
# correspondance.matrix.POS$dim2 <- CA.NOUNS$svd$U[,2]

# How many clusters would be optimal? ----
fviz_nbclust(correspondance.matrix.POS, kmeans, method = "gap_stat")
fviz_nbclust(correspondance.matrix.POS, hcut, method = "gap_stat")

# Looking at cos2 of rows ----
pdf("cos2.row.dim1&2.pdf")
fviz_cos2(CA.NOUNS, choice = "row", axes = 1:2, top = 15)
dev.off()

pdf("cos2.row.dim1.pdf")
fviz_cos2(CA.NOUNS, choice = "row", axes = 1, top = 15)
dev.off()

pdf("cos2.row.dim2.pdf")
fviz_cos2(CA.NOUNS, choice = "row", axes = 2, top = 15)
dev.off()

# Looking at the contributions of rows ----
fviz_contrib(CA.NOUNS, choice = "row", axes = 1:2)

pdf("contrib.rows.dim1&2.pdf")
fviz_contrib(CA.NOUNS, choice = "row", axes = 1:2, top = 15)
dev.off()

pdf("contrib.rows.dim1.pdf")
fviz_contrib(CA.NOUNS, choice = "row", axes = 1, top = 15)
dev.off()

pdf("contrib.rows.dim2.pdf")
fviz_contrib(CA.NOUNS, choice = "row", axes = 2, top = 15)
dev.off()

# Looking at cos2 of cols ----
fviz_cos2(CA.NOUNS, choice = "col", axes = 1:2)
pdf("cos2.col.dim1&2.pdf")
fviz_cos2(CA.NOUNS, choice = "col", axes = 1:2, top = 15)
dev.off()

pdf("cos2.col.dim1.pdf")
fviz_cos2(CA.NOUNS, choice = "col", axes = 1, top = 15)
dev.off()

pdf("cos2.col.dim2.pdf")
fviz_cos2(CA.NOUNS, choice = "col", axes = 2, top = 15)
dev.off()


# Looking at the contributions of columns ----
fviz_contrib(CA.NOUNS, choice = "col", axes = 1:2)
pdf("contrib.col.dim1&2.pdf")
fviz_contrib(CA.NOUNS, choice = "col", axes = 1:2, top = 15)
dev.off()

pdf("contrib.col.dim1.pdf")
fviz_contrib(CA.NOUNS, choice = "col", axes = 1, top = 15)
dev.off()

pdf("contrib.col.dim2.pdf")
fviz_contrib(CA.NOUNS, choice = "col", axes = 2, top = 15)
dev.off()

## Looking at the columns/rows with a cos2> 0.3 ----
fviz_ca_row(CA.NOUNS, select.row = list(cos2 = 0.3))

fviz_ca_col(CA.NOUNS, select.col = list(cos2 = 0.7))

fviz_ca_biplot(CA.NOUNS, select.row = list(cos2 = 0.29),
               select.col = list(cos2 = 0.6)) +
  theme_minimal()
  
## Looking at the top contributors columns/rows ----
fviz_ca_biplot(CA.NOUNS, select.row = list(contrib = 10),
               select.col = list(contrib = 20), repel = TRUE) +
  theme_minimal()


### For rows ----
fviz_ca_row(CA.NOUNS, select.row = list(contrib = 10))+
  theme_minimal()

### For columns ----
fviz_ca_col(CA.NOUNS, select.col = list(contrib = 13), title = "Top Row Contributors")+
  theme_minimal() 

# Contribution Biplotting ----
fviz_ca_biplot(CA.NOUNS, map="colgreen", arrow = c(FALSE, TRUE),
               repel = TRUE, top = 15)

fviz_ca_col(CA.NOUNS, map="colgreen", arrow = c(TRUE),
               repel = TRUE, top = 15)

# Description of Dimensions ----
res.desc <- dimdesc(CA.NOUNS, axes = c(1:2))

## Description of dimension 1 ----
head(res.desc[[1]]$row, 5)
head(res.desc[[1]]$col, 5)

## Description of dimension 2 ----
head(res.desc[[2]]$row, 5)
head(res.desc[[2]]$col, 5)


# Hierarchical Cluster ----
HCPC.NOUNS <- HCPC(CA.NOUNS, nb.clust = 5) 
save(HCPC.NOUNS, file = "HCPC.NOUNS.Rda")    

fviz_cluster(HCPC.NOUNS)

# Which variables describe better the clusters? ----
head(HCPC.NOUNS$desc.var$`5`, 15)

# Which documents are present in cluster NºX ----
clusters <- HCPC.NOUNS$data.clust %>% 
  select(clust) %>% 
  tibble::rownames_to_column("Year")
save(clusters, file = "clusters.HCPC.Rda")
clusters %>% 
  filter(str_detect(Year, "2009"))
merge(wordnumperyear, clusters) %>% 
  group_by(clust, words) %>% 
  summarize(n = sum(n)) %>% 
  arrange(desc(n)) %>% 
  bind_tf_idf(words, clust, n) %>% 
  filter(clust %in% 1:3) %>% 
  slice_max(tf_idf , n = 40) %>% 
  ggplot(aes(tf_idf, reorder_within(words, tf_idf, clust))) +
  geom_col() +
  facet_wrap(vars(clust), scales = "free") +
  scale_y_reordered()

HCPC.NOUNS

HCPC.NOUNS$desc.var

# Dendogram 
fviz_dend(HCPC.NOUNS, show_labels = FALSE)
# Individuals factor map
fviz_cluster(HCPC.NOUNS, geom = "point", main = "Factor map")

# why are not working
# HCPC.NOUNS$desc.var$test.chi2
# HCPC.NOUNS$desc.var$category
# HCPC.NOUNS$desc.axes


# Which are the most representative reports of the clusters? ----
HCPC.NOUNS$desc.ind$para$`1`
HCPC.NOUNS$desc.ind$dist$`1`

HCPC.NOUNS$desc.ind$para$`2`
HCPC.NOUNS$desc.ind$dist$`2`

HCPC.NOUNS$desc.ind$para$`3`
HCPC.NOUNS$desc.ind$dist$`3`

HCPC.NOUNS$desc.ind$para$`4`
HCPC.NOUNS$desc.ind$dist$`4`

HCPC.NOUNS$desc.ind$para$`5`
HCPC.NOUNS$desc.ind$dist$`5`

## Which are the reports for the X clust? ----
HCPC.NOUNS$data.clust %>% 
  tibble::rownames_to_column("Year") %>% 
  group_by(clust) %>% distinct(Year) %>% 
  filter(clust == 4) %>% 
  print(n= 51)


# Checking the words over and under-represented ----
clus1 <- HCPC.NOUNS$desc.var$`1`
clus1[c(1:20, 169:189),]
clus2 <- HCPC.NOUNS$desc.var$`2`
clus2[c(1:20, 359:379),]
clus3 <- HCPC.NOUNS$desc.var$`3`
clus3[c(1:10, 368:378),]
clus4 <- HCPC.NOUNS$desc.var$`4`
clus4[c(1:20, 360:380),]
clus5 <- HCPC.NOUNS$desc.var$`5`
clus5[c(1:20, 359:379),]

HCPC.NOUNS$desc.ind$para
HCPC.NOUNS$desc.ind$dist
HCPC.NOUNS$call

