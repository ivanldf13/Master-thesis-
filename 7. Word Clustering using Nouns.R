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
#   count(lemma, sort=TRUE) %>%  print(n=25)


# Word clustering with just nouns ----
##CA per year 
correspondance.matrix.POS <- 
  POS.NOUNS %>%
  filter(n > 40) %>%
  spread(key = token, value = n, fill = 0) %>% 
  tibble::column_to_rownames("Year") 


library(dplyr)


# Extract the first five rows
rows <- slice(correspondance.matrix.POS, 1:5)

# Extract the first five columns from the extracted rows
table_data <- select(rows, 1:5)

# Print the table
print(table_data)

formatted_table <- kable(table_data, caption = "Table X: Word Frequency per Year",
                         align = "c") %>% 
  kable_styling(bootstrap_options = "striped",
                font_size = 16)
save_kable(formatted_table, file = "freq_table.png")
# Save the HTML table as a PNG file
webshot::webshot(formatted_table, file = "output.png")


# TODO mejorar la tabla


# Looking whether there is a problem with a row ----
# POS.NOUNS %>% 
#   filter(Year == 2009) %>% 
#   arrange(desc(n))
# 
# CA.NOUNS$row$contrib[,2] %>% 
#   sort(., decreasing = TRUE) %>% 
#   head(., n= 5)

correspondance.matrix.POS[is.na(correspondance.matrix.POS)] <- 0
nrow(correspondance.matrix.POS)

CA.NOUNS <- CA(correspondance.matrix.POS, ncp = 2)
save(CA.NOUNS, file = "CA.NOUNS.Rda")

# Comparing the number of words of early and late reports ----
freq.rep <- summary(corpus(dc.np.final)) %>% 
  select(Text, Tokens, Year)

freq.rep %>% 
  ggplot(aes(Year, Tokens)) +
  geom_line() +
  labs(y = "Number of words per year") +
  theme(legend.position = "none")

ggsave("number.of.words.per.year.png", units = "cm", width = 26, height = 14)


# Getting the chi-square ----
chisq.test(correspondance.matrix.POS)

correspondance.matrix.POS %>% 
  cramer_v()

# Getting the eigen values ----
get_eigenvalue(CA.NOUNS)

png("eigen.values.screeplot.png")
fviz_eig(CA.NOUNS, addlabels = TRUE, ylim = c(0,50))
dev.off()

# Correlation matrix ----
CA.NOUNS$col$cos2[ ,-3:-5] %>%
  .[order(.[,2], decreasing=TRUE),] %>% 
  .[.[,1]>=0.3|.[,2]>=0.3,  ] %>% 
  ggcorrplot(lab = TRUE, lab_size = 2.5)

## Just the 1st dimension ----
png("Dim1.png")
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
png("Dim2.png")
Dim2 <- CA.NOUNS$col$cos2[, 2] %>%
  .[order(., decreasing=TRUE)] %>% 
  .[. >=0.3] %>% 
  as.matrix()
colnames(Dim2) <- "Dim 2"
Dim2 %>% 
  ggcorrplot(lab = TRUE, lab_size = 2.5) 
dev.off()

# Plots ----
png("biplot.png")
fviz_ca_biplot(CA.NOUNS)
dev.off()

png("row.points.CA.png")
fviz_ca_row(CA.NOUNS)
dev.off()

png("col.points.CA.png")
fviz_ca_col(CA.NOUNS)
dev.off()

# correspondance.matrix.POS$dim1 <- CA.NOUNS$svd$U[,1]
# correspondance.matrix.POS$dim2 <- CA.NOUNS$svd$U[,2]

# How many clusters would be optimal? ----
png("kmeans_k.png")
fviz_nbclust(correspondance.matrix.POS, kmeans, method = "gap_stat")
dev.off()

png("hcut_k.png")
fviz_nbclust(correspondance.matrix.POS, hcut, method = "gap_stat")
dev.off()

# Looking at cos2 of rows ----
png("cos2.row.dim1&2.png")
fviz_cos2(CA.NOUNS, choice = "row", axes = 1:2, top = 15)
dev.off()

png("cos2.row.dim1.png")
fviz_cos2(CA.NOUNS, choice = "row", axes = 1, top = 15)
dev.off()

png("cos2.row.dim2.png")
fviz_cos2(CA.NOUNS, choice = "row", axes = 2, top = 15)
dev.off()

# Looking at the contributions of rows ----
fviz_contrib(CA.NOUNS, choice = "row", axes = 1:2)

png("contrib.rows.dim1&2.png")
fviz_contrib(CA.NOUNS, choice = "row", axes = 1:2, top = 15)
dev.off()

png("contrib.rows.dim1.png")
fviz_contrib(CA.NOUNS, choice = "row", axes = 1, top = 15)
dev.off()

png("contrib.rows.dim2.png")
fviz_contrib(CA.NOUNS, choice = "row", axes = 2, top = 15)
dev.off()

# Looking at cos2 of cols ----
fviz_cos2(CA.NOUNS, choice = "col", axes = 1:2)
png("cos2.col.dim1&2.png")
fviz_cos2(CA.NOUNS, choice = "col", axes = 1:2, top = 15)
dev.off()

png("cos2.col.dim1.png")
fviz_cos2(CA.NOUNS, choice = "col", axes = 1, top = 15)
dev.off()

png("cos2.col.dim2.png")
fviz_cos2(CA.NOUNS, choice = "col", axes = 2, top = 15)
dev.off()


# Looking at the contributions of columns ----
fviz_contrib(CA.NOUNS, choice = "col", axes = 1:2)
png("contrib.col.dim1&2.png")
fviz_contrib(CA.NOUNS, choice = "col", axes = 1:2, top = 15)
dev.off()

png("contrib.col.dim1.png")
fviz_contrib(CA.NOUNS, choice = "col", axes = 1, top = 15)
dev.off()

png("contrib.col.dim2.png")
fviz_contrib(CA.NOUNS, choice = "col", axes = 2, top = 15)
dev.off()

## Looking at the columns/rows with a cos2> 0.3 ----
fviz_ca_row(CA.NOUNS, select.row = list(cos2 = 0.3))

fviz_ca_col(CA.NOUNS, select.col = list(cos2 = 0.7))

fviz_ca_biplot(CA.NOUNS, 
               repel = TRUE,
               select.row = list(cos2 = 0.29),
               select.col = list(cos2 = 0.65),
               title = "Figure X: Top cos2 individuals")+
  theme_minimal()

## Looking at the top contributors columns/rows ----
fviz_ca_biplot(CA.NOUNS, select.row = list(contrib = 20),
               select.col = list(contrib = 20), repel = TRUE, title = "Figure X: Top 20 contributors") +
  theme_minimal()
ggsave("Top 20 Contributors.png", units = "cm", width = 26, height = 14)

### For rows ----
fviz_ca_row(CA.NOUNS, select.row = list(contrib = 10))+
  theme_minimal()

### For columns ----
fviz_ca_col(CA.NOUNS, select.col = list(contrib = 10))+
  theme_minimal()

# Contribution Biplotting ----
fviz_ca_biplot(CA.NOUNS, 
               map = "colgreen", 
               arrow = c(FALSE, TRUE),
               repel = TRUE, top = 15)

fviz_ca_col(CA.NOUNS, map = "colgreen", arrow = c(TRUE),
            repel = TRUE, top = 15)

# TODO What to do with this graph? Read the book
fviz_ca_biplot(CA.NOUNS, 
               select.row = list(contrib = 20),
               select.col = list(contrib = 20),
               map = "rowprincipal", 
               arrow = c(TRUE, TRUE),
               repel = TRUE)

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
factor_map_plot <- plot(HCPC.NOUNS, 
                        choice = "map", 
                        draw.tree = FALSE,
                        title = "Figure X: Factor map with the five clusters")

ggsave("factor_map.png", plot = factor_map_plot, width = 10, height = 7.5, dpi = 300)
# FIXME V: the factor map file is white, possible to include the second dimension?


fviz_cluster(HCPC.NOUNS, show.clust.cent = TRUE)


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
  filter(clust %in% 1:5) %>% 
  slice_max(tf_idf , n = 40) %>% 
  ggplot(aes(tf_idf, reorder_within(words, tf_idf, clust))) +
  geom_col() +
  facet_wrap(vars(clust), scales = "free") +
  scale_y_reordered()

# Dendogram 
fviz_dend(HCPC.NOUNS, show_labels = FALSE)
fviz_dend(HCPC.NOUNS, 
          show_labels = FALSE,
          cex = 0.5,
          color_labels_by_k = TRUE,
          type = "rectangle", 
          repel = TRUE,
          main = "Figure X: Cluster Dendogram"
          )
# TODO completar el dendogram

# Which are the most representative reports of the clusters? ----
HCPC.NOUNS$desc.ind$para$`1` %>% 
  belle_table("para_1.png")
as.data.frame(HCPC.NOUNS$desc.ind$dist$`1`)

HCPC.NOUNS$desc.ind$para$`2`
HCPC.NOUNS$desc.ind$dist$`2`

HCPC.NOUNS$desc.ind$para$`3`
HCPC.NOUNS$desc.ind$dist$`3`

HCPC.NOUNS$desc.ind$para$`4`
HCPC.NOUNS$desc.ind$dist$`4`

HCPC.NOUNS$desc.ind$para$`5`
HCPC.NOUNS$desc.ind$dist$`5`


belle_table <- function(table, file){
  table %>% 
    as.data.frame() %>%
    tibble::rownames_to_column("year") %>% 
    pivot_wider(names_from = 1, values_from = 2) %>%
    gt() %>%
    gt::gtsave(filename = file)
}

HCPC.NOUNS$desc.ind$para$`1` %>% 
  as.data.frame() %>%
  kable() %>% 
  save_kable(file = "para_1.png")

## Which are the reports for the X clust? ----
HCPC.NOUNS$data.clust %>% 
  tibble::rownames_to_column("Year") %>% 
  group_by(clust) %>% 
  distinct(Year) %>% 
  filter(clust == 5) %>% 
  arrange(Year) %>% 
  print(n= 51)


# Checking the words over and under-represented ----
clus1 <- HCPC.NOUNS$desc.var$`1`
clus1[c(1:30, 159:189),]
clus2 <- HCPC.NOUNS$desc.var$`2`
clus2[c(1:30, 349:379),]
clus3 <- HCPC.NOUNS$desc.var$`3`
clus3[c(1:30, 348:378),]
clus4 <- HCPC.NOUNS$desc.var$`4`
clus4[c(1:30, 350:380),]
clus5 <- HCPC.NOUNS$desc.var$`5`
clus5[c(1:30, 349:379),]

HCPC.NOUNS$desc.ind$para
HCPC.NOUNS$desc.ind$dist
HCPC.NOUNS$call

