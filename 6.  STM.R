# Loading useful packages ----
library(tidytext)
library(dplyr)
library(stm)
library(stringr)
library(broom)
library(ggplot2)
library(wordcloud)
library(corrplot)
library(igraph)
library(stmCorrViz)
library(tidyr)

#Loading useful files ----
load("dc.np.final.Rda")
load("wordnumperyear.Rda")
load("POS.NOUNS.Rda")


# DFM ----
## Indicating the year of each document.
metadata <- dc.np.final %>%
  select(Year) %>% 
  as.matrix()

# Creating the Dataframe
dfm <- wordnumperyear %>% 
  cast_dfm(document = Year, term = words, value = n)

dfm_nouns <- POS.NOUNS %>% 
  cast_dfm(document = Year, term = token, value = n)

# To choose how many topics will be better ----
searchk_5.20 <- searchK(documents = dfm,
                   K = 5:20,
                   N = 10,
                   prevalence = metadata,
                   cores = 4,
                   heldout.seed = 9)

save(searchk_5.20, file = "searchk_5.20.Rda")

searchk_5.20_nouns <- searchK(documents = dfm_nouns,
                        K = 5:20,
                        N = 10,
                        prevalence = metadata,
                        cores = 4,
                        heldout.seed = 9)
# set.seed()
save(searchk_5.20_nouns, file = "searchk_5.20_nouns.Rda")


## Looking for the best K ----
exclus <- scale(unlist(searchk_5.20$results$exclus))
semcoh <- scale(unlist(searchk_5.20$results$semcoh))


## number of topics suggested ----
tibble(k = 5:20, exclus = exclus[,1], semcoh = semcoh[,1]) %>% 
  pivot_longer(c(exclus, semcoh), names_to = "scores", values_to = "values") %>% 
  ggplot(aes(k, values, linetype = scores)) +
  geom_line() +
  scale_x_continuous(breaks = 5:20) +
  scale_linetype_manual(values = c("solid", "dashed"), 
                        name = "Score Type", 
                        labels = c("Exclusivity", "Semantic Coherence")) +
  labs(x = "Number of suggested topics", y = "Standarised values") +
  ggtitle("Figure X: Number of topic suggested by exclusivity and semantic coherence")
ggsave("number.of.topics.stm.pdf", units = "cm", width = 26, height = 14)


## Looking for the best K only nouns ----
exclus_nouns <- scale(unlist(searchk_5.20_nouns$results$exclus))
semcoh_nouns <- scale(unlist(searchk_5.20_nouns$results$semcoh)) 
# number of topics suggested only nouns 
tibble(k = 5:20, exclus_nouns = exclus_nouns[,1], semcoh_nouns = semcoh_nouns[,1]) %>% 
  pivot_longer(c(exclus_nouns, semcoh_nouns), names_to = "scores", values_to = "values") %>% 
  ggplot(aes(k, values, linetype = scores)) +
  geom_line() +
  scale_x_continuous(breaks = 5:20) +
  scale_linetype_manual(values=c("solid", "dashed"), 
                        name="Score Type", 
                        labels=c("Exclusivity", "Semantic coherence")) +
  labs(x = "Number of suggested topics", y = "Standarised values") +
  ggtitle("Figure X: Number of topic suggested by Exclusivity and Semantic coherence")
ggsave("number.of.topics.stm.nouns.pdf", units = "cm", width = 26, height = 14)

# Estimation of the STM
## 10 topics, all words ----
set.seed(2216013)

res10 <- stm(documents=dfm,
             K=10,
             prevalence = metadata,
             max.em.its = 150,
             # data=meta,
             init.type = "Spectral",
             seed = 9)
save(res10, file = "res10.Rda")

## 10 topics, only nouns ----
set.seed(2216013)
res10_nouns <- stm(documents = dfm_nouns,
                   K=10,
                   prevalence = metadata,
                   max.em.its = 150,
                   # data=meta,
                   init.type="Spectral")
save(res10_nouns, file = "res10_nouns.Rda")


# What are the words belonging to each topic? ----
labelTopics(res10, n = 20)
topics.dfm.10 <- labelTopics(res10, n = 30)
save(topics.dfm.10, file = "topics.dfm.10.Rda")


## What are the words belonging to each topic - only nouns ----
labelTopics(res10_nouns, n = 20, frexweight = 0.25)
,
            topics = c(1, 3, 4, 5, 6, 8, 9, 10))
topics.dfm.10_nouns <- labelTopics(res10_nouns, n = 20, frexweight = 0.25, 
                                   topics = c(1, 3, 4, 5, 6, 8, 9, 10))

save(topics.dfm.10_nouns, file = "topics.dfm.10_nouns.Rda")

### Table ----
df <- as.data.frame(topics.dfm.10_nouns$frex)

df <- df[1:10, 1:10]
kable(df, format = "markdown", align = "lcc", caption = "Top 10 FREX Words for Topic 1")
png("prueba.png")

knitr::include_graphics(tmp_file)
dev.off()

topics.dfm.10_nouns$frex
topics.dfm.9$frex
topics.dfm.10_nouns

# Getting the most salient reports for a given topic ----
## All words
thoughts10 <- findThoughts(res10, topics = 1, texts = dc.np.final$Year, n = 5)$docs[[1]]
plotQuote(thoughts10,    width    =    30,    main    =   "Topic 10")
## Only nouns
thoughts10_nouns <- findThoughts(res10_nouns, topics = 3, texts = dc.np.final$Year, n = 20)$docs[[1]]
plotQuote(thoughts10_nouns,    width    =    30,    main    =   "Topic 3")


toughts10_nouns <- findThoughts(res10_nouns)
topic1_docs <- which(toughts10_nouns$theta[, 1] == max(toughts10_nouns$theta[, 1]))


# Graph of the topics' proportion, only nouns ----
png("plot.expected.topic.proportions.png", width = 1000, height = 600)
plot(res10_nouns, type = "summary", topic.names = c("T1*:", 
                                                    "T2:",
                                                    "T3*: 20th century challenges - African, cities and transport development:", 
                                                    "T4*: War relief, Insect-borne diseases:", 
                                                    "T5*: Population issues & Education for development:",
                                                    "T6*: Insect-borne diseases & Public health education:",
                                                    "T7:",
                                                    "T8*: 20th century challenges - Climate change:",
                                                    "T9*: Agronomy:", 
                                                    "T10*: Expansion of interests:"),
     xlab = "Expected Topic Proportions.\n Topics statistically correlated with time are shown with a star (*)",
     labeltype = "frex", 
     n = 7,
     main = "Figure X: Expected topic proportions")

dev.off()


## Word graph per topic ----
cloud(res10, topic=7, random.order = FALSE,max.words = 25, rot.per = 0)


# Regression, to see if the years are correlated with the topics ----
## Only nouns ----
reg10_nouns <- estimateEffect(1:10 ~I(Year^2) + Year, res10_nouns,
                      metadata = as.data.frame(metadata),
                      uncertainty = "Global")
save(reg10_nouns, file = "reg10_nouns.Rda")

### Regression result for all topics 
summary(reg10_nouns, topics = 1:10)

## All words ----
reg10 <- estimateEffect(1:10 ~I(Year^2) + Year, res10,
                      metadata = as.data.frame(metadata),
                      uncertainty = "Global")
save(reg10, file = "reg10.Rda")

# Résultat de la régression pour le topic 1
summary(reg10, topics = c(1:10))

# Classification on a metric variable ----
plot(reg10, covariate="Year", topics = ,
     method = "continuous",
     xlab = "years")

## Same thing with several topics ----
plot(reg10, covariate = "Year", topics = c(1, 2, 5, 7, 9),
     method = "continuous",
     xlab = "Years")

plot(reg10_nouns, covariate = "Year", topics = c(1, 3, 4, 5, 6, 8, 9, 10),
     method = "continuous",
     xlab = "years")


# Correlation graph ----
mod.out.corr <- topicCorr(res10)

pdf("correlation.topics.plot.pdf")
plot(mod.out.corr, vlabels = c("Topic 1*", 
                               "Topic 2",
                               "Topic 3*", 
                               "Topic 4*",
                               "Topic 5*", 
                               "Topic 6*", 
                               "Topic 7", 
                               "Topic 8*", 
                               "Topic 9*", 
                               "Topic 10*"), 
     vertex.label.cex = 0.9)
dev.off()

## Correlation graph only nouns ----
mod.out.corr_nouns <- topicCorr(res10_nouns)

plot(mod.out.corr_nouns, vlabels = c("Topic 1*", 
                               "Topic 2",
                               "Topic 3*", 
                               "Topic 4*",
                               "Topic 5*", 
                               "Topic 6*", 
                               "Topic 7", 
                               "Topic 8*", 
                               "Topic 9*", 
                               "Topic 10*"), 
     vertex.label.cex = 0.9)
png("correlation.topics_nouns.plot.png", width = 800, height = 800)
dev.off()

plot(mod.out.corr_nouns, c(1, 3, 4, 5, 6, 8, 9, 10))

# Getting the Beta (probability of a word to belong to a topic, topical content ----
# it gives the words with the highest prob of appearing in a topic) 
beta <- tidy(res10, matrix = "beta")
beta_nouns <- tidy(res10_nouns, matrix = "beta")

beta_nouns %>% 
  filter(topic == 1) %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10)

# FIXME V 
beta_nouns %>% 
  filter(topic %in% c(1, 3, 4, 5, 6, 8, 9, 10)) %>% 
  group_by(topic) %>% 
  slice_max(beta_nouns, n = 10)


# Beta graph for the most salient words in each topic ----
beta %>% 
  filter(topic %in% c(7:8)) %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 50) %>% 
  ungroup() %>% 
  ggplot(aes(x = beta, y = reorder_within(term, beta, topic), group = topic))+
  geom_col()+
  facet_wrap(vars(topic), scales = "free")+
  scale_y_reordered()

# Getting the gamma (probability of a document being in one or more topics) ----
gamma <- tidy(res10, matrix = "gamma")
gamma_nouns <- tidy(res10_nouns, matrix = "gamma")

gamma_nouns %>% 
  filter(topic == 3) %>% 
  top_n(gamma, n = 30)

gamma_nouns %>%
  filter(topic == 8) %>%
  mutate(label = seq(1914, 2013)) %>% 
  print(n = 100)

gamma %>% 
  filter(topic %in% c(1, 2, 5, 7, 9)) %>% 
  ggplot(aes(x= document, y = gamma, linetype = factor(topic)))+
  geom_line()

# Take into account those topics with a p>0.25 ----
gamma_nouns %>% 
  filter(gamma > 0.25) %>% 
  ggplot(aes(document, gamma, fill = factor(topic)))+
  geom_col()

#   scale_fill_manual(values = c("#387fc7", "#7459aa", "#af338e", "#eb0d71"))

# Graph for all the topics
gamma %>% 
  ggplot(aes(document, gamma, fill = factor(topic)))+
  geom_col()+
  scale_fill_discrete(labels = c(1:10))

# Graph for the statistically significant topics ----
gamma %>% 
  filter(topic %in% c(1, 2, 5, 7, 9)) %>%
  ggplot(aes(document, gamma, fill = factor(topic)))+
  geom_col()+
  scale_fill_manual(values = c("#387fc7","#e63cfe", "#af338e", "#97bea1", 
                                        "#bfbb8a", "#e7b973"),
                    labels = c("1.Natural Sciences", "2.Ways to collaborate, who we are", 
                               "5.XXI century challenges", 
                               "10.Hygiene, psychological wellbeing",
                               "9"),
                    name = "Topics")+
  ggtitle("Topics and its correlation with time metadata")
ggsave("gamma.stat.significant.pdf", units = "cm", width = 26, height = 14)

                                # "#e7b973"


# Graph for the statistically significant topics only nouns ----
gamma_nouns %>% 
filter(topic %in% c(1, 3, 4, 5, 6, 8, 9, 10)) %>%
  ggplot(aes(document, gamma, fill = factor(topic)))+
  geom_col()+
  scale_fill_manual(values = c("#387fc7","#97bea1", "#e57272", "#e63cfe",
                                        "#bfbb8a", "#7459aa", "#8a9ac5", "#6fc0b8"),
                                        labels = c("1.", 
                                                   "3. Globalisation challenges", 
                                                   "4. War relief, Insect-borne diseases", 
                                                   "5. Population issues & Education for development",
                                                   "6. Insect-borne diseases & Public health education",
                                                   "8. Smart Globalisation - Climate Change",
                                                   "9. Post-WWII needs", 
                                                   "10. Expansion of interests"),
                    name = "Topics") +
  ggtitle("Figure X: Noun Topics and its correlation with time metadata") +
  scale_x_continuous(limits = c(1, 100), 
                     breaks = seq(1, 100, 10),
                     labels = seq(1914, 2013, 10)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year",
       y = "Probability of a document being in one or more topics")
ggsave("noun_topics_gamma.pdf", units = "cm", width = 26, height = 14)


# Graph for all topics only nouns ----
gamma_nouns %>% 
  ggplot(aes(document, gamma, fill = factor(topic)))+
  geom_col()+
  scale_fill_manual(values = c("#387fc7","#008080", "#97bea1", "#e57272", "#e63cfe",
                               "#bfbb8a","#800000", "#7459aa", "#8a9ac5", "#6fc0b8"),
                    labels = c("1. ", 
                               "2. Topic 2",
                               "3. Globalisation challenges", 
                               "4. War relief, Insect-borne diseases", 
                               "5. Population issues & Education for development",
                               "6. Insect-borne diseases & Public health education",
                               "7, Topic 7",
                               "8. Smart Globalisation - Climate Change",
                               "9. Post-WWII needs", 
                               "10. Expansion of interests"),
                    name = "Topics") +
  ggtitle("Figure X: Noun Topics") +
  scale_x_continuous(limits = c(1, 100), 
                     breaks = seq(1, 100, 10),
                     labels = seq(1914, 2013, 10)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year",
       subtitle = "Topics statistically correlated with time and not statistically correlated",
       y = "Probability of a document being in one or more topics")
ggsave("allnoun_alltopics_gamma.pdf", units = "cm", width = 26, height = 14)

# Seeing in more detail the probability of each topic in their periods ----

gamma %>% 
  filter(document %in% 1:38) %>% 
  ggplot(aes(document, gamma, fill = factor(topic)))+
  geom_col()+
  scale_fill_manual(values = c("#387fc7", "#7459aa", "#af338e", "#eb0d71"))


gamma.col.test <- gamma %>% 
  filter(topic  %in% c(1, 2, 3, 5, 9), 
         document %in% 69:72) %>%
  slice_max(gamma, n = 15) %>% 
  ggplot(aes(gamma, factor(topic), fill = factor(topic)))+
  geom_col()+
  facet_wrap(vars(document))



# gamma.col.test + scale_fill_manual(values = c("red", "blue", "green", "black"))
cols <- c("2" = "#387fc7", "4" = "#7459aa", "7" = "#af338e", "17" = "#eb0d71")
gamma.col.test + scale_fill_manual(values = cols)

save.image('stm_10_nouns.RData')
