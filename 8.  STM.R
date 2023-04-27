# Loading useful packages ----
library(tidytext)
library(dplyr)
library(stm)
library(stringr)
library(broom)
library(tidytext)
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

# TODO results of the tibble below still change despite the seed

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

save(searchk_5.20_nouns, file = "searchk_5.20_nouns.Rda")

## Looking for the best K
exclus <- scale(unlist(searchk_5.20$results$exclus))
semcoh <- scale(unlist(searchk_5.20$results$semcoh))

# number of topics suggested
tibble(k = 5:20, exclus = exclus[,1], semcoh = semcoh[,1]) %>% 
  pivot_longer(c(exclus, semcoh), names_to = "scores", values_to = "values") %>% 
  ggplot(aes(k, values, linetype = scores)) +
  geom_line() +
  scale_x_continuous(breaks = 5:20) +
  labs(x="number of suggested topics")
ggsave("number.of.topics.stm.pdf", units = "cm", width = 26, height = 14)

# number of topics suggested only nouns
exclus_nouns <- scale(unlist(searchk_5.20_nouns$results$exclus))
semcoh_nouns <- scale(unlist(searchk_5.20_nouns$results$semcoh)) 

tibble(k = 5:20, exclus_nouns = exclus_nouns[,1], semcoh_nouns = semcoh_nouns[,1]) %>% 
  pivot_longer(c(exclus_nouns, semcoh_nouns), names_to = "scores", values_to = "values") %>% 
  ggplot(aes(k, values, linetype = scores)) +
  geom_line() +
  scale_x_continuous(breaks = 5:20) +
  labs(x="number of suggested topics")
ggsave("number.of.topics.stm.nouns.pdf", units = "cm", width = 26, height = 14)

# does the topic makes semantic sense?
res20 <- stm(documents=dfm,
           K=20,
           prevalence = metadata,
           max.em.its = 150,
           # data=meta,
           init.type = "Spectral",
           content = ~ b4.after)
save(res20, file = "res20.Rda")

res10 <- stm(documents=dfm,
             K=10,
             prevalence = metadata,
             max.em.its = 150,
             # data=meta,
             init.type = "Spectral",
             seed = 9)
save(res10, file = "res10.Rda")

res10_nouns <- stm(documents=dfm_nouns,
             K=10,
             prevalence = metadata,
             max.em.its = 150,
             # data=meta,
             init.type="Spectral")
save(res10_nouns, file = "res10_nouns.Rda")

## with b4.after ----
# res10.b4 <- stm(documents=dfm,
#              K=10,
#              data = metadata,
#              max.em.its = 150,
#              prevalence = ~ year + b4.after,
#              # data=meta,
#              init.type="Spectral",
#              content = ~ b4.after)
# save(res10.b4, file = "res10.b4.Rda")

# what are the words belonging to each topic? ----
labelTopics(res10, n = 20)
topics.dfm.10 <- labelTopics(res10, n = 30)
save(topics.dfm.10, file = "topics.dfm.10.Rda")

labelTopics(res10_nouns, n = 20)
topics.dfm.10_nouns <- labelTopics(res10_nouns, n = 20)
save(topics.dfm.10_nouns, file = "topics.dfm.10_nouns.Rda")

labelTopics(res20, n = 15)
topics.dfm.20 <- labelTopics(res20, n = 15)
save(topics.dfm.20, file = "topics.dfm.20.Rda")

topics.dfm.10$frex
topics.dfm.9$frex

# Getting the most salient reports for a given topic ----
## All words
thoughts10 <- findThoughts(res10, topics = 10, texts = dc.np.final$Year, n = 5)$docs[[1]]
plotQuote(thoughts10,    width    =    30,    main    =   "Topic 10")
## Only nouns
thoughts10_nouns <- findThoughts(res10_nouns, topics = 1, texts = dc.np.final$Year, n = 5)$docs[[1]]
plotQuote(thoughts10_nouns,    width    =    30,    main    =   "Topic 1")

# Proportion des topics en graphique ----
pdf("plot.correlation.topics.pdf")
plot(res10, type = "summary", topic.names = c("Topic 1* natural sciences research:", 
                                            "Topic 2* what are we, how we collaborate:",
                                            "Topic 3 African challenges:", 
                                            "Topic 4 Hygiene education:",
                                            "Topic 5* 21st century challenges:", 
                                            "Topic 6 Agricultural education & cultural promotion:", 
                                            "Topic 7 *", 
                                            "Topic 8 Demographic challenges:", 
                                            "Topic 9 Medical education, early 20th century +:", 
                                            "Topic 10 Hygiene, psychological wellbeing:",
                                            "T11"))
dev.off()


# Graph des mots par topics ----
cloud(res10, topic=7, random.order = FALSE,max.words = 25, rot.per = 0)

# Régression, pour voir si les années sont correlées avec les topics ----
## Only nouns ----
reg10_nouns <- estimateEffect(1:10 ~I(Year^2) + Year, res10_nouns,
                      metadata = as.data.frame(metadata),
                      uncertainty = "Global")
save(reg10_nouns, file = "reg10_nouns.Rda")

## All words ----
reg10 <- estimateEffect(1:10 ~I(Year^2) + Year, res10,
                      metadata = as.data.frame(metadata),
                      uncertainty = "Global")
save(reg10, file = "reg10.Rda")

# Résultat de la régression pour le topic 1
summary(reg10, topics = c(1:10))

# Résultat de la régression pour tous les topics 
summary(reg10_nouns, topics = 1:10)


# Classement sur un variable métrique ----
plot(reg, covariate="year", topics = c(1:3),
     method = "continuous",
     xlab = "years")

# Même chose avec plusieurs topics
plot(reg10, covariate = "Year", topics = c(1, 2, 5, 7, 9),
     method = "continuous",
     xlab = "Years")

plot(reg10_nouns, covariate = "Year", topics = c(1, 3, 4, 5, 6, 8, 9, 10),
     method = "continuous",
     xlab = "years")

# Graphique de corrélation ----
mod.out.corr <- topicCorr(res10)
mod.out.corr_nouns <- topicCorr(res10_nouns)

pdf("correlation.topics.plot.pdf")
plot(mod.out.corr, vlabels = c("Topic 1* natural sciences research:", 
                               "Topic 2* what are we, how we collaborate:",
                               "Topic 3 African challenges:", 
                               "Topic 4 Hygiene education:",
                               "Topic 5* 21st century challenges:", 
                               "Topic 6 Agricultural education & cultural promotion:", 
                               "Topic 7: health", 
                               "Topic 8 Demographic challenges:", 
                               "Topic 9 Medical education, early 20th century:", 
                               "Topic 10* Hygiene, psychological wellbeing:"), 
                               vertex.label.cex = 0.9)
dev.off()



# graphic of the correlation with the given topics
mod.out.corr <- topicCorr(res10)

plot(mod.out.corr_nouns, c(1, 3, 4, 5, 6, 8, 9, 10))

# Getting the Beta (probability of a word to belong to a topic, topical content) ----
beta <- tidy(res10, matrix = "beta")
beta_nouns <- tidy(res10_nouns, matrix = "beta")

beta %>% 
  filter(topic %in% c(1, 2, 5, 7,9, 10)) %>% 
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

gamma %>% 
  filter(topic %in% c(1, 2, 5, 7, 9)) %>% 
  ggplot(aes(x= document, y = gamma, linetype = factor(topic)))+
  geom_line()

# Take into account those topics with a p>0.25
gamma %>% 
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
# TODO what would be another way to add a title?
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
                                                   "3.", 
                                                   "4.", 
                                                   "5.",
                                                   "6.",
                                                   "8.",
                                                   "9.", 
                                                   "10."),
                    name = "Topics") +
  ggtitle("Topics and its correlation with time metadata") 


# TODO not working +
  scale_x_continuous(limits = c(1913, 2013), 
                     breaks = seq(1913, 2013, 20),
                     labels = seq(1913, 2013, 20)) +
  scale_y_continuous()
# TODO what would be another way to add a title?
# TODO how to change the xlab



ggsave("gamma_nouns.stat.significant.pdf", units = "cm", width = 26, height = 14)

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

