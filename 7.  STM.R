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

load("res.Rda")

# DFM ----
## Indicating the year of each document.
metadata <- dc.np.final %>%
  select(Year) %>% 
  as.matrix()

# it goes well until we apply the as.matrix
  # filter(year != 2012) %>% quitar cuando esté el ocr 2012
  # as.matrix()
  # b4.after = ifelse(year>1960, "after 1960", "before 1960")) %>% 

# Creating the Dataframe
dfm <- wordnumperyear %>% 
  cast_dfm(document = Year, term = words, value = n)

# TODO whenever there are two words toguether, it separes it and it puts an NA in the 2nd one
wordnumperyear %>% 
  separate(words, c("FrstWord", "SndWord"), sep = "\\s") %>% 
  filter(!is.na(SndWord))

# To choose how many topics will be better ----
# Exclusivity and semcoh (semantic coherence) should be as high as possible.
# TODO results of the tibble below still change despite the seed
searchk_5.20 <- searchK(documents = dfm,
                   K = 5:20,
                   N = 10,
                   prevalence = metadata,
                   cores = 4,
                   seed = 9)

save(searchk_5.20, file = "searchk_5.20.Rda")

## Looking for the best K
exclus <- scale(unlist(searchk_5.20$results$exclus))
semcoh <- scale(unlist(searchk_5.20$results$semcoh))

tibble(k = 5:20, exclus = exclus[,1], semcoh = semcoh[,1]) %>% 
  pivot_longer(c(exclus, semcoh), names_to = "scores", values_to = "values") %>% 
  ggplot(aes(k, values, linetype = scores)) +
  geom_line() +
  scale_x_continuous(breaks = 5:20) +
  labs(x="number of suggested topics")
ggsave("number.of.topics.stm.pdf", units = "cm", width = 26, height = 14)

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

res9 <- stm(documents=dfm,
             K=9,
             prevalence = metadata,
             max.em.its = 150,
             # data=meta,
             init.type="Spectral")
save(res9, file = "res9.Rda")

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
topics.dfm.10 <- labelTopics(res10, n =20)
save(topics.dfm.10, file = "topics.dfm.10.Rda")

labelTopics(res9, n = 15)
topics.dfm.9 <- labelTopics(res9, n =15)
save(topics.dfm.9, file = "topics.dfm.9.Rda")

labelTopics(res20, n = 15)
topics.dfm.20 <- labelTopics(res20, n =15)
save(topics.dfm.20, file = "topics.dfm.20.Rda")

topics.dfm.10$frex
topics.dfm.9$frex

# Getting the most salient reports for a given topic
thoughts10 <- findThoughts(res10, topics = 9, texts = dc.np.final$Year, n = 5)$docs[[1]]
plotQuote(thoughts10,    width    =    30,    main    =   "Topic 9")

thoughts11 <- findThoughts(res11, topics = 10, texts = dc.np.final$Year, n = 5)$docs[[1]]
plotQuote(thoughts11,    width    =    30,    main    =   "Topic 10")

# Proportion des topics en graphique ----
pdf("plot.correlation.topics.pdf")
plot(res11, type = "summary", topic.names = c("Topic 1* natural sciences research:", 
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

# Getting the Theta
thetaPosterior(res10)
# Graph des mots par topics ----
cloud(res10, topic=7, random.order = FALSE,max.words = 25, rot.per = 0)

# Régression, pour voir si les années sont correlées avec les topics ----
reg <- estimateEffect(1:10 ~I(Year^2) + Year, res10,
                      metadata = as.data.frame(metadata),
                      uncertainty = "Global")
save(reg, file = "reg.Rda")

reg11 <- estimateEffect(1:11 ~I(Year^2) + Year, res11,
                      metadata = as.data.frame(metadata),
                      uncertainty = "Global")
save(reg11, file = "reg11.Rda")

# Résultat de la régression pour le topic 1
summary(reg, topics = c(1:10))

# Résultat de la régression pour tous les topics 
summary(reg11, topics = 1:11)


# Classement sur un variable métrique ----
plot(reg, covariate="year", topics = c(1:3),
     method = "continuous",
     xlab = "years")

# Même chose avec plusieurs topics
plot(reg, covariate = "Year", topics = c(1, 2, 5, 9),
     method = "continuous",
     xlab = "Years")

plot(reg, covariate = "Year", topics = c(5,10),
     method = "continuous",
     xlab = "years")

# Graphique de corrélation ----
mod.out.corr <- topicCorr(res10)
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

plot(mod.out.corr, c(1:8), vlabels = c("University", "Health", "army", "school",
                                       "rcmdr", "rstudio", "silge", "dplyr"))

# Getting the Beta (probability of a word to belong to a topic, topical content) ----
beta <- tidy(res10, matrix = "beta")

beta %>% 
  filter(topic %in% c(1, 2, 5, 7)) %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10)
# maybe create an object from this

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

gamma %>% 
  filter(topic %in% c(1, 2, 5, 7)) %>% 
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
  filter(topic %in% c(1, 2, 5, 7)) %>%
  ggplot(aes(document, gamma, fill = factor(topic)))+
  geom_col()+
  scale_fill_manual(values = c("#387fc7","#e63cfe", "#af338e", "#97bea1", "#bfbb8a"),
                    labels = c("1.Natural Sciences", "2.Ways to collaborate, who we are", 
                               "5.XXI century challenges", 
                               "10.Hygiene, psychological wellbeing"),
                    name = "Topics")
ggsave("gamma.stat.significant.pdf", units = "cm", width = 26, height = 14)

                               # , "#e7b973", ))"#6fc0b8"

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

