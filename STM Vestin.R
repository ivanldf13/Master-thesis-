# Nettoyage du texte ----
text <- textProcessor(data.clean$text, metadata = data.clean)

## Voir combien de documents ou mots sont retirés (pas utile)
plotRemoved(text$documents, lower.thresh = seq(1, 200, 100))

# Créer un document d'analyse (lower.tresh = fréquence minimum du mot)
out <- prepDocuments(text$documents, text$vocab, text$meta,
                     lower.thresh = 15,
                     upper.thresh = 50)

# Sélectionner le bon nombre de topic ----
s <- searchK(out$documents, out$vocab, K = 5:10,
             prevalence = ~treatment + s(pid_rep),
             data=out$meta)
# Il faut que exclus (exclusivity) et semcoh (semantic coherence) soient les plus grande possible.

# Faire le STM ----
res <- stm(documents=out$documents,
           vocab=out$vocab,
           K=20,
           prevalence = ~treatment + s(pid_rep),
           max.em.its = 75,
           data=out$meta,
           init.type="Spectral")

# Voir le labels des topics ----
labelTopics(res)
topics <- labelTopics(res)

# Proportion des topics en graphique
plot(res, type = "summary")

# Graph des mots par topics ----
cloud(res, topic=7)

# Regression ----
reg <- estimateEffect(1:20 ~treatment + s(pid_rep), res,
                      metadata = out$meta, uncertainty = "Global")

# Résultat de la régression pour le topic 1 ----
summary(reg, topics=1)

# Résultat de la régression pour les topics 2 à 4 ----
summary(reg, topics=2:4)


# Classement sur un variable métrique ----
plot(reg, covariate="pid_rep", topics=1,
     method = "continuous")

# Même chose avec plusieurs topics ----
plot(reg, covariate="pid_rep", topics=2:4,
     method = "continuous")

# Correlation Graph ----
mod.out.corr <- topicCorr(res)

plot(mod.out.corr)


