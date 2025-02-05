## Thursday, February 6, 2025
## Quantifying History workshop, NTNU

## Session 4.2 -- Word embedding

install.packages("word2vec")


library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidytext)
library(word2vec)

# setwd() if necessary
nobel <- read_rds("nobel_cleaned.Rds")

####################
## Word embedding
####################

## Word2Vec



write_lines(nobel$AwardSpeech, "nobel.txt")
prep_word2vec(origin="nobel.txt",destination="nobel_prep.txt",lowercase=TRUE,bundle_ngrams=2)
set.seed(123)
model <- train_word2vec("nobel_prep.txt","nobel_vectors.bin", vectors = 200, threads = 4 , 
                        window = 10, iter = 5, negative_samples = 10, force = TRUE)

model %>% closest_to("peace", n = 15)
model %>% closest_to(~"king"+"woman"-"man")
model %>% closest_to(~"bomb" - "war" + "peace")
model %>% closest_to(~"war" - "peace" + "education")
model %>% closest_to(~"berlin" + "france" - "germany" )

peace <- model[rownames(model) == "peace"]
violence <- model[rownames(model) == "violence"]
pv_spectrum <- peace-violence
cosineSimilarity(pv_spectrum, model[["prize"]])
cosineSimilarity(pv_spectrum, model[["war"]])
cosineSimilarity(pv_spectrum, model[["attack"]])
cosineSimilarity(pv_spectrum, model[["bomb"]])
cosineSimilarity(pv_spectrum, model[["man"]])

violation <- model %>% 
  closest_to(~ "violation"-"rights",n=Inf) 
war <- model %>% 
  closest_to(~ "war" - "peace", n=Inf)
politics <- model %>%
  closest_to("politics", n = 100)

politics %>%
  inner_join(violation) %>%
  inner_join(war) %>%
  ggplot() + 
  geom_text(aes(x=`similarity to "violation" - "rights"`,
                y=`similarity to "war" - "peace"`,
                label=word)) +
  labs(x = "Rights <----> Violation", y = "Peace <-----> War")

peacewords <- model %>% closest_to("peace", n = 50)
peace <- model[[peacewords$word,average=F]]
plot(peace,method="pca")

plot(model,perplexity=50)

## GloVe
library(text2vec)
glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(nobel_fcm, n_iter = 10, convergence_tol = 0.01, n_threads = 8)
word_vectors = wv_main + t(glove$components)

x <- word_vectors["war", , drop = FALSE] - 
  word_vectors["bomb", , drop = FALSE] + 
  word_vectors["peace", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = x, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 20)