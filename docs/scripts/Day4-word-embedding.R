## Thursday, February 6, 2025
## Quantifying History workshop, NTNU

## Session 4.2 -- Word embedding

install.packages("word2vec")

## the below are needed to run wordvectors
# install Rtools
install.packages("devtools")
devtools::install_github("bmschmidt/wordVectors")

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidytext)
library(word2vec)

# setwd() if necessary
rm(list=ls())
# setwd() as needed

Sys.setlocale("LC_ALL", "en_US.utf8")

nobel <- read_csv("data/NobelPeace.csv") |> 
  mutate(clean_text = str_to_lower(AwardSpeech)) |> 
  mutate(clean_text = str_squish(clean_text)) |> 
  select(Year, Laureate, clean_text) |> 
  rename(Year = Year, Laureate = Laureate, AwardSpeech = clean_text)


####################
## Word embedding
####################

### word2vec

library(word2vec)

text <- nobel$AwardSpeech

set.seed(11111)
embed <- word2vec(x = text, type = "cbow", dim = 50, iter = 10)
as.matrix(embed)
predict(embed, c("rights", "law"), type = "embedding")
predict(embed, c("rights", "law"), type = "nearest", top_n = 5)

people <- predict(embed, newdata = c("king", "man", "woman"), type = "embedding")
queen <- people["king", ] - people["man", ] + people["woman", ]
predict(embed, newdata = queen, type = "nearest", top_n = 3)

analogy <- c("war", "peace", "education")
analogy <- c("bomb", "war", "peace")
analogy <- c("berlin", "germany", "france")
p <- predict(embed, newdata = analogy, type = "embedding")
ans <- p[analogy[1], ] - p[analogy[2], ] + p[analogy[3], ]
predict(embed, newdata = ans, type = "nearest", top_n = 3)

axis <- c("peace", "war")
#axis <- c("right", "violation")
embed_axis <- predict(embed, axis[1], type = "embedding") - predict(embed, axis[2], type = "embedding")
rownames(embed_axis) <- paste(axis[1], "-", axis[2])
predict(embed, embed_axis, type = "nearest", top_n = 5)

word <- "law"
word <- "water"
word_vec <- predict(embed, newdata = word, type = "embedding")
word2vec_similarity(embed_axis, word_vec, type="cosine")

# doc2vec
year_embed <- nobel |> 
  select(Year, AwardSpeech) |> 
  rename(doc_id = Year, text= AwardSpeech) |> 
  doc2vec(embed, newdata = _)
predict(embed, year_embed[1,], type = "nearest", top_n = 5)
# find vector by year?
year_embed[rownames(year_embed) == "2024",]
predict(embed, year_embed[rownames(year_embed) == "2024",], type = "nearest", top_n = 5)
# compare documents to axes
word2vec_similarity(embed_axis, year_embed[rownames(year_embed) == "2024",], type="cosine")


## Wordvectors
library(wordVectors)

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
peace <- model[[peacewords$word,average=FALSE]]

# PCA dimension reduction
plot(peace,method="pca")
# tsne dimension reduction
#install.packages("tsne")
library(tsne)
plot(model,perplexity=50)

## GloVe
#install.packages("text2vec")
library(text2vec)
glove <- GlobalVectors$new(rank = 50, x_max = 10)
# create new fcm
nobel_fcm <- nobel |> 
  corpus(text_field = 'AwardSpeech') |> 
  quanteda::tokens(remove_numbers = TRUE, remove_punc = TRUE) |> 
  fcm()
# train the GloVe model
wv_main <- glove$fit_transform(nobel_fcm, n_iter = 10, convergence_tol = 0.01, n_threads = 8)
word_vectors = wv_main + t(glove$components)
# war - bomb + peace = ?
x <- word_vectors["war", , drop = FALSE] - 
  word_vectors["bomb", , drop = FALSE] + 
  word_vectors["peace", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = x, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 20)
