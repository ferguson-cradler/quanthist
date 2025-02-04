## Wednesday, February 5 2025
## Quantifying History workshop, NTNU

## Session 3.2 -- Vectorization and topic modelling

# setwd() if need by

Sys.setlocale("LC_ALL", "en_US.utf8")
nobel <- read_rds("data/nobel_cleaned.Rds")

### Co-occurrence 

library(widyr)

# co-occurrence within speeches
nobel |>
  unnest_tokens(word, AwardSpeech) |>
  filter(!word %in% stop_words$word) |>
  pairwise_count(word, Year, upper = FALSE, sort = TRUE)

cooccur <- nobel |> 
  unnest_tokens(sentences, AwardSpeech, token = "sentences") |>
  mutate(sentences = str_to_lower(sentences)) |>
  mutate(sentence = 1:n()) |>
  unnest_tokens(word, sentences) |> 
  filter(!word %in% stop_words$word)  |> 
  pairwise_count(word, sentence, upper = TRUE, sort = TRUE) |> 
  filter(item1 == "equality" & item2 == "peace")

# there are collocation measures in the quanteda package also but it can be hard to know
# how to interpret them

#######################
# Vectorization       #
#######################

### what is a document term matrix?

### what is a cosine distance?

####### Cosine distance

library(quanteda)
library(quanteda.textstats)

# creating a dfm (dtm)
nobel |> 
  corpus(text_field = 'AwardSpeech') |> 
  quanteda::tokens(remove_numbers = TRUE, remove_punc = TRUE) |> 
  dfm()

# cosine distance by decade
nobel_decade <- nobel |>
  mutate(decade = Year %/% 10 * 10) 

nobel_dfm <- nobel_decade |>
  corpus(text_field = 'AwardSpeech') |>
  quanteda::tokens(remove_numbers = TRUE, remove_punc = TRUE) |>
  dfm() |>
  dfm_remove(pattern = stop_words$word) |>
  dfm_group(groups = decade)

cosine_diff <- textstat_simil(nobel_dfm, method = "cosine")

## Heat map
tab <- as_tibble(as.matrix(cosine_diff))
tab['Decade'] <- nobel_dfm@Dimnames$docs
tab[lower.tri(tab, diag = FALSE)] <- NA

tot_gath <- gather(tab, 1:as.integer(ncol(tab)-1), key = 'to', value = 'cosine')
tot_gath <- tot_gath |>
  mutate(cosine = round(cosine,2))
tot_gath |>
  #filter(cosine < .99) |>
  ggplot(aes(Decade, to)) +
  geom_tile(aes(fill = cosine)) +
  scale_fill_continuous("",limits=c(.3, 1), breaks=seq(.3,1,by=0.2), low = "white", high = "blue", na.value = "white") +
  theme_bw() +
  geom_text(aes(label = format(cosine, nsmall=1)), color = 'white') +
  theme(axis.text.x=element_text(angle=0), axis.ticks=element_blank(), axis.line=element_blank(), panel.border=element_blank(),
        panel.grid.major=element_line(color='#eeeeee')) +
  #scale_y_discrete(position = "right") +
  labs(x = '', y = '', title = 'Cosine Distances', subtitle = 'Similarity of responses by rating of EVs')

# editing the dfm
dfm_small <- dfm_trim(nobel_dfm, min_termfreq = 3, min_docfreq = 2)

# clustering
eu_dist <- textstat_dist(dfm_weight(dfm_small, scheme = "prop"))
# hiarchical clustering using Euclidean distance -- hclust()
cluster <- hclust(as.dist(eu_dist))
cluster$labels <- docnames(nobel_dfm)
# plot as a dendrogram
plot(cluster, xlab = "", sub = "", main = "Clustered Euclidean Distance")

library(quanteda.textplots)
# Keyness
keyness <- textstat_keyness(nobel_dfm, target = nobel_dfm$decade >= 1945)
textplot_keyness(keyness)


#######################
# Topic models        #
#######################

nobel_stemmed <- nobel |>
  unnest_tokens(output = words, input = AwardSpeech) |>
  anti_join(stop_words, by = c("words" = "word")) |>
  mutate(word_stem = wordStem(words))
#rename(Year = Year, Laureate = Laureate, word = word_stem)

# transform dataframe to DTM
nobel_dtm <- nobel_stemmed |>
  group_by(Year) |>
  count(word_stem, sort = TRUE) |>
  cast_dtm(Year, word_stem, n)

k <- 10
alpha <- .1
nobel_tm <- LDA(nobel_dtm, k = k, control = list(alpha = alpha))

terms(nobel_tm, 15)

# go back to tidy to look at this -- beta distribution
terms <- tidy(nobel_tm, matrix = "beta")
words_in_topics <- terms |>
  group_by(topic) |>
  slice_max(beta, n = 10) |> 
  ungroup() |>
  arrange(topic, -beta)
words_in_topics |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

## -- theta dist
topics_in_documents <- tidy(nobel_tm, matrix = "gamma")
(auto_topics <- apply(terms(nobel_tm, 3), 2, paste, collapse = "-"))
(auto_topics <- tibble(old_topic = 1:k, new_topic = auto_topics))
(topics <- topics_in_documents |>
    left_join(auto_topics, by=c("topic" = "old_topic")))

topics |>
  filter(document %in% c(1977, 1985, 1996)) |>  # the documents we want to compare
  ggplot(aes(new_topic, gamma, fill = document)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ document, ncol = 3)

topics |>
  ggplot(aes(document, gamma)) +
  geom_col(aes(group = new_topic, fill = new_topic)) +
  scale_x_discrete(breaks = seq(1905, 2019, 10))

topics |>
  #filter(str_detect(new_topic, "war")) |>
  ggplot(aes(document, gamma)) +
  geom_line(aes(group = new_topic, color = new_topic)) +
  #geom_line(aes(group = new_topic, color = new_topic)) +
  scale_x_discrete(breaks = seq(1905, 2019, 10)) +
  facet_wrap(~new_topic)

#### Structural topic model

library(stm)
nobel_decade <- nobel |>
  mutate(decade = Year %/% 10 * 10) |>
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII")) |>
  corpus(text_field = 'AwardSpeech') |>
  quanteda::tokens(remove_numbers = TRUE, remove_punc = TRUE) |>
  dfm() |>
  dfm_remove(pattern = stop_words$word) |>
  dfm_group(groups = decade)
fit10 <- stm(nobel_decade, K = 10, max.em.its = 5, init.type = "Spectral")
plot(fit10)

                ############# Show more ways to visualize stm ##########


nobel_periods <- nobel |>
  #mutate(decade = Year %/% 10 * 10) |>
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII")) |>
  corpus(text_field = 'AwardSpeech') |>
  quanteda::tokens(remove_numbers = TRUE, remove_punc = TRUE) |>
  dfm() |>
  dfm_remove(pattern = stop_words$word)
# setting EM iterations to 10 for speed -- normally this should be many more (often set at 75)
fit10_period <- stm(nobel_periods, K = 10, content =~Period, prevalence =~ Period, max.em.its = 5, init.type = "Spectral")
plot(fit10_period)


