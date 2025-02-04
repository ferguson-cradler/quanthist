## Tuesday, February 4 2025
## Quantifying History, NTNU

## Session 2.2 - Word frequency, dictionary methods, correlation/collocation analysis

rm(list=ls())
# setwd() as needed

Sys.setlocale("LC_ALL", "en_US.utf8")
nobel <- read_csv("data/NobelPeace.csv")

library(tidyverse)

nobel <- nobel |> 
  mutate(clean_text = str_to_lower(AwardSpeech)) |> 
  mutate(clean_text = str_squish(clean_text)) |> 
  select(Year, Laureate, clean_text) |> 
  rename(Year = Year, Laureate = Laureate, AwardSpeech = clean_text)

##########################
# Keywords in context    #
##########################

library(quanteda)
# tokenize
tokens_nobel <- corpus(nobel, text_field = "AwardSpeech") |>  
  quanteda::tokens()

# kwic - keywords in context
keywords <- kwic(tokens_nobel, pattern = "peace", window = 5)
keywords[1:15]

# view with covariables
covars <- paste(tokens_nobel$Year, tokens_nobel$Laureate, sep = ": ") # list of covariables
docnames(tokens_nobel) <- covars 
peace_kwic <- kwic(tokens_nobel, pattern = phrase("peace movement"), valuetype = "glob", window = 50) |>
  as_tibble() |>
  unite(col = "KWIC", pre:post, sep = " ** ") |>
  mutate(KWIC = paste(docname, KWIC, sep = " -- "))
head(peace_kwic$KWIC)

# export for easier reading
library(writexl)
write_xlsx(peace_kwic, "data/peace_kwic.xlsx")

####################################################
# Tokenization, stopwords, stemming/lemmatizing    #
####################################################

### Tokenization

## Tidy data principles: 
# There are three interrelated rules which make a dataset tidy:
#   - Each variable must have its own column.
#   - Each observation must have its own row.
#   - Each value must have its own cell.

# if we are counting words then each word is an observation
library(tidytext)
nobel |> 
  unnest_tokens(output = words, input = AwardSpeech)

### Stopwords

stop_words # english stopwords from tidytext
table(stop_words$lexicon)
# other stopword lists
library(stopwords)
stopwords("no")
stopwords("ru")
stopwords("zh", source = "stopwords-iso")
# there are multiple sources for stopwords
stopwords_getsources()
stopwords_getlanguages("snowball")

# remove stopwords from nobel corpus
nobel_tidy <- nobel |> 
  unnest_tokens(output = words, input = AwardSpeech) |> 
  anti_join(stop_words, by = c("words" = "word"))  # by= specifies which columns to use, had they been named the same thing we could have omitted it
# adding our own words to the stopword list
my_words <- c("peace", "war")
custom_stop_words <- tibble(word = my_words, lexicon = "my_customization")
stop_words_custom <- rbind(stop_words, custom_stop_words)
tail(stop_words_custom) # view the end of the tibble, look like our words were added correctly
# using stopwords package
stop_en <- tibble(word = stopwords("en"))
nobel |> 
  unnest_tokens(output = words, input = AwardSpeech) |> 
  anti_join(stop_en, by = c("words" = "word"))

### Stemming/lemmatizing

# stemming
library(SnowballC)
words_to_stem <- c("going", "represented", "wars", "similarity", "books", "went")
SnowballC::wordStem(words_to_stem)

# lemmatizing
library(textstem)
data("hash_lemmas", package = "lexicon")
lemmatize_words(words_to_stem,dictionary = hash_lemmas) # lemmatize_strings also exists

##########################
# Word frequency charts  #
##########################

# quick and dirty word count
nobel_tidy |> 
  count(words, sort=TRUE)

# lets now apply all we've learned and then some and make two side by side 
#    charts of top word frequency

nobel  |> 
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII"))  |>
  mutate(Period = factor(Period, levels = c("Pre-WWII", "Post-WWII"))) |>
  unnest_tokens(output = words, input = AwardSpeech, stopwords = stop_words$word) |> 
  mutate(lemma = lemmatize_words(words, dictionary = hash_lemmas)) |> 
  group_by(Period) |>
  count(lemma, sort=TRUE) |> 
  mutate(proportion = n / sum(n) * 1000) |>                    
  #slice_max(order_by=proportion, n = 15) %>%                    
  top_n(15) |> 
  ggplot(aes(reorder_within(x = lemma, by = proportion, within = Period), proportion, fill = Period)) +
  geom_col() +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~Period, ncol = 2, scales = "free") +
  labs(x = "Word", y = "Frequency per 1000 words", title = "Word frequencies in the Nobel Peace Prize corpus") +
  theme_bw() +
  guides(fill="none")


#########################
# Word clouds           #
#########################

library(wordcloud)
library(wordcloud2)
nobel_tidy %>%
  count(words, sort=TRUE) %>%
  with(wordcloud(words, n, max.words = 80))

dat <- nobel_tidy %>%
  count(words, sort=TRUE) %>%
  mutate(word = words) %>%
  mutate(freq = n) %>%
  select(word, freq) %>%
  top_n(200)
wordcloud2(dat, size = 1) 

# both of these have numerous options that can be customized


#######################
# TF-IDF              #
#######################

# what is tf-idf and why might it make sense?

keyword <- "human right"
term_freq <- sum(str_count(nobel$AwardSpeech, keyword))
total_docs <- dim(nobel)[1]
total_docs_mentioning_keyword <- nobel |>  
  mutate(word = str_count(AwardSpeech, keyword)) |> 
  filter(word > 0) |> 
  count()
(1 + log(term_freq)) * log(total_docs / total_docs_mentioning_keyword[[1]])

# graphing tf-idf
nobel  |> 
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII")) |> 
  unnest_tokens(words, AwardSpeech) |> 
  count(words, Period, sort = TRUE) |> 
  bind_tf_idf(words, Period, n) |> 
  arrange(desc(tf_idf)) |> 
  mutate(Period = factor(Period, levels = c("Pre-WWII", "Post-WWII"))) |> 
  group_by(Period) |> 
  slice_max(tf_idf, n = 15)  |> 
  ungroup() |> 
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = Period)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Period, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


#########################
# n-grams in the corpus #
#########################

nobel |> 
  unnest_tokens(bigram, AwardSpeech, token = "ngrams", n = 2)

nobel |> 
  unnest_tokens(bigram, AwardSpeech, token = "ngrams", n = 2) |> 
  count(bigram, sort = TRUE)

# removing stopwords from bigrams
top40_bigrams <- nobel %>%
  unnest_tokens(bigram, AwardSpeech, token = "ngrams", n = 2) |> 
  separate(bigram, into=c("word1", "word2"), sep = " ") |>
  filter(!word1 %in% stop_words$word) |>
  filter(!word2 %in% stop_words$word) |>
  unite(bigram, word1, word2, sep = ' ') |>
  count(bigram, sort=TRUE) |>  
  top_n(40)
print(top40_bigrams, n=Inf)

# binding bigrams together
top40_bigrams <- top40_bigrams |> 
  mutate(bigram_split = bigram) |> 
  separate(bigram_split, into=c("word1", "word2"), sep = " ") |> 
  unite(bigram_fused, word1, word2, sep = "_")
# replacing two words with bigrams to be considered as one word
nobel$AwardSpeech_bigrams <- str_replace_all(nobel$AwardSpeech, 
  setNames(as.vector(top40_bigrams$bigram_fused), as.vector(top40_bigrams$bigram)))

topwords <- nobel |> 
  unnest_tokens(output = words, input = AwardSpeech_bigrams) |> 
  anti_join(stop_words, by = c("words" = "word")) |> 
  count(words, sort=TRUE)

print(topwords, n = 20)

#############################
# Tagging                   #
#############################

nobel_uncleaned <- read_csv("data/NobelPeace.csv") |> 
  mutate(AwardSpeech = str_squish(AwardSpeech)) |> 
  select(Year, Laureate, AwardSpeech)

### Named entity recognition
library(entity)
location_entity(nobel_uncleaned$AwardSpeech[84])

nobel_locations <- nobel_uncleaned |> 
  mutate(locations = location_entity(AwardSpeech)) 
nobel_locations |>   
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII"))  |>  
  mutate(Period = factor(Period, levels = c("Pre-WWII", "Post-WWII"))) |> 
  unnest(locations)|> 
  group_by(Period) |>                                            
  count(locations, sort=TRUE) |> 
  mutate(proportion = n / sum(n) * 1000) |>                    
  #slice_max(order_by=proportion, n = 15) %>%                    
  top_n(15) |> 
  ggplot(aes(reorder_within(x = locations, by = proportion, within = Period), proportion, fill = Period)) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~Period, ncol = 2, scales = "free") +
  labs(x = "Word", y = "Frequency per 1000 words", title = "Location frequencies in the Nobel Peace Prize corpus") +
  theme_bw() +
  guides(fill="none")

# also person and organization entity, though they are far from perfect
person_entity(nobel_uncleaned$AwardSpeech[4])
organization_entity(nobel_uncleaned$AwardSpeech[10])

### POS tagging
library(udpipe)
# need to download a model
pos_model <- udpipe_download_model("english-gum") # needs to be done only once
pos_model <- udpipe_load_model(file = pos_model$file_model)

# as example, show POS for first sentance in the corpus
library(tokenizers)
sample_sentence <- tokenize_sentences(nobel_uncleaned$AwardSpeech[1])[[1]][[1]]
tagged_pos <- udpipe_annotate(pos_model, x = sample_sentence)
as.data.frame(tagged_pos, detailed=TRUE)

# count most frequent nouns
nobel_pos <- udpipe_annotate(pos_model, x = nobel$AwardSpeech, doc_id = nobel$Year) # this will take a while
nobel_pos_df <- as.data.frame(nobel_pos, detailed=TRUE) |> 
  mutate(Year = as.numeric(doc_id))
# plot chart of top nouns
nobel_pos_df |> 
  filter(upos == "NOUN") |> 
  count(lemma, sort=TRUE) |>  
  top_n(15) |>                 
  mutate(lemma = reorder(lemma,n)) |>  
  ggplot(aes(lemma, n)) + 
  geom_col() + 
  coord_flip()

# other possibilities - plot noun, verb and adj frequency over time
nobel_pos_df |> 
  group_by(Year) |> 
  summarize(word_count = n(), nouns = sum(upos == "NOUN"), verbs = sum(upos == "VERB"),
            adj = sum(upos == "ADJ")) |> 
  mutate(nouns = nouns/word_count, verbs = verbs/word_count, adj = adj/word_count) |> 
  select(!word_count) |> 
  pivot_longer(!Year, names_to = "POS", values_to = "Count") |> 
  ggplot() +
    geom_line(aes(x=Year, y=Count, group = POS, color = POS)) +
    scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(1905,2025,10))

################################
# Sentiment analysis           #
################################

# dictionaries with tidytext
get_sentiments("afinn") # other dictionaries are "bing" and "nrc"
table(get_sentiments("afinn")$value)
summary(get_sentiments("afinn")$value)

# calculating text sentiment by subtracting total positive sentiment words from total negative with bing lexicon
nobel %>%
  unnest_tokens(word, AwardSpeech) %>%  ## we call our new column "word" which makes inner_joins easier 
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, year = Year) %>% 
  # want to subtract positive from negative so need these in two columns
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(year, sentiment)) +
    geom_line(show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = 2, alpha = .8)

# calculate the same from afinn, with total sentiment the mean of sentiment dictionary values
nobel %>%
  unnest_tokens(word, AwardSpeech) %>%  ## we call our new column "word" which makes inner_joins easier 
  inner_join(get_sentiments("afinn")) %>%
  group_by(Year) |> 
  summarize(sentiment = mean(value)) |> 
  ggplot(aes(Year, sentiment)) +
    geom_line(show.legend = FALSE) +
    geom_hline(yintercept = mean(get_sentiments("afinn")$value), linetype = 2, alpha = .8)

#######################
# Dictionaries        #
#######################

# what is a dictionary?

war_dict <- read_lines("data/war.txt")
war_dict <- tibble(word = war_dict, dictionary = "war")
war_dict <- war_dict |> 
  filter(word != "")
rights_dict <- tibble(word = read_lines("data/rights.txt"), dictionary = "rights")  |>
  filter(word != "")
war_dict; rights_dict
dict <- rbind(rights_dict, war_dict)

nobel |>
  unnest_tokens(output = words, input = AwardSpeech) |> 
  inner_join(dict, by=c("words"="word")) |> 
  count(Year, dictionary) |> 
  ggplot() +
  geom_line(aes(x=Year, y=n, color=dictionary))

