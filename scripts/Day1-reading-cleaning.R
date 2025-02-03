## Monday, February 3 2025
## Quantifying History, NTNU

## Session 1.2: Collecting, reading and cleaning text data

# setwd() if need be

#################################
# Reading in Text Documents     #
#################################

### Reading in txt, doc or pdf files

# to see list of files in your working directory
list.files() 

# list of files in subdirectories of working directory
(books_list <- list.files(path = "data/dickens", full.names = TRUE))

# we now use the package readtext to read in the whole list of documents
# another similar reader is called pdftools
library(readtext)
books <- readtext(books_list)
class(books)


library(tidyverse)
# extract metadata from the filenames, save as tibble
books <- readtext(books_list, docvarsfrom="filenames", dvsep = ' - ', 
                  docvarnames = c('Title', 'Author', 'Year')) |> 
    as_tibble()
books

# saving our tibble as a csv
# write_csv(books, "data/dickens_books.csv")

## Reminder: There are R packages for many document databases such as Project Gutenberg 
## https://cran.r-project.org/web/packages/gutenbergr/vignettes/intro.html.


### PDFs that use columns or other different text layouts

# readtext has trouble with columns. One strategy can be to cut up pdfs and sew
# them back together - see the magick package: https://docs.ropensci.org/magick/articles/intro.html

# Example: Norwegian storting document from the early 1960s
stort_meld<- readtext('data/stmeld8_kongo.pdf')
# returns a df
stort_meld
# print with formatting 
cat(stort_meld$text)
# print plain text, first 1000 characters ("substring")
str_sub(stort_meld$text, 1, 1000) 

# tabulapdf does much better though it takes a couple steps to install
# see: https://docs.ropensci.org/tabulapdf/

library(tabulapdf)
file <- 'data/stmeld8_kongo.pdf'
numpages <- get_n_pages(file)
stort_meld_2 <- extract_text(file, encoding = 'UTF-8', pages=1:numpages)
# what is the datastructure here?
str(stort_meld_2)
cat(stort_meld_2[1])
str_sub(stort_meld_2[1], 1, 1000) 

# reading tables (requires also packages shiny and miniUI be loaded)
extract_areas("data/sustainability-report-2001-equinor.pdf", 44)

# If trouble installing tabulapdf on local computer: https://posit.cloud


### Non-machine readable PDFs (hardest and probably most likely for historians)

# To recognize characters we need to do Optical Character Recognition (OCR) 
# We will use a package called tesseract, an open source Google project 
# https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html
# List of language training data: 
# https://tesseract-ocr.github.io/tessdoc/Data-Files-in-different-versions.html)).

## Example: 1925 document from Norwegian parliament

# First download and load Norwegian OCR model
library(tesseract)
tesseract_download('nor') # this need be done only once
norsk <- tesseract(language='nor')
# Now convert pdf to image format - here we use png
png_archivedoc <- pdftools::pdf_convert("data/Storting_KongHaakon_1925.pdf", dpi = 600)
text <- ocr(png_archivedoc, engine=norsk)
cat(text)


#####################
# Cleaning text     #
#####################

# Often collecting, digitizing and cleaning documents is the majority of work
# Less exciting but important.

nobel <- read_csv("data/NobelPeace.csv", locale=locale(encoding = "UTF-8"))
oil_sr <- read_csv("data/srps.csv")


### Viewing your tibble corpus
nobel # note that this tells you the datatype of each column
tail(nobel, n = 10) # and head
print(nobel, n = Inf)
glimpse(nobel)
View(nobel)

# view just one awardspeech
nobel$AwardSpeech[3]
cat(nobel$AwardSpeech[3])


### White spaces, urls, tags, etc.

# Escape characters: \t, \n, \r (https://en.wikipedia.org/wiki/Escape_character).
test <- "     Here \n there \t\t\t and \n\n\r everywhere.\n"
print(test)
cat(test)

# Practice cleaning on our small test string

test_clean <- str_replace_all(test, "[(\n)(\r)(\t)]", ' ')
test_clean <- str_replace_all(test_clean, " {2,}", " ")
test_clean <- str_replace_all(test_clean, "^ *| *$", "")
test_clean
# regex can be visualized using internet tools: https://regexr.com/

# encoding can be difficult : https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/


# tidyverse has built-in tools for this
str_squish(test)
str_to_lower(test)

str_squish(str_to_lower(test))

# but better, easier to read:
nobel$AwardSpeech[1] |> 
  str_squish() |> 
  str_to_lower()

# do this for whole corpus
nobel <- nobel |> 
  mutate(clean_text = str_to_lower(AwardSpeech)) |> 
  mutate(clean_text = str_squish(clean_text))
View(nobel)

# reduce and rename columns
nobel <- nobel |> 
  select(Year, Laureate, clean_text) |> 
  rename(Year = Year, Laureate = Laureate, AwardSpeech = clean_text)  # renaming cols

# other problems in the corpus? Ex. beginning of speeches starting in 1971.
View(nobel)

# remove the formulaic opening "Your majesty.... ladies and gentlemen,
str_replace(nobel$AwardSpeech[45], "^.{5,800}gentlemen[,:\\.] ", "") 
# do to all 
nobel <- nobel |> 
  mutate(AwardSpeech = str_replace(AwardSpeech, "^.{5,800}gentlemen[,:\\.] ", ""))
View(nobel)

# save our cleaned text
write_rds(nobel, "data/nobel_cleaned.Rds")

#More information on regular expressions:
# - https://www.princeton.edu/~mlovett/reference/Regular-Expressions.pdf
# - https://r4ds.hadley.nz/, chapter 14.

## Exercises
# The corpus for nobel still isn't perfectly clean. 
#     - What other issues do you see and how might they be fixed?
#     - Are the problems the same with the SR report corpus? Take a look and see if 
# we can run the same code or we need to do different things to clean that corpus.

###########################
# Manipulating dataframes #
###########################

# subsetting

# select only certain years
nobel |>
  filter(Year >= 1950 & Year <= 1980) # also possible: | and !
nobel |>
  filter(Year == 1950 | Year == 1980) # returns rows for 1950 and 1980
nobel |>
  filter(Year >= 1950 & Year <= 1954 & Year != 1953) # returns rows >= 1950, <=1954 and not 1953

# new columns
nobel  |> 
  mutate(after_WWII = Year > 1945)
# do this to create a new column for word count
nobel <- nobel |>
  mutate(wc = str_count(AwardSpeech, '[\\w]+'))

# inspect this column
nobel$wc

# make a plot of word counts
nobel |>
  ggplot(aes(x = Year, y = wc)) +
  geom_line() # geom_point() would also show missing years
nobel |> 
  ggplot(aes(x = Year, y = wc)) +
  geom_col() 
# what is the problem with the line graph?
nobel |> 
  complete(Year = seq(min(Year), max(Year))) |> 
  ggplot(aes(x = Year, y = wc)) +
    geom_line()

# inspect the recent low count speeches - error here?
nobel |> 
  filter(wc < 500)

sum(nobel$wc) # total word count
mean(nobel$wc) # average speech word count

# to compare different decades
nobel |>
  mutate(decade = (Year %/% 10) * 10) |> # uses something called modulo division to get the decade
  group_by(decade) |>
  summarize(mean(wc))
nobel |>
  mutate(period = case_when(
    Year <= 1945 ~ "Pre-cold war",
    Year > 1945 & Year <= 1991 ~ "Cold War",
    Year > 1991 ~ "Post-cold war")) |>
  group_by(period) |>
  summarize(mean(wc))

# how many speeches per decade
nobel |>
  mutate(decade = (Year %/% 10) * 10) |>
  group_by(decade) |>
  summarize(n())

# google n-grams style plots
nobel |>
  mutate(peace = str_count(AwardSpeech, "peace")) |>
  mutate(war = str_count(AwardSpeech, "war")) |>
  mutate(human_rights = str_count(AwardSpeech, "human rights")) |>
  #group_by(Year) |>
  #summarize(peace = sum(peace), war = sum(war), human_rights = sum(humright)) |>
  pivot_longer(c("peace", "war", "human_rights"), names_to = "word", values_to = "counts") |>
  ggplot(aes(x = Year, y = counts, color = word)) +
    geom_line()

# as a proportion of total words
nobel |>
  mutate(peace = str_count(AwardSpeech, "peace") / wc) |>
  mutate(war = str_count(AwardSpeech, "war") / wc) |>
  mutate(human_rights = str_count(AwardSpeech, "human rights") / wc) |>
  #group_by(Year) |>
  #summarize(peace = sum(peace), war = sum(war), human_rights = sum(humright)) |>
  pivot_longer(c("peace", "war", "human_rights"), names_to = "word", values_to = "counts") |>
  ggplot(aes(x = Year, y = counts, color = word)) +
  geom_line()

# we might want to smooth this out -- we will look at ways to do this later in the week

# another example with a different corpus
oil_sr
table(oil_sr$Year, oil_sr$Company)

# n-grams using oil_sr
oil_sr |>   
  mutate(wc = str_count(Text, '[\\w]+')) |> 
  mutate(wind = str_count(Text, "wind")) |>
  mutate(oil = str_count(Text, "oil")) |>
  mutate(nuclear = str_count(Text, "nuclear")) |>
  group_by(Year) |>
  summarize(wind = sum(wind), oil = sum(oil), nuclear = sum(nuclear)) |>
  pivot_longer(c("wind", "oil", "nuclear"), names_to = "word", values_to = "counts") |>
  ggplot(aes(x = Year, y = counts, color = word)) +
  geom_line()


## Excercises
#- Make other n-grams of your choice of the Nobel corpus. Anything surprising?
#- Chart a word over time per company with the SR corpus.


##################
# Dates          #
##################

# there are advantages to treating dates as a class, especially when we have more than just years

library(lubridate)
now() # class is POSIXct
today() # class is Date
(today <- "2025-01-18")
(day <- ymd(today)) # lubridate
class(today); class(day)

# Date class means we can do arithmetic
today + 1
day + 1
# with date-time 
now <- now()
now
now + 1

# changing strings to dates
ymd("2024-12-25")
mdy("12/25/2024") # American standard
dmy("25.12.2024") # lubridate will also recognize various possible separators
dmy("21st of June in the year of our lord 2024") 

# Sys.setlocale("LC_ALL", "nb_NO.utf8")
dmy("den 2. februar 2019") # and multilingual, though this will depend on your language locale (Sys.getlocale (category = "LC_ALL"))

t <- now() 
year(t)
month(t)
day(t)
week(t)
wday(t, label = TRUE, abbr = FALSE)

date <- "18-5-2005"
date <- dmy(date)
wday(date, label = TRUE)

# intervals
today() - date

# https://lubridate.tidyverse.org/ for much more