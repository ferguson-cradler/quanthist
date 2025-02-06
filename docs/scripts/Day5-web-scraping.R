## Friday, February 7 2025
## Quantifying History workshop, NTNU


#######################################################
## Session 5.2 -- Brief introduction to web scraping
#######################################################

#### First example -- it might be very easy to just immediately download a bunch of pdfs
# Ex: https://www.bundestag.de/protokolle

download.file("https://dserver.bundestag.de/btp/20/20209.pdf", "bundestag.pdf", mode = "wb")

# can we guess how these urls change?

# first, look at robot.txt
# one option: use install.packages("polite")

## "loop" through all possible addresses
list = c()
for (term in 1:1) {
  for (session in 281:290) {
    term_two <- formatC(term, width = 2, format = "d", flag = "0")
    session_three <- formatC(session, width = 3, format = "d", flag = "0")
    url <- paste0("https://dserver.bundestag.de/btp/", term_two, "/", term_two, session_three, ".pdf")
    print(paste("Downloading:", url))
    #list <- c(list, url)
    tryCatch({
      download.file(url, paste0("bundestag/", term_two, session_three, ".pdf"),mode = "wb")
    },
      warning = function(cnd) {
        message(paste("URL caused a warning:", url))
        message("Here's the original warning message:")
        message(conditionMessage(cnd))
        break
      }
    )
  }
}

####

library(rvest)

# practice on sample

test <- read_html("data/test.html")
# find p's
test |>  html_elements("p")
# find class import
test |> html_elements(".special") |> html_text2()
test |> html_elements("#two") |> html_text()

# Real world example: nobel prize speeches

# first we google to find where they are 
# (eg: https://www.nobelprize.org/prizes/peace/2018/ceremony-speech/)

# lets scrape one page

url <- "https://www.nobelprize.org/prizes/peace/2018/ceremony-speech/"
speech <- read_html(url) # reads in the html for the page

# 
speech |>  html_elements(".smalltext") |> 
  html_text() 

speech |> html_elements(".smalltext") |> html_children()

# Web browser go to the menu button -> More tools -> Web Developer Tools.

speech |> html_elements(".nobel-content") |>  html_text2()

## extract text
speech |> html_elements(".smalltext") |> html_text2()

ps <- speech |> html_nodes('p')

speech |> 
  html_nodes('p:not([id]):not([class])') 

text <- speech |> 
  html_elements('article.page-content.border-top.entry-content') |> 
  html_nodes('p:not([id]):not([class])') |> 
  html_text()
# remove final row that is citation
text <- text[-length(text)] |> 
  paste(collapse = " ")

## extract date
Sys.setlocale("LC_ALL", "en_US.utf8")
library(lubridate)

date <- speech |> 
  html_element(".smalltext") |> 
  html_text2() |> 
  dmy()

  
## extract year
# just for demonstration purposes
url
year <- str_extract(url, "[0-9]{4}") |> as.numeric()

## extract laureate
laureate <- speech |> 
  html_nodes(xpath='/html/body/main/div/div/section[1]/aside/nav/ul/li[2]/ul/li/a') |> 
  html_text2() |> 
  paste(collapse = "; ")


## put it all together
tibble_2018 <- tibble(Date = date, Year = year, Laureate = laureate, AwardSpeech = text)

## now put this in a loop to scrape all of them 

# make a list of url addresses
# this could be in the main loop, but for demonstration purposes separate
urls <- vector()
for (i in 1905:2024){
  new_url <- paste0("https://www.nobelprize.org/prizes/peace/", i, "/ceremony-speech/")
  urls <- append(urls, new_url)
}
