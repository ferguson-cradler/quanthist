## Friday, February 7 2025
## Quantifying History workshop, NTNU


#######################################################
## Session 5.2 -- Brief introduction to web scraping
#######################################################

# our example will be the corpus we have been working with all week, the Nobel 
# peace prize award speeches

# first we google to find where they are 
# (eg: https://www.nobelprize.org/prizes/peace/2018/ceremony-speech/)

# lets scrape this one and its meta information then see how we can do this for a 
# list of documents

library(rvest)

url <- "https://www.nobelprize.org/prizes/peace/2018/ceremony-speech/"
speech <- read_html(url) # reads in the html for the page

# 

So we have it? Well, kind of. We have "it" but we also have a whole lot more. There's a lot of information on this page, much of which we don't need or want to collect. The menus, the other related stories, and so on. Plus, what we get with R is not just what we see but a whole ton of tags and definitions that are intended for our browsers, not for us. We maybe want the text, headline, author, and the date. So how do we get this information, no more no less?
  
  We'll need to go into the actual HTML code and find the information and then tell R where to get it. Luckily this is a bit easier than it sounds. In your web browser go to the menu button -> More tools -> Web Developer Tools.

You'll get a panel of web developer tools. On the top menu line of the panel is an icon (circled in the image below) for selecting an element in the page.

```{r, echo=FALSE, fig.align="center", fig.cap = "This will look slightly different in different browsers."}
knitr::include_graphics("data/nrk_tools.jpg")
```

With this selected, you now want to find the parts of the page you want to extract. When you go to the journalists' names and hover over with your mouse, you will see a box above with ``a.author__name`` written in it. That's the html tag telling the browser how to display the author's names and that's what we'll tell R to extract.

```{r}
authors <- nrk |>
  html_elements('a.author__name') |>
  html_text()
authors
```

We do the same with the thing with the headline, date and text (it might take some experimentation to get this right).

```{r}

headline <- nrk |>
  html_elements('h1.title.title-large.article-title') |>
    html_text()

text <- nrk |>
  html_elements('div.lp_articlebody.text-body.text-body-sans-serif.container-widget-content.nostack.cf') |>
  html_text()
text
```

The date is a little bit trickier (as is the sub-headline). First of all, for me, looking at it the day of, the date is just "i dag". That's not too helpful for posterity. But it's not just that, it's within a ``span`` class that's not going to recognize for various and sundry reasons anyway (you can try it). But if we click on the date and then look in the developer tools panel below (or on the side if its Chrome) it will show us that this span class is itself embedded in a a time tag (one line up from the span with the date-time) which in turn has a date-time. When we hover over this line we now see a box over the date with 'time.datetime-absolute.datePublished' written. Let's see if this works.

```{r, warnings=FALSE}
library(lubridate)
date <- nrk |>
  html_element('time.datetime-absolute.datePublished') |>
  html_text()
date
```

This has it, but more than we want. We have some "\n" which is code to create a new line on the page. There might well be a way to fine tune our html_element() parameters with ``rvest`` to get it to extract just the information we want but we can also do this with other R tools. We'll use [``stringr``](https://stringr.tidyverse.org/) (yet another member of the tidyverse). ``stringr`` is a package to search ad manipulate character strings. What we need is something that will extract just the date from the above character string ``time``. To do this we'll think back and remember the tutorial on regular expressions. We'll extract the portion of the string that matches the DDDD-DD-DD pattern, where D are digits.

```{r, warnings = FALSE}
library(stringr)

date <- date |>
  str_extract(pattern = "[0-9]{2}.[0-9]{2}.[0-9]{4}") |> # should be //.
  dmy()

```

So we've now extracted all the information we want. We can put it all together in a dataframe now.

```{r, message=FALSE}
library(tidyverse)
(article <- tibble(Author = authors, Date = date, Headline = headline, Text = text))
```

Note in true tidy form its creating two rows of this dataframe, two "observations", one for each journalist. We got this because we had two authors and they were in a vector of two items (each name). To condense we could concatenate them into one object.

```{r}
(authors <- str_c(authors, collapse = ", ")) # also from stringer, concatenates multiple character objects into one
(article <- tibble(Author = authors, Date = date, Headline = headline, Text = text))
```

### Links to more information

As noted, this is just the barest of introductions. I wanted, however, to at least go through the basics because it's a rich source of possible texts for historians. As of just a few years ago if you wanted to do sophisticated web scraping you really had to turn to Python. And Python perhaps still has the edge, particularly with a library called BeautifulSoup.^[A good, thorough introduction for those interested is @mitchell2018web.] But packages in R have come a long way in the last couple years and you can now do pretty sophisticated scraping in R as well. I attach a few links that will help you get started if you are so interested.

- https://rvest.tidyverse.org/articles/harvesting-the-web.html
- https://github.com/yusuzech/r-web-scraping-cheat-sheet/blob/master/README.md
- https://www.scrapingbee.com/blog/web-scraping-r/


## More scraping (more advanced)

Back when we were scraping stories from the NRK website, we scraped one story. But probably what we'd really like to do is build up a _corpus_ of texts, not just one article but numerous. You could just do the above by hand and have a tibble for every article and then combine multiple tibbles to get one large tibble that had your whole corpus. But there is a much better way and one that will introduce a basic concept of programming called a "for loop".

Assume we want to scrape a bunch of NRK stories. We can put the urls in a vector in R.

```{r}
articles <- c("https://www.nrk.no/sport/grovdal-fullstendig-parkert-i-ol-finalen_-_-det-er-rett-og-slett-litt-vondt-a-se-pa-1.15595563", "https://www.nrk.no/sport/vant-heat-etter-fall-i-siste-runde_-_-umenneskelig-1.15595067", "https://www.nrk.no/vestland/hemmelig-plan-for-statsraad-lehmkuhl_-sommerskuta-dukket-plutselig-opp-i-bergen-1.15595488", "https://www.nrk.no/osloogviken/tiltalt-for-forsettlig-drap-pa-christian-halvorsen-1.15595796") # a random list of the top stories on NRK at the time of writing. Saving as character objects so remember to put the urls in quotes

```

I now have a list of urls and for each item of this list I want to do the same thing. So we will "loop" through this list doing what we just did for the one NRK article to each of the objects of this list. Here is the syntax.

```{r}
corpus <- tibble()  # creating an empty tibble to copy everything into
for (url in articles){ # looping over our list of 4 urls
  nrk <- read_html(url)
  authors <- nrk |>
    html_elements('a.author__name') |>
    html_text()
  authors <- str_c(authors[[1]], collapse = ", ") # also from stringer, concatenates multiple character objects into one
  headline <- nrk |>
    html_elements('h1.title.title-large.article-title') |>
    html_text()
  text <- nrk |>
    html_elements('div.lp_articlebody.text-body.text-body-sans-serif.container-widget-content.nostack.cf') |>
    html_text()
  date <- nrk |>
    html_element('time.datetime-absolute.datePublished') |>
    html_text() |>
    str_extract(pattern = "[0-9]{2}.[0-9]{2}.[0-9]{2}") |>
    dmy()
  article <- tibble(Author = authors, Date = date, Headline = headline, Text = text, URL = url)
  corpus <- rbind(corpus, article) # rbind stands for "row bind". We copy the rows of our new article dataframe to the old corpus dataframe which at the end of the for loop will give us a dataframe called corpus with all the data from our four articles
}
```

In words, we are telling R: "Hey R, I have a vector (i.e. a list) called ``articles``. I want you to go into this vector and look at each object individually. We're going to call these objects ``url`` (note we could call them anything at all, this is just name. Often objects in for lists are called i, sometimes x, etc.) For each individual object in my vector, do what is written in the curly braces (all the steps we went through to scrape one article").

## Scraping the nobel speeches

If we google "nobel prize ceremony speech", we land on a page that looks like [this](). We note that we can navigate to ceremony speeches of other years but this takes a lot of clicking. It's going to take a lot of work to program a bot to do this. BUT! If we look at the url we see that it's standardized with a year. What if we try the same thing with 2017? It gives us exactly the page we want. In fact, it does so for any year since the very earliest years of the prize. (Incidentally, navigating by links on the cite I have trouble accessing them before the 1960s.) This makes it very easy to generate a list of links.

# https://www.nobelprize.org/prizes/peace/1905/ceremony-speech/   This is the link that we need to replicate changing the year for each year between 1905 and 2019. We'll start in 1905 as before that it's not really the speech but something less, we want to compare apples to apples as much as possible
library(rvest)
## https://www.nobelprize.org/prizes/peace/1905/ceremony-speech/
urls <- vector()
for (i in 1905:2019){
  new_url <- paste0("https://www.nobelprize.org/prizes/peace/", i, "/ceremony-speech/")
  urls <- append(urls, new_url)
}

But not all these links exist because the Nobel wasn't given out in all years. If we try to open those years (try, for instance, https://www.nobelprize.org/prizes/peace/1915/ceremony-speech/) There are several ways we could deal with this, we could write down years it wasn't given out and remove them from our list of possible urls. But the other thing we can do is use a "try block" within our loop that tells R precisely that: try this, but if it doesn't work, just got on to the next object in the loop. We're going to do this, and then scrape the page in a similar way to which we scraped nrk.no in the last unit. We're going to be a bit more particular about what we edit out because we want only the the text of the speeches as much as possible, not the additional information (citation info, informational footers, etc) that the site gives us.

```{r, eval = FALSE}
corp <- tibble()
for (url_address in urls){
try(
{
nobel <- read_html(url_address)
text <- nobel |>
html_elements('article.page-content.border-top.entry-content') |>
html_elements('p') |>
html_text() |>
tibble()
footer <- nobel |>
html_elements("footer") |>
html_elements('p') |>
html_text() |>
tibble()
small_text <- nobel |>
html_elements("p.smalltext") |>
html_text() |>
tibble() |>
drop_na()
copy_text <- nobel |>
html_elements("p.copy") |>
html_text() |>
tibble() |>
drop_na()
remove_text <- rbind(footer, small_text, copy_text)
remove <- vector()
for (i in 1:dim(remove_text)[1]){
for (j in 1:dim(text)[1]){
if (text[j,] == remove_text[i,]){
remove <- c(remove, j)
}
}
}
text <- text[-remove, ]
total_text <- ''
for (i in 1:dim(text)[1]) {
total_text <- paste(total_text, str_c(text[i,1]))
}
laureate <- nobel |>
html_elements('li.list-laureate') |>
html_text() |>
str_trim() # trims white space before and after
laureate <- str_c(laureate, collapse = ", ") # also from stringer, concatenates multiple character objects into one
year <- str_extract(url_address, "[0-9]{4}")
temp_tibble <- tibble(Year = year, Laureate = laureate, AwardSpeech = total_text)
temp_tibble
corp <- rbind(corp, temp_tibble)
}
)
}

write.csv(corp, "NobelPeace.csv")
```


