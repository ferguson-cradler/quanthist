library(rvest)
Sys.setlocale("LC_ALL", "nb_NO.utf8")

urls <- vector()
for (i in 1905:2024){
  new_url <- paste0("https://www.nobelprize.org/prizes/peace/", i, "/ceremony-speech/")
  urls <- append(urls, new_url)
}

url_address <- url_address[1]

corp <- tibble()
for (url_address in urls){
  try(
    {
      nobel <- read_html(url_address)
      text <- nobel %>%
        html_elements('article.page-content.border-top.entry-content') %>%
        html_elements('p') %>%
        html_text() %>%
        tibble()
      footer <- nobel %>%
        html_elements("footer") %>%
        html_elements('p') %>%
        html_text() %>%
        tibble()
      small_text <- nobel %>%
        html_elements("p.smalltext") %>%
        html_text() %>%
        tibble() %>%
        drop_na()
      copy_text <- nobel %>%
        html_elements("p.copy") %>%
        html_text() %>%
        tibble() %>%
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
        html_nodes(xpath='/html/body/main/div/div/section[1]/aside/nav/ul/li[2]/ul/li/a') |> 
        html_text() |> 
        str_trim()
        #   nobel %>%
        # html_elements('li.list-laureate') %>%
        # html_text() %>%
        # str_trim() # trims white space before and after
      laureate <- str_c(laureate, collapse = ", ") # also from stringer, concatenates multiple character objects into one
      year <- str_extract(url_address, "[0-9]{4}")
      temp_tibble <- tibble(Year = year, Laureate = laureate, AwardSpeech = total_text)
      temp_tibble
      corp <- rbind(corp, temp_tibble)
    }
  )
}

write.csv(corp, "data/NobelPeace.csv")
