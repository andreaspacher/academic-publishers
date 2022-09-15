library(tidyverse)
library(rvest)

publisher.name <- "Sciencedomain International"
publisher.url <- "http://www.sciencedomain.org/journals.html"

# ========
#
# webscrape
#
# ========

webpage <- read_html(publisher.url)

journalnames <- html_nodes(webpage, "div.container.py-5 div.col-sm-12.jur-lst > p") %>%
  html_text() %>%
  #unique() %>%
  str_squish()

journalurl <- html_nodes(webpage, "div.container.py-5 div.col-sm-12.jur-lst img") %>%
  html_attr("src") %>%
  #unique() %>%
  str_remove("image/journal/") %>%
  str_remove(".jpg$")
journalurl <- paste0("https://www.journal", journalurl, ".com")

fulldata <- data.frame(
  journal = journalnames,
  url = journalurl,
  publisher = publisher.name,
  date = Sys.Date()
)

fulldata <- fulldata %>% distinct()

write_csv(fulldata,
          paste0("Output//Journals//Predatory//",
                 publisher.name,
                 "-",
                 Sys.Date(),
                 ".csv"))
