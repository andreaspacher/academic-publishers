library(tidyverse)
library(rvest)

publisher.name <- "Advanced Research Publications"
publisher.url <- "https://www.advancedresearchpublications.com/adr-shop"

# ========
#
# webscrape
#
# ========

webpage <- read_html(publisher.url)

journalnames <- html_nodes(webpage, "div.col-md-8 li > a") %>%
  html_text() %>%
  unique() %>%
  str_squish()

journalurl <- html_nodes(webpage, "div.col-md-8 li > a") %>%
  html_attr("href") %>%
  unique()

fulldata <- data.frame(
  journal = journalnames,
  url = journalurl,
  publisher = publisher.name,
  date = Sys.Date()
)

write_csv(fulldata,
          paste0("Output//Journals//Predatory//",
                 publisher.name,
                 "-",
                 Sys.Date(),
                 ".csv"))
