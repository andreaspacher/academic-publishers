library(tidyverse)
library(rvest)

publisher.name <- "Herbert Publications"
publisher.url <- "https://www.hoajonline.com/includes/homepages/journalsA-Z.php"

# ========
#
# webscrape
#
# ========

webpage <- read_html(publisher.url)

journalnames <- html_nodes(webpage, "table.table tr td a") %>%
  html_text() %>%
  unique() %>%
  str_squish()

journalurl <- html_nodes(webpage, "table.table tr td a") %>%
  html_attr("href") %>%
  unique()

journalissn <- html_nodes(webpage, "table tr td.col-md-2") %>%
  html_text()

fulldata <- data.frame(
  journal = journalnames,
  url = journalurl,
  issn = journalissn,
  publisher = publisher.name,
  date = Sys.Date()
)

write_csv(fulldata,
          paste0("Output//Journals//Predatory//",
                 publisher.name,
                 "-",
                 Sys.Date(),
                 ".csv"))
