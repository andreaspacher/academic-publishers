library(tidyverse)
library(rvest)

publisher.name <- "Open Access Pub"
publisher.url <- "https://openaccesspub.org/openaccess-journals-a-to-z/all"

# ========
#
# webscrape
#
# ========

webpage <- read_html(publisher.url)

journalnames <- html_nodes(webpage, "div.float-sm-left span") %>%
  html_text() %>%
  unique()

journalurl <- html_nodes(webpage, "div.float-sm-right a.btn.btn-primary.btn-sm.active") %>%
  html_attr("href") %>%
  unique()

journalissn <- html_nodes(webpage, "div.float-sm-right a.btn.btn-primary.btn-sm.active") %>%
  html_text()

journalissn <- stringr::str_remove(journalissn, "^ISSN : ")
journalissn <- ifelse(journalissn == "Coming Soon", NA, journalissn)

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
