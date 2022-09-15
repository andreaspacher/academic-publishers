library(tidyverse)
library(rvest)

publisher.name <- "Peertechz Publications"
publisher.url <- "https://www.peertechzpublications.com/journals"

# ========
#
# webscrape
#
# ========

webpage <- read_html(publisher.url)

journalnames <- html_nodes(webpage, "div.caption > h5 > img") %>%
  html_attr("alt") %>%
  unique() %>%
  str_squish()

journalurl <- html_nodes(webpage, "div.pz-journal-btns > a.btn.btn-primary:first-child") %>%
  html_attr("href") %>%
  unique()

journalissn <- html_nodes(webpage, "p.pz-mini-purple") %>%
  html_text()
journalissn <- str_remove(journalissn, "ISSN: ")
journalissn <- unique(journalissn)

journalissn[96:101] <- NA

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
