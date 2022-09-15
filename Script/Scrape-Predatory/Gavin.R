library(tidyverse)
library(rvest)

publisher.name <- "Gavin Publishers"
publisher.url <- "https://www.gavinpublishers.com/journals"

# ========
#
# webscrape
#
# ========

webpage <- read_html(publisher.url)

journalnames <- html_nodes(webpage, "div.mdcl-sdr a") %>%
  html_text() %>%
  unique() %>%
  str_squish()

journalurl <- html_nodes(webpage, "div.mdcl-sdr a") %>%
  html_attr("href") %>%
  unique()

journalissn <- stringr::str_extract(journalnames,
                                    "(?<=ISSN: ).........")
journalnames <- stringr::str_remove(journalnames,
                                    "(?=ISSN: ).*")

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
