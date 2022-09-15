library(tidyverse)
library(rvest)

publisher.name <- "International Scholars Journals"
publisher.url <- "https://internationalscholarsjournals.org/journals/"

# ========
#
# webscrape
#
# ========

webpage <- session(publisher.url,
                   httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
)

journalnames <- html_nodes(webpage, "dl.journals > dd > a > span:first-child") %>%
  html_text() %>%
  unique() %>%
  str_squish()

journalurl <- html_nodes(webpage, "dl.journals > dd > a") %>%
  html_attr("href") %>%
  unique()
journalurl <- paste0("https://internationalscholarsjournals.org", journalurl)
journalissn <- html_nodes(webpage, "dl.journals > dd > a")
journalissn <- stringr::str_extract(journalissn,
                                    "(?<=ISSN: ).........")

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
