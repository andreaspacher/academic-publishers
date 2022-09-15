library(tidyverse)
library(rvest)

publisher.name <- "Open Access Text"
publisher.url <- "https://oatext.com/journals.php"

# ========
#
# webscrape
#
# ========

webpage <- read_html(publisher.url)

journalnames <- html_nodes(webpage, "div.j-list-items ul li a") %>%
  html_text() %>%
  unique()

journalurl <- html_nodes(webpage, "div.j-list-items ul li a") %>%
  html_attr("href") %>%
  unique()

journalurl <- paste0("https://oatext.com/", journalurl)

# 12 August 2022: these two have no corresponding URLs:
journalurl <- append(journalurl, c(NA, NA), after = 89)

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