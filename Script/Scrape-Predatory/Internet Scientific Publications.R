library(tidyverse)
library(rvest)

publisher.name <- "Internet Scientific Publications"
publisher.url <- "http://ispub.com/journals"

# ========
#
# webscrape
#
# ========
# Webscraping neither workd with rvest nor with RSelenium.
# We thus saved the HTML code in the following file:

webpage <- read_html("Script\\Scrape-Predatory\\Internet Scientific Publications.txt")

# ... and we will scrape the data from that file.

journalnames <- html_nodes(webpage, "div.main_content h3 > a") %>%
  html_text() %>%
  unique() %>%
  str_squish()

journalurl <- html_nodes(webpage, "div.main_content h3 > a") %>%
  html_attr("href") %>%
  unique()
journalurl <- paste0("https://ispub.com", journalurl)

fulldata <- data.frame(
  journal = journalnames,
  url = journalurl,
  issn = NA,
  publisher = publisher.name,
  date = Sys.Date()
)

write_csv(fulldata,
          paste0("Output//Journals//Predatory//",
                 publisher.name,
                 "-",
                 Sys.Date(),
                 ".csv"))
