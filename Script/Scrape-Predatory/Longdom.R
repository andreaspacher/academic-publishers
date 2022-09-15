library(tidyverse)
library(rvest)

publisher.name <- "Longdom"
publisher.url <- "https://www.longdom.org/journals-by-title.html"

# ========
#
# webscrape
#
# ========
# Webscraping neither workd with rvest nor with RSelenium.
# We thus saved the HTML code in the following file:

webpage <- read_html("Script\\Scrape-Predatory\\Longdom.txt")

# ... and we will scrape the data from that file.

journalnames <- html_nodes(webpage, "#pull_subjects h3 a.deep-orange-400-before") %>%
  html_text() %>%
  unique() %>%
  str_squish()

journalurl <- html_nodes(webpage, "#pull_subjects h3 a.deep-orange-400-before") %>%
  html_attr("href") %>%
  unique()

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
