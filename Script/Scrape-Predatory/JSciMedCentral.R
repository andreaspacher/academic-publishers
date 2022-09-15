library(tidyverse)
library(rvest)

publisher.name <- "JSciMedCentral"
publisher.url <- "https://www.jscimedcentral.com/journals.php"

# ========
#
# webscrape
#
# ========

webpage <- read_html(publisher.url)

journalnames <- html_nodes(webpage, "div.tab_container > div.tab_content td > div > a") %>%
  html_text() %>%
  unique() %>%
  str_squish()
journalnames <- journalnames[!journalnames %in% "Home Page"]

journalurl <- html_nodes(webpage, "div.tab_container > div.tab_content td > div > a") %>%
  html_attr("href") %>%
  unique()

journalurl <- paste0("https://www.jscimedcentral.com/", journalurl)

fulldata <- data.frame(
  journal = journalnames,
  url = journalurl,
  publisher = publisher.name,
  date = Sys.Date()
)

fulldata <- fulldata[-2,]

write_csv(fulldata,
          paste0("Output//Journals//Predatory//",
                 publisher.name,
                 "-",
                 Sys.Date(),
                 ".csv"))
