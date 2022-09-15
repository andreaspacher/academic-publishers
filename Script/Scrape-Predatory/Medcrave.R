library(tidyverse)
library(rvest)

publisher.name <- "Medcrave"
publisher.url <- "https://medcraveonline.com/journals"

# ========
#
# webscrape
#
# ========

webpage <- read_html(publisher.url)

journalnames <- html_nodes(webpage, 'a[title="Click view Journal"]') %>%
  html_text() %>%
  unique() %>%
  str_squish()

journalurl <- html_nodes(webpage, 'a[title="Click view Journal"]') %>%
  html_attr("href") %>%
  unique()

journalurl <- paste0("https://medcraveonline.com/", journalurl)

journalissn <- html_nodes(webpage, "span") %>%
  html_text()
journalissn <- journalissn[grepl("ISSN", journalissn)]
journalissn <- str_remove(journalissn, "(- |)eISSN: ") %>%
  str_remove(" \\(Discontinued\\)") %>%
  str_squish() %>%
  unique()

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
