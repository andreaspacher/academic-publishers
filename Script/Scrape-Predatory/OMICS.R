library(tidyverse)
library(rvest)

publisher.name <- "OMICS"
publisher.url <- "https://www.omicsonline.org/open-access-journals-list.php"

# ========
#
# webscrape
#
# ========
# Webscraping neither workd with rvest nor with RSelenium.
# We thus saved the HTML code in the following file:

webpage <- read_html("Script\\Scrape-Predatory\\OMICS.txt")

# ... and we will scrape the data from that file.

journalnames <- html_nodes(webpage, "a.nav-link.dark-golden-rod-before") %>%
  html_attr("title") %>%
  unique() %>%
  str_squish()

journalurl <- html_nodes(webpage, "a.nav-link.dark-golden-rod-before") %>%
  html_attr("href") #%>%
  #unique()

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



# Some Analysis
library(urltools)

OMICS <- fulldata %>%
  mutate(domain = suffix_extract(domain(url))) %>%
  mutate(domainname = paste0(domain$domain, ".", domain$suffix))

# order by domains
topdomains <- sort(table(OMICS$domainname), decreasing = T)

topdomains
# =====================================
# top 5 results, as of 15 August 2022, are:
# =====================================
# --- 1 --- omicsonline.org           463
# --- 2 --- imedpub.com               129
# --- 3 --- scitechnol.com            51
# --- 4 --- rroij.com                 33
# --- 5 --- openaccessjournals.com    5