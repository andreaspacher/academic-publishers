library(rvest)
library(purrr)

# basic url for webscraping
url_base <- "https://publons.com/publisher/?page="

# prepare dataframe in which data will be stored during the webscraping process
publons <- data.frame("publisher" = character(), "journals" = integer(), "reviews" = integer())

# scrape 43 pages at Publons,
# so as to cover publishers that have at least 1.000 reviews there
# (as of Dec. 2020)
for (i in 1:43) {

  # progress indicator
  cat(".")

  # scrape each of the 43 pages
  pg <- xml2::read_html(paste0(url_base, i))

  # fetch data from Publons website
  publisher <- html_text(html_nodes(pg, "h3"))
  journaldata <- html_node(pg, "#body > div > div.content > div")
  journals <- stringr::str_extract_all(journaldata, "(?<=<span>).*?(?= Journal)")
  journals <- unlist(journals)
  reviews <- stringr::str_extract_all(journaldata, "(?<=<span>).*?(?= Reviews</span>)")
  reviews <- unlist(reviews)

  currentdf <- data.frame("publisher" = publisher, "journals" = journals, "reviews" = reviews)

  publons <- rbind(publons, currentdf)

  Sys.sleep(8)
}

publons$journals <- as.numeric(publons$journals)
df <- publons[order(publons$journals, decreasing = TRUE), ]
df <- df[!duplicated(df$publisher), ]
df$reviews <- substring(df$reviews, 7)

write.csv(df, "Output\\02_publishers_Publons.csv", row.names = FALSE)
