#
# EDIT THIS
#
fileToSave <- "C:\\Your\\File\\Path\\all-journals-publons.csv"

#
# EXECUTABLE CODE STARTS HERE
#
library(rvest)
library(purrr)

url_base <- "https://publons.com/publisher/?page="

publons <- data.frame("publisher" = character(), "journals" = integer(), "reviews" = integer())

# 43 pages at Publons, covering publishers that have at least 1.000 reviews there (as of Dec. 2020)
for (i in 1:43) {

  # simple but effective progress indicator
  cat(".")

  pg <- xml2::read_html(paste0(url_base, i))

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

write.csv(df, fileToSave, row.names = FALSE)
