email <- "YOUR@MAIL.COM" # to use the CrossRef API

# scrape IOP Publishing journals

iop_url <- "https://ioppublishing.org/publications/our-journals/"

iop_page <- rvest::html_session(
  iop_url,
  httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
)

iop_journals <- rvest::html_node(iop_page, css = "#main > section.m028 > div > div.m028__row.post-container")

iop_journal_url <- rvest::html_nodes(iop_journals, "article > a")
iop_journal_url <- rvest::html_attr(iop_journal_url, "href")

iop_issn <- stringr::str_extract_all(iop_journal_url, "(?<=journal/).*$")
iop_issn <- unlist(iop_issn)


iop_journal_names <- list()

# find journal names based on CrossRef API
for (i in 1:length(iop_issn)) {

  crossref <- paste0("https://api.crossref.org/journals/", iop_issn[i], "&mailto=", email)
  res <- httr::GET(crossref)

  printtext <- paste(i, crossref, sep = ": ")
  print(printtext)
    
  if(res$status_code == 200) {
    json_data <- jsonlite::fromJSON(rawToChar(res$content), flatten = TRUE)
    iop_journal_names[[i]] <- json_data$message$title
    rm(json_data)
  } else {
    print("Error")
  }

  Sys.sleep(0.5)
}

iop_journal_names[sapply(iop_journal_names, is.null)] <- NA
iop_journal_names <- unlist(iop_journal_names)

iop_full <- do.call(rbind, Map(data.frame, journal = iop_journal_names, url = iop_journal_url))
iop_full$publisher = "IOP"
iop_full$date = Sys.Date()

currentDate <- Sys.Date()
write.csv(iop_full,
          file = paste0("Output/journals-IOP-", currentDate, ".csv"),
          row.names = F
)