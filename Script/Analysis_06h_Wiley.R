# scrape Wiley journals

wiley_journals <- list()
wiley_journal_names <- list()
wiley_journal_url <- list()

url <- paste0("https://onlinelibrary.wiley.com/action/showPublications?PubType=journal&pageSize=50")

firstpage <- rvest::html_session(
  url,
  httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
)

results <- rvest::html_node(firstpage, css = ".result__count > b")
results <- rvest::html_text(results)
results <- stringr::str_remove(results, ",")
results <- as.integer(results)

totalpages <- results / 50
totalpages <- ceiling(totalpages)

for (i in 1:totalpages) {
  wiley_url <- paste0("https://onlinelibrary.wiley.com/action/showPublications?PubType=journal&pageSize=50&startPage=", i - 1)

  printtext <- paste(i, wiley_url, sep = ": ")
  print(printtext)

  wiley_page <- rvest::html_session(
    wiley_url,
    httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
  )

  wiley_journals[[i]] <- rvest::html_node(wiley_page, css = "ul.rlist:nth-child(5)")
  wiley_journals[[i]] <- rvest::html_nodes(wiley_page, css = "li.search__item")

  # if one of wiley_journals[[i]] contains "Currently known as", then do NOT scrape it
  wiley_journals[[i]] <- Filter(function(x) !any(grepl("Currently known as", x)), wiley_journals[[i]])

  wiley_journal_names[[i]] <- rvest::html_nodes(wiley_journals[[i]], css = "h3")
  wiley_journal_names[[i]] <- rvest::html_text(wiley_journal_names[[i]])
  wiley_journal_names[[i]] <- trimws(wiley_journal_names[[i]])


  wiley_journal_url[[i]] <- rvest::html_nodes(wiley_journals[[i]], css = "a.visitable")
  wiley_journal_url[[i]] <- rvest::html_attr(wiley_journal_url[[i]], "href")
  wiley_journal_url[[i]] <- paste0("https://onlinelibrary.wiley.com", wiley_journal_url[[i]])

  Sys.sleep(7)
}



wiley_full <- do.call(rbind, Map(data.frame, journal = wiley_journal_names, url = wiley_journal_url))

wiley_full$url <- ifelse(grepl("wiley.comhttp", wiley_full$url),
               stringr::str_remove(wiley_full$url, "^.*wiley.com?(?=http)"),
               wiley_full$url)

wiley_full$publisher <- "Wiley"
wiley_full$date <- Sys.Date()

wiley_full <- unique(wiley_full)

write.csv(wiley_full, file = ".\\Output\\journals_wiley.csv", row.names = F)
