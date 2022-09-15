# scrape Springer journals

springer_journal_names <- list()
springer_journal_url <- list()

for (i in 1:27) {
  printtext <- paste0("Hello, ", letters[i])
  print(printtext)

  springer_url <- ifelse(i == 27, "https://link.springer.com/journals/0/1", paste0("https://link.springer.com/journals/", letters[i], "/1"))

  springer_page <- rvest::session(
    springer_url,
    httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
  )

  springer_total_pages <- rvest::html_node(springer_page, css = "#main-content > p > span")
  springer_total_pages <- rvest::html_text(springer_total_pages)
  springer_total_pages <- trimws(springer_total_pages) # 264
  springer_total_pages <- as.numeric(springer_total_pages)

  springer_page_number <- springer_total_pages / 200 # 13,2
  springer_page_number <- ceiling(springer_page_number) # 14 ----- i.e. 14 pages to browse!

  Sys.sleep(5.5)
  
  if(springer_page_number > 0) {

    springer_journal_names[[i]] <- list()
    springer_journal_url[[i]] <- list()
  
    for (ii in 1:springer_page_number) {
      springer_url <- ifelse(i == 27, "https://link.springer.com/journals/0", paste0("https://link.springer.com/journals/", letters[i]))
      springer_url <- paste0(springer_url, "/", ii)
  
      printtext <- paste(i, ii, springer_url, sep = ": ")
      print(printtext)
  
      springer_page <- rvest::session(
        springer_url,
        httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
      )
  
      springer_journals <- rvest::html_node(springer_page, css = "#main-content > ol")
  
  
      JOURNALS <- rvest::html_nodes(springer_journals, css = "li")
      JOURNALS <- rvest::html_text(JOURNALS)
      JOURNALS <- trimws(JOURNALS)
      springer_journal_names[[i]][[ii]] <- JOURNALS
  
  
      URLS <- rvest::html_nodes(springer_journals, css = "li")
      URLS <- rvest::html_node(URLS, "a")
      URLS <- rvest::html_attr(URLS, "href")
      #    URLS <- paste0("https://www.tandfonline.com", URLS)
      springer_journal_url[[i]][[ii]] <- URLS
  
      Sys.sleep(5.5)
    }
  }
}

springer_full_journals <- unlist(springer_journal_names)
springer_full_urls <- unlist(springer_journal_url)

springer_full <- data.frame("journal" = springer_full_journals, "url" = springer_full_urls)

springer_full$publisher <- "Springer"
springer_full$date <- Sys.Date()

springer_full <- unique(springer_full)

currentDate <- Sys.Date()
write.csv(springer_full,
          file = paste0("Output/journals-springer-", currentDate, ".csv"),
          row.names = F
)
