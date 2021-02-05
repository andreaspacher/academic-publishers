# scrape Taylor & Francis journals

tf_journal_names <- list()
tf_journal_url <- list()

for (i in 1:26) {
  printtext <- paste0("Hello, ", letters[i])
  print(printtext)

  tf_url <- paste0("https://www.tandfonline.com/action/showPublications?pubType=journal&sortBy=&pageSize=20&subjectTitle=&startPage=&alphabetRange=", letters[i])

  tf_page <- rvest::html_session(
    tf_url,
    httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
  )

  tf_total_pages <- rvest::html_node(tf_page, css = ".search-results > p:nth-child(1) > strong:nth-child(2)")
  tf_total_pages <- rvest::html_text(tf_total_pages)
  tf_total_pages <- trimws(tf_total_pages) # 264
  tf_total_pages <- as.numeric(tf_total_pages)

  tf_page_number <- tf_total_pages / 20 # 13,2
  tf_page_number <- ceiling(tf_page_number) # 14 ----- i.e. 14 pages to browse!

  Sys.sleep(5.5)

  tf_journal_names[[i]] <- list()
  tf_journal_url[[i]] <- list()

  for (ii in 1:tf_page_number) {
    tf_url <- paste0("https://www.tandfonline.com/action/showPublications?pubType=journal&sortBy=&pageSize=20&subjectTitle=&alphabetRange=", letters[i], "&startPage=", ii - 1)

    printtext <- paste(i, ii, tf_url, sep = ": ")
    print(printtext)

    tf_page <- rvest::html_session(
      tf_url,
      httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
    )

    tf_journals <- rvest::html_node(tf_page, css = "div.clear:nth-child(1)")


    JOURNALS <- rvest::html_nodes(tf_journals, css = "h4.art_title")
    JOURNALS <- rvest::html_text(JOURNALS)
    JOURNALS <- trimws(JOURNALS)
    tf_journal_names[[i]][[ii]] <- JOURNALS


    URLS <- rvest::html_nodes(tf_journals, css = "h4.art_title")
    URLS <- rvest::html_node(URLS, "a")
    URLS <- rvest::html_attr(URLS, "href")
    URLS <- paste0("https://www.tandfonline.com", URLS)
    tf_journal_url[[i]][[ii]] <- URLS

    Sys.sleep(5.5)
  }
}

tf_full_journals <- unlist(tf_journal_names)
tf_full_urls <- unlist(tf_journal_url)

tf_full <- data.frame("journal" = tf_full_journals, "url" = tf_full_urls)

tf_full$publisher = "Taylor & Francis"
tf_full$date = Sys.Date()

tf_full <- unique(tf_full)

write.csv(tf_full, file = ".\\Output\\journals_taylor-and-francis.csv", row.names = F)
