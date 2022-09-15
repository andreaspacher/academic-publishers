# scrape Medknow journals

medknow_journal_names <- list()
medknow_journal_url <- list()

medknow_url <- "https://www.medknow.com/journals.asp?tag=0"

medknow_page <- rvest::session(
  medknow_url,
  httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
)

medknow_total_pages <- rvest::html_node(medknow_page, css = "#content_inner > div:nth-child(3)")
medknow_total_pages <- stringr::str_extract(medknow_total_pages, ".*?(?=Last)")
medknow_total_pages <- stringr::str_extract(medknow_total_pages, ".{15,15}$")
medknow_total_pages <- stringr::str_extract(medknow_total_pages, "(?<=pg=).*?(?=\")")

medknow_page_number <- ifelse(is.na(medknow_total_pages), 1, medknow_total_pages)

for (i in 1:medknow_page_number) {
  medknow_url <- paste0("https://www.medknow.com/journals.asp?tag=0&pg=", i)

  printtext <- paste(i, medknow_url, sep = ": ")
  print(printtext)

  medknow_page <- rvest::session(
    medknow_url,
    httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
  )

  medknow_journals <- rvest::html_node(medknow_page, css = "#content_inner")
  medknow_journals <- rvest::html_nodes(medknow_page, css = "table")

  # if one of medknow_journals[[i]] contains "Awaited", then do NOT scrape it
  medknow_journals <- Filter(function(x) !any(grepl("Awaited", x)), medknow_journals)

  JOURNALS <- rvest::html_nodes(medknow_journals, css = "h2")
  JOURNALS <- rvest::html_text(JOURNALS)
  JOURNALS <- trimws(JOURNALS)
  medknow_journal_names[[i]] <- JOURNALS

  URLS <- rvest::html_nodes(medknow_journals, css = "a")
  URLS <- rvest::html_attr(URLS, "href")

  # remove every 2nd element
  toDelete <- seq(0, length(URLS), 2)
  URLS <- URLS[-toDelete]
  medknow_journal_url[[i]] <- URLS

  Sys.sleep(5.5)
}

medknow_full_journals <- unlist(medknow_journal_names)
medknow_full_urls <- unlist(medknow_journal_url)
medknow_full_journals <- unique(medknow_full_journals)
medknow_full_urls <- unique(medknow_full_urls)

medknow_full <- data.frame("journal" = medknow_full_journals, "url" = medknow_full_urls)

medknow_full$publisher = "Medknow"
medknow_full$date = Sys.Date()

medknow_full <- unique(medknow_full)

currentDate <- Sys.Date()
write.csv(medknow_full,
          file = paste0("Output/journals-medknow-", currentDate, ".csv"),
          row.names = F
)
