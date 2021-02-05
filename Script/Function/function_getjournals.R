getjournals <- function(ALL) {
  if (ALL$PUBLISHER_PAGES != "MULTIPLE" || (ALL$PUBLISHER_PAGES_NR == "UNKNOWN" && ALL$PUBLISHER_PAGES == "MULTIPLE")) {
    if (ALL$PUBLISHER_MODE == "RSelenium") {

      # FIREFOX
      eCaps <- list(`mox:firefoxOptions` = list(general.useragent.override = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:84.0) Gecko/20100101 Firefox/84.0"))
      rD <- RSelenium::rsDriver(browser = "firefox", port = 4546L, verbose = F, extraCapabilities = eCaps)

      # CHROME
      # eCaps <- list(chromeOptions = list(
      #  args = list('--user-agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko)"')
      # ))
      # rD <- RSelenium::rsDriver(browser="chrome", port=4546L, verbose=F, extraCapabilities = eCaps)

      remDr <- rD[["client"]]

      remDr$navigate(ALL$PUBLISHER_URL)

      for (i in 1:4) {
        remDr$executeScript(paste("scroll(0,", i * 10000, ");"))
        Sys.sleep(3)
      }

      wholepage <- remDr$getPageSource()
      webpage <- xml2::read_html(wholepage[[1]])

      remDr$close()
      gc()
      rD$server$stop()
      system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE)
    } else {
      if (grepl("SESSION_REQUIRED", ALL$OTHER)) {
        webpage <- rvest::html_session(
          ALL$PUBLISHER_URL,
          httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
        )
      } else {
        webpage <- xml2::read_html(ALL$PUBLISHER_URL)
      }
    }
  }

  if (ALL$PUBLISHER_PAGES != "MULTIPLE") {
    journalnames <- rvest::html_nodes(webpage, ALL$JOURNALNAME_CSS)
    if (!grepl("OMIT_NAME_TEXT", ALL$OTHER)) journalnames <- rvest::html_text(journalnames)
    if (ALL$JOURNALNAMES_ADDCODE_YESNO == "1") {
      eval(parse(text = ALL$JOURNALNAMES_ADDITIONAL_CODE))
    }
    if (ALL$OTHER != "OMIT_ALL_UNIQUE") journalnames <- unique(journalnames)
    journalnames <- trimws(journalnames)

    journalurl <- rvest::html_nodes(webpage, ALL$JOURNAL_URL_NODES)
    journalurl <- rvest::html_attr(journalurl, "href")
    if (ALL$JOURNALURL_ADDCODE_YESNO == "1") {
      eval(parse(text = ALL$JOURNALURL_ADDITIONAL_CODE))
    }
    if (ALL$OTHER != "OMIT_URL_UNIQUE" & ALL$OTHER != "OMIT_ALL_UNIQUE") journalurl <- unique(journalurl)
    journalurl <- trimws(journalurl)
    journalurl <- paste0(ALL$JOURNALURL_PASTE, journalurl)
  } else if (ALL$PUBLISHER_PAGES == "MULTIPLE") {
    if (ALL$PUBLISHER_PAGES_NR == "UNKNOWN") {
      results <- rvest::html_node(webpage, css = ALL$PUBLISHER_RESULTS_NODE)
      results <- rvest::html_text(results)
      results <- stringr::str_extract(results, ALL$PUBLISHER_RESULTS_REGEX)

      if (ALL$PUBLISHER_RESULTS_ADDCODE_YESNO == "1") {
        eval(parse(text = ALL$PUBLISHER_RESULTS_ADDITIONAL_CODE))
      }

      total_pages <- ceiling(as.numeric(results) / as.numeric(ALL$PUBLISHER_RESULTS_PER_PAGE))
    } else {
      total_pages <- as.numeric(ALL$PUBLISHER_PAGES_NR)
    }

    journalnames <- list()
    journalurl <- list()

    for (i in 1:total_pages) {
      if (ALL$PUBLISHER_PAGE_I == "i") {
        webpage_url <- paste0(ALL$PUBLISHER_PAGE_URL_BEFORE, i, ALL$PUBLISHER_PAGE_URL_AFTER)
      } else if (ALL$PUBLISHER_PAGE_I == "i-1") {
        webpage_url <- paste0(ALL$PUBLISHER_PAGE_URL_BEFORE, i - 1, ALL$PUBLISHER_PAGE_URL_AFTER)
      }

      printtext <- paste(i, webpage_url, sep = ": ")
      print(printtext)

      if (ALL$PUBLISHER_MODE == "RSelenium") {

        # FIREFOX
        eCaps <- list(`mox:firefoxOptions` = list(general.useragent.override = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:84.0) Gecko/20100101 Firefox/84.0"))
        rD <- RSelenium::rsDriver(browser = "firefox", port = 4546L, verbose = F, extraCapabilities = eCaps)

        # CHROME
        # eCaps <- list(chromeOptions = list(
        #  args = list('--user-agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko)"')
        # ))
        # rD <- RSelenium::rsDriver(browser="chrome", port=4546L, verbose=F, extraCapabilities = eCaps)
        # rD <- RSelenium::rsDriver(browser="chrome")

        remDr <- rD[["client"]]

        remDr$navigate(webpage_url)

        for (i in 1:4) {
          remDr$executeScript(paste("scroll(0,", i * 10000, ");"))
          Sys.sleep(3)
        }

        wholepage <- remDr$getPageSource()
        webpage <- xml2::read_html(wholepage[[1]])

        remDr$close()
        gc()
        rD$server$stop()
        system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE)
      } else {
        if (ALL$OTHER == "SESSION_REQUIRED") {
          webpage <- rvest::html_session(
            webpage_url,
            httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
          )
        } else if (ALL$OTHER == "RUSSIAN_ENCODING") {
          webpage <- xml2::read_html(url(webpage_url), encoding = "windows-1251")
        }
        else {
          webpage <- xml2::read_html(webpage_url)
        }
      }

      journalnames[[i]] <- rvest::html_nodes(webpage, css = ALL$JOURNALNAME_CSS)
      journalnames[[i]] <- rvest::html_text(journalnames[[i]])
      if (ALL$JOURNALNAMES_ADDCODE_YESNO == "1") {
        eval(parse(text = ALL$JOURNALNAMES_ADDITIONAL_CODE))
      }
      journalnames[[i]] <- unique(journalnames[[i]])
      journalnames[[i]] <- trimws(journalnames[[i]])

      journalurl[[i]] <- rvest::html_nodes(webpage, css = ALL$JOURNAL_URL_NODES)
      journalurl[[i]] <- rvest::html_attr(journalurl[[i]], "href")
      if (ALL$JOURNALURL_ADDCODE_YESNO == "1") {
        eval(parse(text = ALL$JOURNALURL_ADDITIONAL_CODE))
      }
      journalurl[[i]] <- unique(journalurl[[i]])
      journalurl[[i]] <- trimws(journalurl[[i]])
      journalurl[[i]] <- paste0(ALL$JOURNALURL_PASTE, journalurl[[i]])

      Sys.sleep(5.5)
    }
  }
  
  fulldata <- do.call(
    rbind,
    Map(data.frame,
        journal = journalnames,
        url = journalurl,
        publisher = ALL$PUBLISHER_NAME,
        date = Sys.Date()
    )
  )
  
  fulldata <- unique(fulldata)
  
  return(fulldata)
}
