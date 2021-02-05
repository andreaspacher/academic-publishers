# scrape FRONTIERS  journals
library('tidyverse')

eCaps <- list(chromeOptions = list(
  args = list('--user-agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko)"')
))

rD <- RSelenium::rsDriver(browser="chrome", port=4546L, verbose=F, chromever="87.0.4280.20",
                          extraCapabilities = eCaps)

#AT WORK
#rD <- RSelenium::rsDriver(browser="chrome", port=4546L, verbose=F)
remDr <- rD[["client"]]
remDr$navigate("https://www.frontiersin.org/journals?domain=all")

for(i in 1:10){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(3)    
}

frontierspage <- remDr$getPageSource()
frontierspage <- xml2::read_html(frontierspage[[1]])

frontiers_j <- rvest::html_nodes(frontierspage, "li.card-journal")
  
frontiers_j_root <- rvest::html_nodes(frontiers_j, "div.card-journal__root") %>%
  rvest::html_text() %>%
  trimws()
frontiers_j_name <- rvest::html_nodes(frontiers_j, "div.card-journal__name") %>%
  rvest::html_text() %>%
  trimws()
frontiers_j_totalname <- paste0(frontiers_j_root, " ", frontiers_j_name)

frontiers_j_additional <- rvest::html_nodes(frontiers_j, "div.society__journal") %>%
  rvest::html_text()

frontiers_j_all <- c(frontiers_j_totalname, frontiers_j_additional)



frontiers_urls <- rvest::html_nodes(frontiers_j, "a") %>%
  rvest::html_attr("href")
  
frontiers_full <- do.call(rbind, Map(data.frame, journal=frontiers_j_all, url=frontiers_urls))

frontiers_full$publisher <- "Frontiers"
frontiers_full$date <- Sys.Date()


write.csv(frontiers_full, file=".//Output//journals_frontiers.csv", row.names = F)
