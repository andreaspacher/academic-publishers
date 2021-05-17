# website for scraping
webpage <- xml2::read_html("https://v2.sherpa.ac.uk/view/publisher_list/1.html")

# fetch data about publishers from website
publishers <- rvest::html_nodes(webpage, "div.row > div.span-6 > a")
publishers <- rvest::html_text(publishers)

# fetch data about each publisher's journal count from website
journalnumber <- rvest::html_nodes(webpage, "div.ep_view_page_view_publisher_list > div.row > div.span-3:nth-child(3) > strong")
journalnumber <- rvest::html_text(journalnumber)
journalnumber <- gsub(" \\[view \\]", "", journalnumber)

# save all data into a dataframe
df <- data.frame("Publishers" = publishers, "Journals" = journalnumber)
df$Journals <- as.numeric(df$Journals)
df <- df[rev(order(df$Journals)), ]

write.csv(df, "Output\\04_publishers_SherpaRomeo.csv", row.names = FALSE)
