#
# EDIT THIS
#
fileToSave <- "C:\\Your\\File\\Path\\all-journals-sherparomeo.csv"

#
# EXECUTABLE CODE STARTS HERE
#
webpage <- xml2::read_html("https://v2.sherpa.ac.uk/view/publisher_list/1.html")
publishers <- rvest::html_nodes(webpage, "div.row > div.span-6 > a")
publishers <- rvest::html_text(publishers)

journalnumber <- rvest::html_nodes(webpage, "div.ep_view_page_view_publisher_list > div.row > div.span-3:nth-child(3) > strong")
journalnumber <- rvest::html_text(journalnumber)
journalnumber <- gsub(" \\[view \\]", "", journalnumber)

df <- data.frame("Publishers" = publishers, "Journals" = journalnumber)
df$Journals <- as.numeric(df$Journals)
df <- df[rev(order(df$Journals)), ]

write.csv(df, fileToSave, row.names = FALSE)
