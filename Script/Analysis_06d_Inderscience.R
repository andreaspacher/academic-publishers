file <- ".//Data//05_inderscience-journals-all.txt"

indsci_data <- readChar(file, file.info(file)$size)

abbr <- stringr::str_extract_all(indsci_data, "(?<=jcode=).*?(?='>)")
abbr <- unlist(abbr)
abbr <- unique(abbr)

url <- paste0("https://www.inderscience.com/jhome.php?jcode=", abbr)

journals <- stringr::str_extract_all(indsci_data, "(?<='>).{3,90}(?=</a> <span>&nbsp;&nbsp;&nbsp;<a|<span style='color: )")
journals <- unlist(journals)
journals <- unique(journals)

indsci_journals <- data.frame(url, journals)
colnames(indsci_journals) <- c("url", "journal")
indsci_journals$publisher = "Inderscience"
indsci_journals$date = Sys.Date()

write.csv(indsci_journals, ".//Output//journals_inderscience.csv", row.names = FALSE)

rm(list = ls())
