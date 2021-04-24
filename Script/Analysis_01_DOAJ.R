json_data <- rjson::fromJSON(file = "Data\\01_DOAJ.json")

doaj_publishers <- list()

for (i in 1:length(json_data)) {
  doaj_publishers[i] <- as.matrix(json_data[[i]]$bibjson$publisher$name)
}
doaj_publishers <- unlist(doaj_publishers)
df <- data.frame(table(doaj_publishers))
df <- df[rev(order(df$Freq)), ]

write.csv(df, "Output\\01_publishers_DOAJ.csv", row.names = FALSE)
