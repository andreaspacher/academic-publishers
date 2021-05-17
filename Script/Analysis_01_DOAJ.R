#
# EDIT THESE DATA
#

json_file <- "Data\\01_DOAJ.json"
fileToSave <- "C:\\Your\\File\\Path\\all-journals-doaj.csv"

#
# EXECUTABLE CODE STARTS HERE
#
json_data <- rjson::fromJSON(file = json_file)

doaj_publishers <- list()

for (i in 1:length(json_data)) {
  doaj_publishers[i] <- as.matrix(json_data[[i]]$bibjson$publisher$name)
}
doaj_publishers <- unlist(doaj_publishers)
df <- data.frame(table(doaj_publishers))
df <- df[rev(order(df$Freq)), ]

write.csv(df, fileToSave, row.names = FALSE)
