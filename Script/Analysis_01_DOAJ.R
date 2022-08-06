# the source of the csv-file
# is https://doaj.org/docs/public-data-dump/
doaj <- readr::read_csv("Data\\01_DOAJ.csv")
doaj <- janitor::clean_names(doaj)

# extract publishers
df <- data.frame(table(doaj$publisher))

# count frequency of publishers
df <- df[rev(order(df$Freq)), ]

write.csv(df, "Output\\Preliminary_Lists\\01_publishers_DOAJ.csv", row.names = FALSE)
