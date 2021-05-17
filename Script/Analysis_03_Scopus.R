scopusdata <- readxl::read_xlsx("Data\\02_Scopus.xlsx")
scopus_publishers <- scopusdata[, 23]

df <- data.frame(table(scopus_publishers))
df <- df[rev(order(df$Freq)), ]

write.csv(df, "Output\\03_publishers_Scopus.csv", row.names = FALSE)
