#
# EDIT THESE DATA
#
fileread <- "C:\\Your\\File\\Path\\data--scopus.xlsx"
fileToSave <- "C:\\Your\\File\\Path\\all-journals-scopus.csv"

#
# EXECUTABLE CODE STARTS HERE
#

scopusdata <- readxl::read_xlsx(fileread)
scopus_publishers <- scopusdata[, 23]

df <- data.frame(table(scopus_publishers))
df <- df[rev(order(df$Freq)), ]

write.csv(df, fileToSave, row.names = FALSE)
