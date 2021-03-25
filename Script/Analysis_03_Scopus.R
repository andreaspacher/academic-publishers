#
# EDIT THESE DATA
#
fileread <- "./Data/02_Scopus.xlsx"
fileToSave <- "./Output/all-journals-scopus.csv"

#
# EXECUTABLE CODE STARTS HERE
#

scopusdata <- readxl::read_xlsx(fileread)
scopus_publishers <- scopusdata[, 23]

df <- data.frame(table(scopus_publishers))
df <- df[rev(order(df$Freq)), ]

write.csv(df, fileToSave, row.names = FALSE)
