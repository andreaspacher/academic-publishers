# the list is from Scopus' Content Coverage site
# https://www.elsevier.com/solutions/scopus/how-scopus-works/content?dgcid=RN_AGCM_Sourced_300005030
scopusdata <- readxl::read_xlsx("Data\\02_Scopus.xlsx")
scopus_publishers <- scopusdata[, 23]

df <- data.frame(table(scopus_publishers))
df <- df[rev(order(df$Freq)), ]

write.csv(df, "Output\\Preliminary_Lists\\03_publishers_Scopus.csv", row.names = FALSE)
