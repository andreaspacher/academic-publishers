#
# IMPORT DATA
#
library(readr)
csv_doaj <- read_csv(file = "Output\\01_publishers_DOAJ.csv")
csv_publons <- read_csv(file = "Output\\02_publishers_Publons.csv")
csv_scopus <- read_csv(file = "Output\\03_publishers_Scopus.csv")
csv_romeo <- read_csv(file = "Output\\04_publishers_SherpaRomeo.csv")

names(csv_doaj) <- c("Publisher", "Journals")
names(csv_publons) <- c("Publisher", "Journals", "Reviews")
names(csv_scopus) <- c("Publisher", "Journals")
names(csv_romeo) <- c("Publisher", "Journals")

# ============================
# Preliminary Results
# ============================
#
# DOAJ       7217 Publishers
# Publons     430 Publishers
# Romeo      4611 Publishers
# Scopus    11881 Publishers
#

csv_publons$Reviews <- NULL

alljournals <- list(
  DOAJ = csv_doaj,
  Publons = csv_publons,
  Romeo = csv_romeo,
  Scopus = csv_scopus
)

# How many journals should a publisher have in order to be listed?
# This example shows a threshold of 30 journals per publisher
threshold <- 30

alljournals <- lapply(alljournals, function(x) subset(x, Journals >= threshold))

csv_doaj <- as.data.frame(alljournals[1])
csv_publons <- as.data.frame(alljournals[2])
csv_romeo <- as.data.frame(alljournals[3])
csv_scopus <- as.data.frame(alljournals[4])

names(csv_doaj) <- c("Publisher", "DOAJ_Journals")
names(csv_publons) <- c("Publisher", "Publons_Journals")
names(csv_romeo) <- c("Publisher", "Romeo_Journals")
names(csv_scopus) <- c("Publisher", "Scopus_Journals")

doaj_publons <- merge(csv_doaj, csv_publons, by = "Publisher", all = T)
romeo_scopus <- merge(csv_romeo, csv_scopus, by = "Publisher", all = T)
alljournaldata <- merge(doaj_publons, romeo_scopus, by = "Publisher", all = T)

# harmonize publisher names
# you can add additional names to harmonize into "03_publishers_harmonization.txt"
# separated by tabs (\t) (first, the deviation, then, second, after the tab, the harmonization)
harmonia <- read.delim(file = "Data\\03_publishers_harmonization.txt", header = TRUE, sep = "\t")

for (i in 1:nrow(harmonia)) {
  alljournaldata$Publisher[alljournaldata$Publisher == harmonia$Deviation[i]] <- harmonia$Harmonization[i]
}

alljournaldata$DOAJ_Journals <- as.numeric(alljournaldata$DOAJ_Journals)
alljournaldata$Publons_Journals <- as.numeric(alljournaldata$Publons_Journals)
alljournaldata$Romeo_Journals <- as.numeric(alljournaldata$Romeo_Journals)
alljournaldata$Scopus_Journals <- as.numeric(alljournaldata$Scopus_Journals)


library(data.table)
DT <- data.table(alljournaldata)
DT <- DT[, lapply(.SD, sum, na.rm = T), by = Publisher]
DT <- DT[order(Publisher)]

DT <- dplyr::mutate(DT, maxjournals = pmax(DOAJ_Journals, Publons_Journals, Romeo_Journals, Scopus_Journals))
DT <- DT[order(-maxjournals)]

currdate <- Sys.Date()
write.csv(DT, paste0("Output\\allpublishers-", currdate, ".csv"), row.names = FALSE)
