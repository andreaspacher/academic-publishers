#
# IMPORT DATA
#
filePath <- "C:\\Your\\File\\Path\\"

csv_doaj <- read.delim(file = paste0(filePath, "all-journals-doaj.csv"), header = TRUE, sep = ",")
csv_publons <- read.delim(file = paste0(filePath, "all-journals-publons.csv"), header = TRUE, sep = ",")
csv_romeo <- read.delim(file = paste0(filePath, "all-journals-sherparomeo.csv"), header = TRUE, sep = ",")
csv_scopus <- read.delim(file = paste0(filePath, "all-journals-scopus.csv"), header = TRUE, sep = ",")

names(csv_doaj) <- c("Publisher", "Journals")
names(csv_publons) <- c("Publisher", "Journals", "Reviews")
names(csv_romeo) <- c("Publisher", "Journals")
names(csv_scopus) <- c("Publisher", "Journals")

# ============================
# Preliminary Results
# ============================
#
# DOAJ      7217 Publishers
# Publons   430 Publishers
# Romeo     4611 Publishers
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
alljournals <- lapply(alljournals, function(x) subset(x, Journals >= 30))

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
# you can add additional names to harmonize into "harmonize-publishers-data.txt"
# separated by tabs (\t) (first, the deviation, then, second, after the tab, the harmonization)
harmonia <- read.delim(file = paste0(filePath, "harmonize-publishers-data.txt"), header = TRUE, sep = "\t")

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

write.csv(DT, paste0(filePath, "allpublishers.csv"), row.names = FALSE)
