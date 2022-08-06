#
# IMPORT DATA
#
library(readr)
csv_doaj <- read_csv(file = "Output\\Preliminary_Lists\\01_publishers_DOAJ.csv")
csv_publons <- read_csv(file = "Output\\Preliminary_Lists\\02_publishers_Publons.csv")
csv_scopus <- read_csv(file = "Output\\Preliminary_Lists\\03_publishers_Scopus.csv")
csv_romeo <- read_csv(file = "Output\\Preliminary_Lists\\04_publishers_SherpaRomeo.csv")

names(csv_doaj) <- c("Publisher", "Journals")
names(csv_publons) <- c("Publisher", "Journals", "Reviews")
names(csv_scopus) <- c("Publisher", "Journals")
names(csv_romeo) <- c("Publisher", "Journals")

# ============================
# Preliminary Results
# ============================
#
# DOAJ       8101 Publishers
# Publons    5145 Publishers
# Romeo      4273 Publishers
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
# This example shows a threshold of 15 journals per publisher
threshold <- 15

alljournals <- lapply(alljournals, function(x) subset(x, Journals >= threshold))

csv_doaj <- as.data.frame(alljournals[1])
csv_publons <- as.data.frame(alljournals[2])
csv_romeo <- as.data.frame(alljournals[3])
csv_scopus <- as.data.frame(alljournals[4])

names(csv_doaj) <- c("Publisher", "DOAJ_Journals")
names(csv_publons) <- c("Publisher", "Publons_Journals")
names(csv_romeo) <- c("Publisher", "Romeo_Journals")
names(csv_scopus) <- c("Publisher", "Scopus_Journals")

# ============================
# Preliminary Results 2
# ============================
#
# DOAJ       113 Publishers
# Publons    219 Publishers
# Romeo      260 Publishers
# Scopus     162 Publishers
# ALL        568 distinct names! (before harmonization)

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

# manual correction necessary... 
# b/c for some reason, the harmonization-step does not work
# with all publishers
alljournaldata <- alljournaldata %>%
  mutate(Publisher = case_when(
    grepl("Hemeroteca", Publisher) ~ "Institut d'Estudis Catalans",
    grepl("Bologna", Publisher) ~ "University of Bologna Press",
    grepl("Mulino", Publisher) ~ "Il Mulino",
    grepl("Johns Hop", Publisher) ~ "John Hopkins University Press",
    TRUE ~ Publisher
  ))

library(data.table)
DT <- data.table(alljournaldata)

Encoding(DT$Publisher) <- 'latin1'
DT$Publisher <- stringi::stri_trans_general(DT$Publisher, 'Latin-ASCII')

DT <- DT[, lapply(.SD, sum, na.rm = T), by = Publisher]
DT <- DT[order(Publisher)]

DT <- dplyr::mutate(DT, maxjournals = pmax(DOAJ_Journals, Publons_Journals, Romeo_Journals, Scopus_Journals))
DT <- DT[order(-maxjournals)]

currdate <- Sys.Date()

write.csv(DT, paste0("Output\\Preliminary_Lists\\allpublishers-", currdate, ".csv"), row.names = FALSE)

# Next step is manual:
# find all relevant links to journal catalogues,
# and collect CSS selectors etc, for every publisher,
# and save as "Data\04_publishers.xlsx"