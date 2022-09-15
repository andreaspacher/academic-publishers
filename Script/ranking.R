library(tidyverse)
library(ggplot2)

PP <- read_csv("Output/allpublishers-PRELIMINARY-2021-12-09.csv")
P <- PP %>% group_by(publisher) %>% count() %>% arrange(desc(n))


OR <- read_tsv("Data\\04_publishers.tsv") %>%
  select(PUBLISHER_NAME)
names(OR)[names(OR) == 'PUBLISHER_NAME'] <- 'publisher'

DF <- left_join(OR, P) %>%
  arrange(desc(n)) %>%
  mutate(comment = ifelse(is.na(n), "journals were counted manually", NA))
#writexl::write_xlsx(DF, "Output\\allpublishers-FINAL-2022-04-14.xlsx")

DF <- DF %>%
  distinct()

rm(DF, OR, P, PP)
DF <- readxl::read_xlsx("Output\\allpublishers-FINAL-2022-04-14.xlsx")

# histogram
ggplot(DF, aes(x = n)) + 
  geom_histogram(color="black", fill="white", bins = 140)+
  geom_vline(aes(xintercept = mean(n)),
               color="red", linetype="dashed", size=1) +
  theme_classic()

# SUMMARY STATISTICS
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
DF %>%
  filter(!is.na(n)) %>%
  summarise(total = sum(n, na.rm = TRUE),
            count = n(),
            avg = mean(n, na.rm = TRUE),
            med = median(n, na.rm = TRUE),
            mode = Mode(n),
            sd = sd(n, na.rm = TRUE),
            min = min(n, na.rm = TRUE),
            q1 = quantile(n, probs = 0.25, na.rm = TRUE),
            q3 = quantile(n, probs = 0.75, na.rm = TRUE),
            max = max(n, na.rm = TRUE))

# =============
#
# save for table
#
# =============
DF %>%
  select(publisher, n) %>%
  arrange(desc(n)) %>%
  top_n(50) %>%
  write_tsv("prelim_top50_for_table.tsv")

# =================
# compare rankings
# =================
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

colnames(csv_doaj)[1] <- "Publisher"
colnames(csv_publons)[1] <- "Publisher"
colnames(csv_romeo)[1] <- "Publisher"
colnames(csv_scopus)[1] <- "Publisher"

harmonia <- read.delim(file = "Data\\03_publishers_harmonization.txt", header = TRUE, sep = "\t")
for (i in 1:nrow(harmonia)) {
  csv_doaj$Publisher[csv_doaj$Publisher == harmonia$Deviation[i]] <- harmonia$Harmonization[i]
  csv_publons$Publisher[csv_publons$Publisher == harmonia$Deviation[i]] <- harmonia$Harmonization[i]
  csv_romeo$Publisher[csv_romeo$Publisher == harmonia$Deviation[i]] <- harmonia$Harmonization[i]
  csv_scopus$Publisher[csv_scopus$Publisher == harmonia$Deviation[i]] <- harmonia$Harmonization[i]
}
#add rank
csv_doaj$rank_doaj <- seq.int(nrow(csv_doaj))
csv_publons$rank_publons <- seq.int(nrow(csv_publons))
csv_romeo$rank_romeo <- seq.int(nrow(csv_romeo))
csv_scopus$rank_scopus <- seq.int(nrow(csv_scopus))

names(csv_doaj) <- c("Publisher", "DOAJ_Journals", "rank_doaj")
names(csv_publons) <- c("Publisher", "Publons_Journals", "rank_publons")
names(csv_romeo) <- c("Publisher", "Romeo_Journals", "rank_romeo")
names(csv_scopus) <- c("Publisher", "Scopus_Journals", "rank_scopus")


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

names(DT) <- c("publisher", "j_doaj", "rank_doaj", "j_publons", "rank_publons",
               "j_romeo", "rank_romeo", "j_scopus", "rank_scopus", "maxjournals")
DT <- DT %>%
  select("publisher", starts_with("j_"), starts_with("rank_"))

TEST <- merge(DF, DT, all = TRUE) %>%
  arrange(desc(n))

TEST$rank_final <- seq.int(nrow(TEST))

RANK <- TEST %>%
  select(publisher, n, starts_with("rank"))
RANK$rank_scopus[RANK$publisher == "Springer"] <- 2
head(RANK)

TEST %>% 
  select(publisher, n, starts_with("j_")) %>%
  View()
# rank overlap
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("gespeR")
library(gespeR)
gespeR::rbo(TEST$rank_final, TEST$rank_doaj)

#library(TopKLists)