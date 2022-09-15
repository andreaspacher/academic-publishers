library(tidyverse)
library(data.table)

ALL <- readxl::read_xlsx("./Data/04_publishers_new-15.xlsx") %>%
  filter(!is.na(results_manual)) %>%
  select("publisher" = PUBLISHER_NAME, "n" = results_manual) %>%
  mutate("comment" = "journals were counted manually")

PP <- readxl::read_xlsx("Output/allpublishers-2022-04-15.xlsx")
P <- PP %>% group_by(publisher) %>% arrange(desc(n))

FINAL <- rbind(ALL, P) %>%
  arrange(desc(n))

FINAL <- readxl::read_xlsx("Data\\04_publishers.xlsx")

rm(ALL, PP, P)

# ================
#
# Comparison with Scopus & Publons
# & DOAJ & Sherpa Romeo
#
# ================
SCOPUS <- read_csv("Output\\Preliminary_Lists\\03_publishers_Scopus.csv")
PUBLONS <- read_csv("Output\\Preliminary_Lists\\02_publishers_Publons.csv")
DOAJ <- read_csv("Output\\Preliminary_Lists\\01_publishers_DOAJ.csv")
SHERPA <- read_csv("Output\\Preliminary_Lists\\04_publishers_SherpaRomeo.csv")

# cbind(head(SCOPUS,284), FINAL) %>% View()
# cbind(head(PUBLONS,284), FINAL) %>% View()

harmonia <- read.delim(file = "Data\\03_publishers_harmonization.txt", header = TRUE, sep = "\t")
colnames(harmonia) = c("Deviation", "Harmonization")

colnames(SCOPUS) = c("publisher", "Freq")
colnames(SHERPA) = c("publisher", "Freq")
colnames(DOAJ) = c("publisher", "Freq")
colnames(PUBLONS) = c("publisher", "Freq", "reviews")
PUBLONS <- PUBLONS %>% select(-reviews)

for (i in 1:nrow(harmonia)) {
  
  SCOPUS$publisher[SCOPUS$publisher == harmonia$Deviation[i]] <- harmonia$Harmonization[i]
  PUBLONS$publisher[PUBLONS$publisher == harmonia$Deviation[i]] <- harmonia$Harmonization[i]
  SHERPA$publisher[SHERPA$publisher == harmonia$Deviation[i]] <- harmonia$Harmonization[i]
  DOAJ$publisher[DOAJ$publisher == harmonia$Deviation[i]] <- harmonia$Harmonization[i]
  
}

allsources <- list(DOAJ, PUBLONS, SCOPUS, SHERPA)

allsources <- purrr::map(allsources, function(x) {
  DT <- data.table(x)
  DT <- DT[, lapply(.SD, sum, na.rm = T), by = publisher]
  DT <- DT[order(-Freq)]
  DT <- as.data.frame(DT)
  DT <- DT[DT$Freq >= 15,]
  #Encoding(DT$publisher) <- 'latin1'
  #DT$publisher <- stringi::stri_trans_general(DT$publisher, 'Latin-ASCII')
  #colnames(DT) <- c("publisher", deparse(substitute(x)))
})

colnames(allsources[[1]]) <- c("publisher", "doaj")
colnames(allsources[[2]]) <- c("publisher", "publons")
colnames(allsources[[3]]) <- c("publisher", "scopus")
colnames(allsources[[4]]) <- c("publisher", "sherpa")

FINAL <- readxl::read_xlsx("Output\\top-100-publishers.xlsx")
colnames(FINAL) = c("publisher", "nishikawa", "predatory", "globalsouth")

ALL <- full_join(FINAL, allsources[[1]]) %>%
  full_join(allsources[[2]]) %>%
  full_join(allsources[[3]]) %>%
  full_join(allsources[[4]]) %>%
  arrange(desc(nishikawa)) %>%
  select(publisher, nishikawa, scopus, publons, doaj, sherpa)

ALL$really_not_in_scopus <- 0
ALL$really_not_in_publons <- 0
ALL$really_not_in_doaj <- 0
ALL$really_not_in_sherpa <- 0

for (i in 1:nrow(harmonia)) {
  
  ALL$publisher[ALL$publisher == harmonia$Deviation[i]] <- harmonia$Harmonization[i]

}

ALL <- ALL %>%
  # mutate(publisher = ifelse(publisher == "Wolters Kluwer",
  #                           "Lippincott, Williams & Wilkins",
  #                           publisher)) %>%
  group_by(publisher) %>%
  top_n(1, nishikawa) %>%
  distinct()

ALL <- ALL %>%
  mutate(publisher = ifelse(publisher == "Universidade de Bras<ed>lia",
                            "Universidade de Brasilia", publisher)) %>%
  group_by(publisher) %>%
  top_n(1, nishikawa) %>%
  distinct()


#write_csv(ALL, "Output\\RnR-examine.csv")
#writexl::write_xlsx(ALL, "Output\\RnR-final.xlsx")

# Summary Statistics
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
ALL %>%
  head(100) %>%
  mutate(n = nishikawa) %>%
  ungroup() %>%
  #filter(!is.na(n)) %>%
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
# predatoriness?
# =============
ALL$predatory <- 0

ALL <- ALL %>%
  mutate(predatory = case_when(
    publisher == "OMICS" ~ 1,
    publisher == "Science Publishing Group" ~ 1,
    publisher == "SCIRP" ~ 1,
    publisher == "Open Access Text (OAT)" ~ 1,
    publisher == "Bentham" ~ 1, # ???
    publisher == "Austin Publishing Group" ~ 1,
    publisher == "Longdom" ~ 1,
    publisher == "Open Access Pub" ~ 1,
    publisher == "Gavin Publishers" ~ 1,
    publisher == "iMedPub" ~ 1,
    publisher == "Scientific and Academic Publishing" ~ 1,
    publisher == "JSciMedCentral" ~ 1,
    #publisher == "Frontiers" ~ 1, # ???
    publisher == "Hans Publishers" ~ 1, # ???
    publisher == "Advanced Research Publications" ~ 1,
    publisher == "Hilaris" ~ 1,
    publisher == "Academic Journals" ~ 1,
    publisher == "Science and Education Publishing" ~ 1,
    #publisher == "Conscientia Beam" ~ 1, # not in Beall's List
    # Conscientia Beam:
    # promises quick publication (1st decision within 20-30 days,
    # acceptance in 50-60 days); strangely, the field phone nr
    # is required in manuscript submission system; one can also
    # add WhatsApp nr; many journals actually do not require
    # APCs, and four journals seem to be in Scopus
    # but not in DOAJ
    publisher == "Sciencedomain International" ~ 1,
    publisher == "Peertechz Publications" ~ 1,
    publisher == "Medcrave" ~ 1,
    publisher == "SciTechnol" ~ 1,
    publisher == "IOS Press" ~ 1, # ???
    publisher == "Internet Scientific Publications" ~ 1,
    publisher == "International Scholars Journals" ~ 1,
    publisher == "Annex Publishers" ~ 1, # not in Beall's List
    # Annex Publishers: refers to "CiteFactor" as a reference,
    # promises rapid peer review (21 days), visibility on
    # Google Scholar (inter alia), and a publication within
    # 24h after acceptance; not in DOAJ; the APCs are quite
    # high (all between $1200 to $3600)!
    publisher == "Open Access Journals" ~ 1,
    publisher == "Herbert Publications" ~ 1,
    publisher == "Medwin Publishers LLC" ~ 1,
    publisher == "Premier Publishers" ~ 1,
    publisher == "Pulsus Group" ~ 1,
    publisher == "Scholarena" ~ 1,
    TRUE ~ 0
  ))

head(ALL, 100) %>%
  group_by(predatory) %>%
  summarise(n = n(),
            share = paste0(round(n/100*100, 1), "%"))

#writexl::write_xlsx(ALL, "Output\\RnR-final.xlsx")

# =============
# Global Southiness?
# =============
ALL$globalsouth <- 0

ALL <- ALL %>%
  mutate(globalsouth = case_when(
    publisher == "Universitas Pendidikan Indonesia" ~ 1,
    publisher == "Universidad de Buenos Aires" ~ 1,
    publisher == "Universidad Nacional Autonoma de Mexico" ~ 1,
    publisher == "Universitas Gadjah Mada" ~ 1,
    publisher == "Universitas Negeri Semarang" ~ 1,
    publisher == "University of Tehran" ~ 1,
    publisher == "Universidad Nacional de La Plata" ~ 1,
    publisher == "Universitas Airlangga" ~ 1,
    publisher == "University of Malaya" ~ 1,
    publisher == "Universitas Negeri Yogyakarta" ~ 1,
    publisher == "Universidade Federal do Espirito Santo" ~ 1,
    publisher == "Universidad Nacional de Cordoba" ~ 1,
    publisher == "Universitas Negeri Surabaya" ~ 1,
    publisher == "Universidade Federal do Rio Grande do Sul" ~ 1,
    publisher == "Universitas Diponegoro" ~ 1,
    publisher == "Universidade de Brasilia" ~ 1,
    publisher == "Pontificia Universidad Javeriana, Bogota" ~ 1,
    publisher == "Universidade de Bras<ed>lia" ~ 1,
    TRUE ~ 0
  ))

head(ALL, 100) %>%
  group_by(globalsouth) %>%
  summarise(n = n(),
            share = paste0(round(n/100*100, 1), "%"))
# =============
# manual examinations
# =============

SCOPUS %>%
  filter(grepl("Austin", publisher))
SCOPUS %>%
  filter(grepl("Open Access Pub", publisher))
SCOPUS %>%
  filter(grepl("Longdom", publisher))
SCOPUS %>%
  filter(grepl("Gavin", publisher))
SCOPUS %>%
  filter(grepl("Scientific and Ac", publisher))
SCOPUS %>%
  filter(grepl("Advanced Res", publisher))

PUBLONS %>%
  filter(grepl("Open Access", publisher))
PUBLONS %>%
  filter(grepl("Negeri", publisher)) %>% View()
PUBLONS %>%
  filter(grepl("Pleiad", publisher))
PUBLONS %>%
  filter(grepl("B.g.ta|P.ntific", publisher))
PUBLONS %>%
  filter(grepl("Mulino", publisher))

DOAJ %>%
  filter(grepl("OMICS", publisher))
DOAJ %>%
  filter(grepl("Open Access Te", publisher))
DOAJ %>%
  filter(grepl("Longdom", publisher))
DOAJ %>%
  filter(grepl("Gavin", publisher))
DOAJ %>%
  filter(grepl("Scientific (and|&) Ac", publisher))
# eScholarship, KeAi, Sao Paolo, Intellect
SHERPA %>%
  filter(grepl("OMICS", publisher))
SHERPA %>%
  filter(grepl("INDER|Inder", publisher))
SHERPA %>%
  filter(grepl("Science Pub", publisher))
SHERPA %>%
  filter(grepl("Philosophy D", publisher))
SHERPA %>%
  filter(grepl("SCIRP", publisher))
SHERPA %>%
  filter(grepl("IRMA|Information Res", publisher))
SHERPA %>%
  filter(grepl("IGI|I G I", publisher))
SHERPA %>%
  filter(grepl("Open Access T|OAT", publisher))
SHERPA %>%
  filter(grepl("Austin", publisher))
SHERPA %>%
  filter(grepl("Bentham", publisher))
SHERPA %>%
  filter(grepl("Open Access Pu", publisher))
SHERPA %>%
  filter(grepl("Longdom", publisher))
SHERPA %>%
  filter(grepl("Gavin", publisher))
SHERPA %>%
  filter(grepl("Schweizerb", publisher))
SHERPA %>%
  filter(grepl("Fabriz", publisher))

# ================
# calculate probability
# ================
TOP <- head(ALL, 100)

TOP <- readxl::read_xlsx("Output\\top-100-publishers.xlsx") %>%
  head(100) %>%
  mutate(publisher = case_when(
    publisher == "John Hopkins University Press" ~ "Johns Hopkins University Press",
    TRUE ~ publisher
  ))

TOP %>% summarise(sum = sum(journals))

for (i in 1:nrow(harmonia)) {
  
  TOP$publisher[TOP$publisher == harmonia$Deviation[i]] <- harmonia$Harmonization[i]
  
}


DOAJ2 <- DOAJ %>%
  mutate(is_in_top_list = ifelse(publisher %in% TOP$publisher, 1, 0)) %>%
  group_by(publisher) %>%
  top_n(1, Freq) %>%
  distinct() %>%
  ungroup()
PUBLONS2 <- PUBLONS %>%
  mutate(is_in_top_list = ifelse(publisher %in% TOP$publisher, 1, 0)) %>%
  group_by(publisher) %>%
  top_n(1, Freq) %>%
  distinct() %>%
  ungroup()
SCOPUS2 <- SCOPUS %>%
  mutate(is_in_top_list = ifelse(publisher %in% TOP$publisher, 1, 0)) %>%
  group_by(publisher) %>%
  top_n(1, Freq) %>%
  distinct() %>%
  ungroup()
SHERPA2 <- SHERPA %>%
  mutate(is_in_top_list = ifelse(publisher %in% TOP$publisher, 1, 0)) %>%
  group_by(publisher) %>%
  top_n(1, Freq) %>%
  distinct() %>%
  ungroup()

DOAJ3 <- DOAJ2 %>%
  filter(is_in_top_list == 1) %>%
  select(publisher, "doaj" = Freq)
PUBLONS3 <- PUBLONS2 %>%
  filter(is_in_top_list == 1) %>%
  select(publisher, "publons" = Freq)
SCOPUS3 <- SCOPUS2 %>%
  filter(is_in_top_list == 1) %>%
  select(publisher, "scopus" = Freq)
SHERPA3 <- SHERPA2 %>%
  filter(is_in_top_list == 1) %>%
  select(publisher, "sherpa" = Freq)

MADE.IT <- full_join(DOAJ3, PUBLONS3) %>%
  full_join(SCOPUS3) %>%
  full_join(SHERPA3) %>%
  add_row(publisher = "Universidade Federal do Espirito Santo",
          doaj = NA,
          publons = NA,
          scopus = NA,
          sherpa = 18) %>%
  add_row(publisher = "Universidade de Brasilia",
          doaj = 26,
          publons = NA,
          scopus = NA,
          sherpa = 31) %>%
  add_row(publisher = "Intellect Books",
          doaj = NA,
          publons = 17,
          scopus = 38,
          sherpa = 111) %>%
  add_row(publisher = "Universidad Nacional de Cordoba",
          doaj = 19,
          publons = NA,
          scopus = NA,
          sherpa = 54) %>% 
  add_row(publisher = "International Scholars Journals",
          doaj = NA,
          publons = 15,
          scopus = NA,
          sherpa = NA) %>%
  add_row(publisher = "Longdom",
          doaj = NA,
          publons = 15,
          scopus = NA,
          sherpa = NA) %>%
  rowwise() %>% 
  mutate(max = max(doaj, publons, scopus, sherpa, na.rm = T))
  
setdiff(MADE.IT$publisher, TOP$publisher)
setdiff(TOP$publisher, MADE.IT$publisher)
MADE.IT <- MADE.IT[-c(46, 86, 88, 89, 90),]

MIX <- rbind(DOAJ2, PUBLONS2, SCOPUS2, SHERPA2) #%>%
  #select(Freq, is_in_top_list)
# prop.table(table(MIX$is_in_top_list, MIX$Freq)) %>%
#   as.data.table() 

MIX <- MIX %>%
  group_by(publisher, is_in_top_list) %>%
  summarise(Freq = max(Freq)) %>%
  arrange(desc(Freq)) %>%
  filter(Freq >= 15)
MIX$formerrank <- seq.int(nrow(MIX))

QUANTILES <- MIX %>%
  #filter(Freq >= 15) %>%
  ungroup() %>%
  mutate(quantile = ntile(-formerrank, 10)) %>% 
  #mutate(quantile = Hmisc::cut2(Freq, 10)) %>%
  group_by(quantile) %>%
  summarise(total = n(),
            #in_final = sum(is_in_top_list),
            min = min(formerrank),
            max = max(formerrank)#,
            #share = round(in_final / total * 100, 1)
            )

MADE.IT <- MADE.IT %>%
  left_join(MIX %>% select(publisher, formerrank)) %>%
  mutate(quantile = case_when(
    formerrank >= 1 & formerrank <= 41 ~ 10,
    formerrank >= 42 & formerrank <= 82 ~ 9,
    formerrank >= 83 & formerrank <= 123 ~ 8,
    formerrank >= 124 & formerrank <= 164 ~ 7,
    formerrank >= 165 & formerrank <= 205 ~ 6,
    formerrank >= 206 & formerrank <= 246 ~ 5,
    formerrank >= 247 & formerrank <= 288 ~ 4,
    formerrank >= 289 & formerrank <= 330 ~ 3,
    formerrank >= 331 & formerrank <= 372 ~ 2,
    formerrank >= 373 & formerrank <= 414 ~ 1,
    TRUE ~ 0
  ))
QUANTILES <- QUANTILES %>%
  mutate(max = MIX$Freq[QUANTILES$min],
         min = MIX$Freq[QUANTILES$max],
  )

FF <- MADE.IT %>%
  group_by(quantile) %>%
  summarise(in_final = n())
left_join(QUANTILES, FF) %>%
  mutate(share = round(in_final / total * 100, 1)) %>%
  mutate(diff = share-lag(share))


MIX %>%
  filter(Freq >= 15) %>%
  ungroup() %>%
  mutate(quantile = ntile(-formerrank, 10)) %>%
  group_by(quantile) %>%
  summarise(total = n(),
            in_final = sum(is_in_top_list),
            min = min(formerrank),
            max = max(formerrank),
            share = round(in_final / total * 100, 1)) %>%
  mutate(diff = share-lag(share)) %>%
  summarise(median(diff, na.rm = T))
aa <- MIX %>%
  filter(Freq >= 15) %>%
  group_by(Freq) %>%
  summarise(total = n(),
            in_final = sum(is_in_top_list),
            min = min(Freq),
            max = max(Freq),
            share = round(in_final / total * 100, 1)) %>%
  ungroup() %>%
  arrange(desc(Freq))
cor.test(aa$Freq, aa$share, method = "spearman")  

MIX <- MIX %>%
  group_by(Freq) %>%
  summarise(total = n(),
            in_final = sum(is_in_top_list),
            share = round(in_final / total * 100, 1)) %>%
  arrange(desc(Freq)) %>%
  mutate(diff = share-lag(share))
MIX %>%
  filter(Freq >= 15) %>%
  summarise(mean(diff, na.rm = T))

quantile(MIX$Freq)
cor.test(MIX$Freq, MIX$share, method = "kendall")
plot(MIX$share, MIX$Freq)
library(ggplot2)
ggplot(data = MIX %>% filter(Freq <= 150 & Freq >= 20),
       aes(x = Freq, y = share)) +
  geom_point()

head(ALL, 100) %>%
  mutate(rank = seq(1:100)) %>%
  select(rank, everything()) %>%
  writexl::write_xlsx("Output\\finallist-temp.xlsx")

FINFIN <- head(ALL, 100)
FINFIN <- FINFIN %>%
  select(publisher, "journals" = nishikawa, predatory, globalsouth)

writexl::write_xlsx()

CSS <- readxl::read_xlsx("Data\\04_publishers_new-15.xlsx") %>%
  select("publisher" = PUBLISHER_NAME,
         "webscrape_url" = PUBLISHER_URL,
         "webscrape_css_selector" = JOURNALNAME_CSS,
         
         "webscrape_url2" = PUBLISHER_PAGE_URL_BEFORE)

harmonia <- read.delim(file = "Data\\03_publishers_harmonization.txt", header = TRUE, sep = "\t")
colnames(harmonia) = c("Deviation", "Harmonization")

for (i in 1:nrow(harmonia)) {
  
  CSS$publisher[CSS$publisher == harmonia$Deviation[i]] <- harmonia$Harmonization[i]

}

rm(harmonia)

FINFIN %>%
  left_join(CSS) %>%
  filter(!(publisher == "Springer" & is.na(webscrape_url))) %>%
  filter(is.na(webscrape_url) & is.na(webscrape_url2))
         