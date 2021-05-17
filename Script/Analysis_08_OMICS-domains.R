library(tidyverse)
library(urltools)

JJ <- read_csv("Output/alljournals-2021-02-05.csv")

OMICS <- JJ %>%
  filter(publisher == "OMICS") %>%
  mutate(domain = suffix_extract(domain(url))) %>%
  mutate(domainname = paste0(domain$domain, ".", domain$suffix))

# order by domains
topdomains <- sort(table(OMICS$domainname), decreasing = T)

# =====================================
# top 5 results, as of 17 May 2021, are:
# =====================================
# --- 1 --- omicsonline.org   436
# --- 2 --- imedpub.com       129
# --- 3 --- scitechnol.com     54
# --- 4 --- rroij.com          33
# --- 5 --- tsijournals.com    22