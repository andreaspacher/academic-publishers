library(tidyverse)

# This script merges the journals of all predatory publishers together.
#
# Use this script *after* you have scraped the predatory publishers,
# i.e. after having used the other *.R-scripts in this folder
# (Script\Scrape-Predatory) or after having used the general
# scraping file in Script\Analysis_06_Extract-Journals.R.
# 
# The scraped journals, grouped by distinct publishers,
# should be saved in distinct files in the folder
# Output\Journals\Predatory.
#
# Here, we will simply merge them into a single document.

PRED <- list.files(path = "Output\\Journals\\Predatory", pattern="*.csv")
PRED <- lapply(paste0("Output\\Journals\\Predatory\\", PRED), read_csv)
PRED <- data.table::rbindlist(PRED, fill = T)

write_csv(PRED,
                    paste0("Output\\Journals\\predatory-journals-", Sys.Date(), ".csv"))

# Some statistics
PRED %>%
  group_by(publisher) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  View()
