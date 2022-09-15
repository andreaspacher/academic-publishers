library(tidyverse)
# read file with information about all publishers (ALL);
ALL <- readxl::read_xlsx("./Data/04_publishers.xlsx")

# if a specific publisher is needed (e.g. Hindawi),
# use the name for that publisher:
# SCRAPE <- ALL[(ALL$PUBLISHER_NAME == "Hindawi"), ] 

SCRAPE <- ALL[ALL$PUBLISHER_NAME == "Hindawi", ]

alljournals <- list()

# source the function getjournals()
source("./Script/Function/function_getjournals.R")

for (i in 1:nrow(SCRAPE)) {
  alljournals[[i]] <- tryCatch(
    {
      cat(paste0(i, ": Now trying out ", SCRAPE[i, 1], "\n"))
      getjournals(SCRAPE[i, ])
    },
    warning = function(warning_condition) {
      cat(paste0("warning with regards to ", SCRAPE[i, 1], "\n"))
      message(warning_condition)
      cat("\n")
      getjournals(SCRAPE[i, ])
    },
    error = function(error_condition) {
      cat("error with regards to ", SCRAPE[i, 1], "\n")
      message(error_condition)
      cat("\n")
    }
  )
}

DF <- dplyr::bind_rows(alljournals)
rownames(DF) <- NULL
DF <- unique(DF)

currentDate <- Sys.Date()
write.csv(DF,
  file = paste0("Output/Journals/",SCRAPE[1], "-", currentDate, "-", ALL$PUBLISHER_NAME[1], ".csv"),
  row.names = F
)

rm(list = ls())
