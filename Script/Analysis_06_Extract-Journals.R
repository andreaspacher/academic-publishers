library(tidyverse)
# read file with information about all publishers (ALL);
# if a specific publisher is needed (e.g. Univ of Liverpool Press),
# use the short name for that publisher in the 2nd column of the CSV-file,
# and:
# ALL <- ALL[(ALL$PUBLISHER_FILENAME == "liverpool"), ] 

#temp
ALL <- readxl::read_xlsx("./Data/04_publishers.xlsx")
ALL %>% filter(grepl("Scientific", PUBLISHER_NAME)) %>% select(PUBLISHER_NAME)

alljournals <- list()

# if you want to choose only a specific publisher, use this:
# ALL <- ALL[(ALL$PUBLISHER_NAME) == "SAGE", ]
# source the function getjournals()
source("./Script/Function/function_getjournals.R")

for (i in 1:nrow(ALL)) {
  alljournals[[i]] <- tryCatch(
    {
      cat(paste0(i, ": Now trying out ", ALL[i, 1], "\n"))
      getjournals(ALL[i, ])
    },
    warning = function(warning_condition) {
      cat(paste0("warning with regards to ", ALL[i, 1], "\n"))
      message(warning_condition)
      cat("\n")
      getjournals(ALL[i, ])
    },
    error = function(error_condition) {
      cat("error with regards to ", ALL[i, 1], "\n")
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
  file = paste0("Output/alljournals-", currentDate, "-", ALL$PUBLISHER_FILENAME[1], ".csv"),
  row.names = F
)

rm(list = ls())
