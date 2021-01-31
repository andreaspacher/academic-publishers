# read file with information about all publishers (ALL);
# if a specific publisher is needed (e.g. Univ of Liverpool Press),
# use the short name for that publisher in the 2nd column of the CSV-file,
# and: ALL <- ALL[(ALL$PUBLISHER_FILENAME == "liverpool"), ] 

ALL <- read.csv("..\\Data\\04_publishers.csv", header = T, sep = ";")

alljournals <- list()
warninglist <- list()
errorlist <- list()

# source the function getjournals()
source("Function\\function_getjournals.R")

for (i in 1:nrow(ALL)) {
  alljournals[[i]] <- tryCatch(
    {
      cat(i, ": Now trying out", ALL[i, 1], "\n")
      getjournals(ALL[i, ])
    },
    warning = function(warning_condition) {
      warninglist[[i]] <- ALL[i, 2]
      cat("warning with regards to ", ALL[i, 1], "\n")
    },
    error = function(error_condition) {
      errorlist[[i]] <- ALL[i, 2]
      cat("error with regards to ", ALL[i, 1], "\n")
    }
  )
}

DF <- dplyr::bind_rows(alljournals)
rownames(DF) <- NULL
DF <- unique(DF)

currentDate <- Sys.Date()
write.csv(DF,
  file = paste0(
    FilePath,
    paste0("alljournals-", currentDate, ".csv")
  ),
  row.names = F
)

rm(list = ls())
