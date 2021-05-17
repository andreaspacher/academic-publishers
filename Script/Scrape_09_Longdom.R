ALL <- read.csv(".\\Data\\04_publishers.csv", header = T, sep = ";")

alljournals <- list()

ALL <- ALL[(ALL$PUBLISHER_NAME) == "Longdom", ]

# source the function getjournals()
source(".\\Script\\Function\\function_getjournals.R")

for (i in 1:nrow(ALL)) {
  alljournals[[i]] <- tryCatch(
    {
      cat(i, ": Now trying out", ALL[i, 1], "\n")
      getjournals(ALL[i, ])
    },
    warning = function(warning_condition) {
      cat("warning with regards to ", ALL[i, 1], "\n")
    },
    error = function(error_condition) {
      cat("error with regards to ", ALL[i, 1], "\n")
    }
  )
}

DF <- dplyr::bind_rows(alljournals)
rownames(DF) <- NULL
DF <- unique(DF)

currentDate <- Sys.Date()
write.csv(DF,
          file = paste0("Output//09_Longdom-", currentDate, ".csv"),
          row.names = F
)

rm(list = ls())
