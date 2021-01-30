# FilePath <- "C:\\Users\\pac\\Downloads\\largest-publishers\\"
FilePath <- "C:\\Users\\andre\\OneDrive\\2020 Research\\2020-07 Largest Academic Publishers\\"

ALL <- read.csv(paste0(FilePath, "extract_all_publishers.csv"), header = T, sep = ";") # , fileEncoding="latin1"
ALL <- ALL[(ALL$PUBLISHER_FILENAME == "pitt"
| ALL$PUBLISHER_FILENAME == "liverpool"
| ALL$PUBLISHER_FILENAME == "ecronicon"), ]

alljournals <- list()
warninglist <- list()
errorlist <- list()

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
