corr <- function(directory, threshold = 0) {
        source("complete.R")
        files_and_nobs <- complete(directory)
        files_and_nobs <- files_and_nobs[files_and_nobs["nobs"] > threshold, ]
        cor_vector <- numeric()
        if (nrow(files_and_nobs) != 0) {
                file_names <- paste(formatC(unlist(files_and_nobs["id"]), width=3,flag="0"),"csv",sep=".")
                file_paths <- paste(directory,"/",file_names,sep="")
                for(file in file_paths) {
                        data <- read.csv(file)
                        cases <- complete.cases(data[c("sulfate","nitrate")])
                        data <- data[cases,]
                        sulfate <- unlist(data[["sulfate"]])
                        nitrate <- unlist(data[["nitrate"]])
                        cor_vector <- c(cor_vector,cor(sulfate,nitrate))
                }
                cor_vector
        } else {
                numeric(0)
        }
}