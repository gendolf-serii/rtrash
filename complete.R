complete <- function(directory, id = 1:332) {
        file_names <- paste(formatC(id, width=3,flag="0"),"csv",sep=".")
        file_paths <- paste(directory,"/",file_names,sep="")       
        nobs <- numeric()        
        for(file in file_paths) {
                nobs_in_data <- sum(complete.cases(read.csv(file)))
                nobs <- c(nobs,nobs_in_data)
        }
        data.frame(id,nobs)
}