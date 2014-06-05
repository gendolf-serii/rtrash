pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        file_names <- paste(formatC(id, width=3,flag="0"),"csv",sep=".")
        file_paths <- paste(directory,"/",file_names,sep="")
        
        data_vector <- numeric()
        
        for(file in file_paths) {
                data <- unlist(read.csv(file)[pollutant])
                data <- data[!is.na(data)]
                data_vector <- c(data_vector,data)
        }
        mean(data_vector)
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
}