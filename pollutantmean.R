pollutantmean <- function (directory, pollutant, id = 1:332)
{
    ## 'directory' os a character vector of length 1 indicating the location of the CSV files
    ## 'pollutant' is a character vector of length 1 indication the name of the polutant for which we will calculate the mean; either "sulfate" or "nitrate"/
    ## 'id' is an integer vector indicating the monitor ID numbers to be used.
    ## Return the mean of the polutant across all monitors lis in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    pollutantVector  <- c()

    for (i in id) {
        fileNam <- sprintf("%s/%03d.csv",directory,i)
        results <- read.csv(fileNam)
        pollutantVector <- c(pollutantVector,results[,pollutant])

    }
    mean(pollutantVector, na.rm = TRUE)
}
