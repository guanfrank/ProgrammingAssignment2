complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    if(grep("specdata", directory) == 1) {
        directory <- ("./specdata/")
    }
    id_len <- length(id)
    complete_data <- rep(0, id_len)

    count <- 1
    for(i in id) {
        fileNam <- sprintf("%s/%03d.csv",directory,i)
        current_file <- read.csv(fileNam)
        complete_data[count] <- sum(complete.cases(current_file))
        count <- count + 1
    }
    result <- data.frame(id = id, nobs = complete_data)
    return(result)
}
