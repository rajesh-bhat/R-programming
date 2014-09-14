pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
		
		library(plyr)
        filenames <- sprintf("%03d.csv", id)
        filenames <- paste(directory, filenames, sep="/") # adds specdata/000cs1  ....
        ldf <- lapply(filenames, read.csv) #When we want to apply a function to each element of a list in turn and get a list back.
        df<-ldply(ldf) #For each element of a list, apply function then combine results into a data frame.
        # df is  list of data.frames ie make a dataframe by combing all the column values
        print(df)
        mean(df[, pollutant], na.rm = TRUE)
	
}
