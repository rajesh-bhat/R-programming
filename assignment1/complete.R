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
        

        library(plyr)
        #creating a dataframe with column names id and nobs
        d<-data.frame(id=numeric(),nobs=numeric(),stringsAsFactors=FALSE)
        #j used for appending the row in the loop
        j<-1
        for(i in id)
        {
                filename <- sprintf("%03d.csv", i)
                source <- paste(directory, filename, sep="/")
                
                #read.csv will be applied on filename
                ldf <- lapply(source, read.csv)
                df<-ldply(ldf)
                
                #omitting all the rows which contains na
                newdata <- na.omit(df)
                
                #inserting into dataframe id,no of rows without na
                d[j,]<-c(i,nrow(newdata))
                j<-j+1
                
        }
        #for printing the desired result
        d
        
}
