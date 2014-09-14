corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

        library(plyr)
        id<-1:332
        lst<-c()
        for(i in id)
        {
        	filename <- sprintf("%03d.csv", i)
                source <- paste(directory, filename, sep="/")
                
                #read.csv will be applied on filename
                ldf <- lapply(source, read.csv)
                df<-ldply(ldf)
                
                #omitting all the rows which contains na
                newdata <- na.omit(df)


                if(nrow(newdata)>threshold)
                {
                	#appending to the list
                	lst<-c(lst,cor(newdata[,"nitrate"],newdata[,"sulfate"]))


                }
                
        }
        #returning the list
        lst
}