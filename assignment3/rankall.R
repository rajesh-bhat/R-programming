rankall<- function(outcome,num)
{
	data <- read.csv("outcome-of-care-measures.csv")
	
	#crearting the dataframe
	d<-data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)
	
	
	disease<-c("heart attack","heart failure","pneumonia")
	
	if(outcome %in% disease)
	{
		#splitting the data based on the state
		splitted<-split(data,data$State)
		
		len<-length(splitted)
		j<-1
		
		if(outcome=="heart attack")
		{
			for(i in 1:len)
			{
				#accessing the state one by one
				daa<-splitted[[i]]
				newdta<-subset(daa,select=c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
				naremove<-subset(newdta,newdta[,3]!="Not Available")
				ans<-naremove[order(as.numeric(as.character(naremove[,3])),naremove[,1]),]
		
				#appending the required data into the data frame
				#based on the best,worst,num condition
				if(num=="best")
				{
					hosp<-as.character(ans[1,2])
					st<-as.character(ans[1,1])
					d[j,] <-c(hosp, st)
				
				}
				else if(num=="worst")
				{
					hosp<-as.character(ans[nrow(ans),2])
					st<-as.character(ans[nrow(ans),1])
					d[j,] <-c(hosp, st)
				
				}
				else
				{
					hosp<-as.character(ans[num,2])
					st<-as.character(ans[num,1])
					d[j,] <-c(hosp, st)
				
				}
				j<-j+1
			}
		}

		
	
		
	d
 	}
   else
   {
    	stop("invalid outcome")
   }
	

	

}	