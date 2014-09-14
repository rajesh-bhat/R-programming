demo <- function(outcome,num)
{
	data <- read.csv("outcome-of-care-measures.csv")
	d<-data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)
	#data<-data[,1:11]
	#getting all the states values
	

	#getting unique values from allstates

	
	disease<-c("heart attack","heart failure","pneumonia")
	if(outcome %in% disease)
	{
	splitted<-split(data,data$State)
	len<-length(splitted)
	j<-1
	for(i in 1:len)
	{
		daa<-splitted[[i]]
		newdta<-subset(daa,select=c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
		naremove<-subset(newdta,newdta[,3]!="Not Available")
		ans<-naremove[order(as.numeric(as.character(naremove[,3])),naremove[,1]),]
		
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
	d
   }
   else
   {
    	stop("invalid outcome")
   }
	

	

}	