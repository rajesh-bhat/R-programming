rankhospital <- function(state, outcome,num)
{
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate

	data <- read.csv("outcome-of-care-measures.csv")

	#getting all the states values
	allstates<-data[ ,7]

	#getting unique values from allstates

	unique_states<-unique(allstates)


	#
	disease<-c("heart attack","heart failure","pneumonia")
	
	
	if(state %in% unique_states)
	{
		filterstate<-subset(data,State==state)
		if(outcome %in% disease)
		{
			if(outcome=="heart attack")
			{
				newdta<-subset(filterstate,select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
			}
			else if(outcome=="heart failure")
			{
				newdta<-subset(filterstate,select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
			}
			else
			{
				newdta<-subset(filterstate,select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
			}
			
			naremove<-subset(newdta,newdta[,2]!="Not Available")
			ans<-naremove[order(as.numeric(as.character(naremove[,2])),naremove[,1]),]
			


			

			if(num=="best")
			{
				avector <- as.vector(ans[1,1]) #conerting to a vecxtor
				avector
				
			}
			else if(num=="worst")
			{
				avector <- as.vector(ans[nrow(ans),1]) #conerting to a vecxtor
				avector
				
			}
			else
			{
				avector <- as.vector(ans[num,1]) #conerting to a vecxtor
				avector

			}




			
		}
		else
		{
			stop("invalid outcome")
		}
	}else
	{
		stop("invalid state")
	}



}