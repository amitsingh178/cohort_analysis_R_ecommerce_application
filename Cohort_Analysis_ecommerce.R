# Importing Data from multiple CSV Files, storing them in a dataframe called mydata
library(xlsx)
mydata1=read.csv("/Users/amitsingh/Downloads/Data_Sample_1.csv", header=TRUE)
mydata2=read.csv("/Users/amitsingh/Downloads/Data_Sample_2.csv", header=TRUE)
names(mydata2) <- names(mydata1)
mydata <- rbind(mydata1, mydata2)

# Converting All columns that have date into date format, and creating new columns having only month and year
mydata$Member_Since <- as.Date(mydata$Member_Since, format = "%d/%m/%y")
mydata$Transaction.Date <- as.Date(mydata$Transaction.Date, format = "%d/%m/%y")

library(zoo)
mydata$Cohort <- as.yearmon(mydata$Member_Since)
mydata$Cohort2 <- as.yearmon(mydata$Transaction.Date)

# Pivoting Dataframes for calculating absolute cohort, cumulative cohort and revenue cohort
library(plyr)
df <- ddply(mydata, .(Cohort, Cohort2), summarize, registered= length(unique(User.ID)))
df_relative <- ddply(mydata, .(Cohort), summarize, registered= length(unique(User.ID)))
df_revenue <- ddply(mydata, .(Cohort, Cohort2), summarize, revenue= sum(Commision.From.Network))


# This part of the code saves the dataframes into a tabular format for each cohorts d1, d2 and d3
registered_month <- unique(df$Cohort)
transaction_month <- unique(df$Cohort2)
text_month <- as.character(transaction_month)

col.names = union(c("Member_Since"),text_month)
d1 <- as.data.frame(matrix(0, ncol = length(transaction_month)+1, nrow = length(unique(df$Cohort))))
colnames(d1) = union(c("Member_Since"),text_month)
d1$Member_Since <- as.yearmon(unique(df$Cohort))
d2<- d1
d3<- d1

for (i in 1:length(transaction_month)){
	for (k in 1:length(registered_month)) {
		for(j in 1:length(df$Cohort)){
			if (df$Cohort2[j] == transaction_month[i]){
				if(df$Cohort[j] == registered_month[k]){
					d1[k,i+1] = as.numeric(df$registered[j])
					d2[k,i+1] = as.double((100*df$registered[j])/(df_relative$registered[k]))
					d3[k,i+1] = as.numeric(df_revenue$revenue[j])
				}				
			}
		}
	}
}

# This part of the code exports all the cohort tables into excel files

write.xlsx(d1, file="/Users/amitsingh/Desktop/Cohort.xlsx", sheetName="Absolute Cohort", append=TRUE)
write.xlsx(d2, file="/Users/amitsingh/Desktop/Cohort.xlsx", sheetName="Cumulative Cohort", append=TRUE)
write.xlsx(d3, file="/Users/amitsingh/Desktop/Cohort.xlsx", sheetName="Revenue Cohort", append=TRUE)