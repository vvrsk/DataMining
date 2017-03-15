#Check the dimensions of Data
dim(German.Credit)
# Check the Variabls of the Data
names(German.Credit)
str(German.Credit)

#QA- Look at the first 10 values of the data

German.Credit[1:10,]
#or
#head(German.Credit)

# Summary Statistics of the Data
summary(German.Credit)

#Get the number of missing values

sapply(German.Credit, function(x) sum(is.na(x)))


#Get the number of Good and Bad Creditors

summary(German.Credit$RESPONSE)

#plot the frequencies

hist(German.Credit$RESPONSE)

# Get the number of good vs Bad credit risks

resp=table(German.Credit$RESPONSE)
t=as.data.frame(resp)
names(t)[1]='Credit Risk'

###########################################################################################################
#												Analysis
#
#
#
#
###########################################################################################################


#Aggregate of the table based on the type of account

agg_acct=table(German.Credit$CHK_ACCT) 

#look at the aggregation
View(agg_acct)

# Split the data based onthe account type
split_acct= split(German.Credit,German.Credit$CHK_ACCT)

#look at the split data
#View(split_acct)

#Look for the distribution of 'Response' in each category of Account 
dist_acct_0 = table(split_acct$`0`$RESPONSE)
dist_acct_1 = table(split_acct$`1`$RESPONSE)
dist_acct_2 = table(split_acct$`2`$RESPONSE)
dist_acct_3 = table(split_acct$`3`$RESPONSE)

#Look at the distribution

View(dist_acct_0)
View(dist_acct_1)
View(dist_acct_2)
View(dist_acct_3)



#Aggregate of the table based on the History

agg_hist=table(German.Credit$HISTORY) 

#look at the aggregation
View(agg_hist)

# Split the data based onthe history

split_hist= split(German.Credit,German.Credit$HISTORY)

#look at the split data
#View(split_hist)

#Look for the distribution of 'Response' in each category of history 
dist_hist_0 = table(split_hist$`0`$RESPONSE)
dist_hist_1 = table(split_hist$`1`$RESPONSE)
dist_hist_2 = table(split_hist$`2`$RESPONSE)
dist_hist_3 = table(split_hist$`3`$RESPONSE)
dist_hist_4 = table(split_hist$`4`$RESPONSE)

#Look at the distribution / QA the distribution

View(dist_hist_0)
View(dist_hist_1)
View(dist_hist_2)
View(dist_hist_3)
View(dist_hist_4)


#Aggregate of the table based on the Employment

agg_emp=table(German.Credit$EMPLOYMENT) 

#look at the aggregation
View(agg_emp)

# Split the data based onthe history

split_emp= split(German.Credit,German.Credit$EMPLOYMENT)

#look at the split data
#View(split_emp)

#Look for the distribution of 'Response' in each category of EMPLOYMENT 
dist_emp_0 = table(split_emp$`0`$RESPONSE)
dist_emp_1 = table(split_emp$`1`$RESPONSE)
dist_emp_2 = table(split_emp$`2`$RESPONSE)
dist_emp_3 = table(split_emp$`3`$RESPONSE)
dist_emp_4 = table(split_emp$`4`$RESPONSE)

#Look at the distribution / QA the distribution

View(dist_emp_0)
View(dist_emp_1)
View(dist_emp_2)
View(dist_emp_3)
View(dist_emp_4)


#Aggregate of the table based on the Present residence status

agg_res=table(German.Credit$PRESENT_RESIDENT) 

#look at the aggregation
View(agg_res)

# Split the data based onthe history

split_res= split(German.Credit,German.Credit$PRESENT_RESIDENT)

#look at the split data
#View(split_res)

#Look for the distribution of 'Response' in each category of EMPLOYMENT 
dist_res_0 = table(split_res$`0`$RESPONSE)
dist_res_1 = table(split_res$`1`$RESPONSE)
dist_res_2 = table(split_res$`2`$RESPONSE)
dist_res_3 = table(split_res$`3`$RESPONSE)

#Look at the distribution / QA the distribution

View(dist_res_0)
View(dist_res_1)
View(dist_res_2)
View(dist_res_3)


#Aggregate of the table based on the Job

agg_job=table(German.Credit$JOB) 

#look at the aggregation
View(agg_job)

# Split the data based onthe history

split_job= split(German.Credit,German.Credit$JOB)


#Look for the distribution of 'Response' in each category of EMPLOYMENT 
dist_job_0 = table(split_job$`0`$RESPONSE)
dist_job_1 = table(split_job$`1`$RESPONSE)
dist_job_2 = table(split_job$`2`$RESPONSE)
dist_job_3 = table(split_job$`3`$RESPONSE)

#Look at the distribution / QA the distribution

View(dist_job_0)
View(dist_job_1)
View(dist_job_2)
View(dist_job_3)

#Aggregate of the table based on the Nationality

agg_frn=table(German.Credit$FOREIGN) 

#look at the aggregation
View(agg_frn)

# Split the data based onthe history

split_frn= split(German.Credit,German.Credit$FOREIGN)


#Look for the distribution of 'Response' in each category of EMPLOYMENT 
dist_frn_0 = table(split_frn$`0`$RESPONSE)
dist_frn_1 = table(split_frn$`1`$RESPONSE)
dist_frn_2 = table(split_frn$`2`$RESPONSE)
dist_frn_3 = table(split_frn$`3`$RESPONSE)

#Look at the distribution / QA the distribution

View(dist_frn_0)
View(dist_frn_1)
View(dist_frn_2)
View(dist_frn_3)

