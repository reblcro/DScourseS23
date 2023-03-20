library(tidyverse)
library(readxl)
library(tidyr)
library(RSQLite)
library(ggplot2)


#set file path
path<-"C:\\Users\\rebec\\OneDrive - University of Oklahoma\\Documents\\Research\\Tax authority aggressiveness\\Single Tab Data Sets\\"
file1<-"5_Operating_metrics_registration_an.xlsx"
file2<-"7_Operating_metrics_audit_criminal_.xlsx"
file3<-"10_Derived_indicators_revenue_and_r.xlsx"

#Read in Number of taxpayers dataset
ntaxpay<-read_xlsx(paste(path,file1,sep=""))
#remove unneeded rows and columns
ntaxpay<-ntaxpay[-c(1:5),-c(2:4,8:13)]
#add column headings using data in the first row, then remove the row from the dataframe
colnames(ntaxpay)<-as.character(unlist(ntaxpay[1,]))

if(is.na(colnames(ntaxpay)[1])){
  colnames(ntaxpay)[1] <- "jurisdiction"
}
ntaxpay<-ntaxpay[-1,]


#Read in Number of audits
naudits<-read_xlsx(paste(path,file2,sep=""))
#remove unneeded rows and columns
naudits<-naudits[-c(1:4),-c(5:7)]
#add column headings using data in the first row, then remove the row from the dataframe
colnames(naudits)<-as.character(unlist(naudits[1,]))

if(is.na(colnames(naudits)[1])){
  colnames(naudits)[1] <- "jurisdiction"
}
naudits<-naudits[-1,]


#Read in Number of audits with assessment
nauditsw<-read_xlsx(paste(path,file2,sep=""))
#remove unneeded rows and columns
nauditsw<-nauditsw[-c(1:4),-c(2:4)]
#add column headings using data in the first row, then remove the row from the dataframe
colnames(nauditsw)<-as.character(unlist(nauditsw[1,]))

if(is.na(colnames(nauditsw)[1])){
  colnames(nauditsw)[1] <- "jurisdiction"
}
nauditsw<-nauditsw[-1,]

#Read in dataset with external variables for each country
#1
gdplocal<-read_xlsx(paste(path,file3,sep=""))
#remove unneeded rows and columns
gdplocal<-gdplocal[-c(1:4),-c(6:14)]
#add column headings using data in the first row, then remove the row from the dataframe
colnames(gdplocal)<-as.character(unlist(gdplocal[1,]))

if(is.na(colnames(gdplocal)[1])){
  colnames(gdplocal)[1] <- "jurisdiction"
}
if(is.na(colnames(gdplocal)[2])){
  colnames(gdplocal)[2] <- "ISO"
}
gdplocal<-gdplocal[-1,]

#2
govrevlocal<-read_xlsx(paste(path,file3,sep=""))
#remove unneeded rows and columns
govrevlocal<-govrevlocal[-c(1:4),-c(3:8,12:14)]
#add column headings using data in the first row, then remove the row from the dataframe
colnames(govrevlocal)<-as.character(unlist(govrevlocal[1,]))

if(is.na(colnames(govrevlocal)[1])){
  colnames(govrevlocal)[1] <- "jurisdiction"
}
if(is.na(colnames(govrevlocal)[2])){
  colnames(govrevlocal)[2] <- "ISO"
}
govrevlocal<-govrevlocal[-1,]

#check data by looking at first 5 rows
head(ntaxpay)
head(naudits)
head(nauditsw)
head(gdplocal)
head(govrevlocal)

#I have 5 separate "wide" data frames covering OECD countries from 2018-2020
#Now I will convert each to "long" data frames with only 3 columns
gathercols<-c("2018","2019","2020")
ntaxpay_1<-ntaxpay %>% 
  pivot_longer(cols=gathercols,names_to="year",values_to="ntaxpayers",values_drop_na=TRUE)
naudits_1<-naudits %>% 
  pivot_longer(cols=gathercols,names_to="year",values_to="naudits",values_drop_na=TRUE)
nauditsw_1<-nauditsw %>% 
  pivot_longer(cols=gathercols,names_to="year",values_to="nauditsw",values_drop_na=TRUE)
gdplocal_1<-gdplocal %>% 
  pivot_longer(cols=gathercols,names_to="year",values_to="gdplocal",values_drop_na=TRUE)
govrevlocal_1<-govrevlocal %>% 
  pivot_longer(cols=gathercols,names_to="year",values_to="govrevlocal",values_drop_na=TRUE)

#Finally I will merge them into one dataframe using SQLite
#connect to the SQLite database
con <- dbConnect(SQLite(), dbname = "mydatabase.db")

# Write the data frames to tables in the SQLite database
dbWriteTable(con, "ntaxpay_table", ntaxpay_1, overwrite=TRUE)
dbWriteTable(con, "naudits_table", naudits_1, overwrite=TRUE)
dbWriteTable(con, "nauditsw_table", nauditsw_1, overwrite=TRUE)
dbWriteTable(con, "gdplocal_table", gdplocal_1, overwrite=TRUE)
dbWriteTable(con, "govrevlocal_table", govrevlocal_1, overwrite=TRUE)

# Merge the n columns based on the jurisdiction and year columns using a LEFT JOIN
taxauthstats <- dbGetQuery(con, "SELECT gdplocal_1.*,govrevlocal_1.govrevlocal,ntaxpay_1.ntaxpayers, naudits_1.naudits, nauditsw_1.nauditsw FROM gdplocal_table gdplocal_1 
                            LEFT JOIN govrevlocal_table govrevlocal_1 ON gdplocal_1.Jurisdiction = govrevlocal_1.Jurisdiction AND gdplocal_1.year = govrevlocal_1.year
                            LEFT JOIN ntaxpay_table ntaxpay_1 ON gdplocal_1.Jurisdiction = ntaxpay_1.Jurisdiction AND gdplocal_1.year = ntaxpay_1.year
                            LEFT JOIN naudits_table naudits_1 ON gdplocal_1.Jurisdiction = naudits_1.Jurisdiction AND gdplocal_1.year=naudits_1.year 
                            LEFT JOIN nauditsw_table nauditsw_1 ON gdplocal_1.Jurisdiction = nauditsw_1.Jurisdiction AND gdplocal_1.year=nauditsw_1.year")

# Disconnect from the database
dbDisconnect(con)

# Print the resulting merged data frame to check if it was created correctly
head(taxauthstats)

# Convert n columns to numeric instead of character
taxauthstats$gdplocal <- as.numeric(unlist(taxauthstats$gdplocal))
taxauthstats$govrevlocal <- as.numeric(unlist(taxauthstats$govrevlocal))
taxauthstats$ntaxpayers <- as.numeric(unlist(taxauthstats$ntaxpayers))
taxauthstats$naudits <- as.numeric(unlist(taxauthstats$naudits))
taxauthstats$nauditsw <- as.numeric(unlist(taxauthstats$nauditsw))

#Create new data columns
taxauthstats$govrevgdp <- taxauthstats$govrevlocal/taxauthstats$gdplocal
taxauthstats$auditrate<-taxauthstats$naudits/taxauthstats$ntaxpayers
taxauthstats$assessrate<-taxauthstats$nauditsw/taxauthstats$ntaxpayers
taxauthstats$logtaxpayers<-log(taxauthstats$ntaxpayers)

# Examine data & outliers
#summary(taxauthstats)
#outliers<-taxauthstats[taxauthstats$assessrate>1,]

# Drop rows: with missing information, where ntaxpayers=0, where auditrate>1
taxauthstats<-na.omit(taxauthstats)
taxauthstats<-taxauthstats[taxauthstats$ntaxpayers>0,]
taxauthstats<-taxauthstats[taxauthstats$auditrate<=1,]
summary(taxauthstats)


#Visualizations
#use this to show that gov't rev as a % of gdp is significantly negatively 
#related to audit assessment rate - may indicate that the audit function is an 
#important revenue source
ggplot(taxauthstats, aes(x=assessrate, y=govrevgdp)) +
  geom_point(alpha=.5, colour="dark blue") + 
  theme_classic()+
  labs(title="Government Revenues and Tax Audit Assessment Rates", 
       y="Gov't Revenue as % GDP",
       x="Audits with assessment per active taxpayer")

ggplot(taxauthstats, aes(x=assessrate, y=govrevgdp)) +
  geom_smooth() + 
  theme_classic() +
  labs(title="Government Revenues and Tax Audit Assessment Rates", 
       y="Gov't Revenue as % GDP",
       x="Audits with assessment per active taxpayer")

#while visually we see the same pattern with the overall audit rate, the regression
#analysis shows a less significant relationship
ggplot(taxauthstats, aes(x=auditrate, y=govrevgdp)) +
  geom_point(alpha=.5, color="dark green") + 
  theme_classic() +
  labs(title="Government Revenues and Tax Audit Rates", 
       y="Gov't Revenue as % GDP",
       x="Audits per active taxpayer")  

