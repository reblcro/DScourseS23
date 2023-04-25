library(tidyverse)
library(readxl)
library(tidyr)
library(RSQLite)

#set file path
path<-"C:\\Users\\rebec\\OneDrive - University of Oklahoma\\Documents\\Spring 2023\\Data Science for Econ\\Project\\External Data\\"
path2<-"C:\\Users\\rebec\\OneDrive - University of Oklahoma\\Documents\\Research\\Reference Files\\"
ISORA1<-"1_Revenue_collections.xlsx"
ISORA2<-"2_Resources_and_ICT_infrastructure.xlsx"
ISORA3<-"3_Staff_metrics.xlsx"
#ISORA5<-"5_Operating_metrics_registration_an.xlsx"
#ISORA7<-"7_Operating_metrics_audit_criminal_.xlsx"
ISORA10<-"10_Derived_indicators_revenue_and_r.xlsx"
WBAC<-"OGHIST.xlsx"
WGI<-"wgidataset.xlsx"
WVS<-"Justifiable_Cheating_on_taxes.xlsx"
ISO3<-"ISO3 Country Codes.xlsx"

#Create read excel file function
myread<-function(path,x,sheet){
  read_xlsx(paste(path,x,sep=""),sheet=sheet)
}

#Create heading cleaning function
heading_clean<-function(x){
  colnames(x)<-as.character(unlist(x[1,]))
  if(is.na(colnames(x)[1])){
    colnames(x)[1] <- "jurisdiction"
  }
  x<-x[-1,]
}

#Create pivot function
pivotdata<-function(x,z){
  gathercols<-colnames(x)[-c(1:z)]
  varname<-deparse(substitute(x))
  pivot_longer(x,cols=gathercols,names_to="year",values_to=varname,values_drop_na=TRUE)
}

#Create ISO code merge function
isomerge<-function(x){
  left_join(x,iso3_1,by=join_by("jurisdiction"=="ISORA_NAME"),keep=FALSE)
}

#Read in ISORA10 - Revenue related ratios (sheet 2)
rev_ratio<-myread(path,ISORA10,2)
#Split into dfs by variable & clean up headings
revgdp<-rev_ratio[-c(1:4),-c(2:4,8:16)] %>% heading_clean()

#Read in ISORA10 - Resource ratios (sheet 4)
resource_ratios<-myread(path,ISORA10,4)
#Split into dfs by variable & clean up headings
coc<-resource_ratios[-c(1:3),-c(2:7,11:16)] %>% heading_clean()
fte<-resource_ratios[-c(1:3),-c(2:4,8:16)] %>% heading_clean()

#Read in WBAC - Country Analytical History (sheet 3)
country_ah<-myread(path,WBAC,3)
#Split into dfs by variable & clean up headings
countryclass<-country_ah[-c(1:4,6:10),] %>% heading_clean()
names(countryclass)[names(countryclass) == "jurisdiction"]<-"ISO3"
names(countryclass)[names(countryclass) == "Data for calendar year :"]<-"jurisdiction"

#Read in ISORA10 - Staff allocation by function (sheet 5)
staff_alloc<-myread(path,ISORA10,5)
#Split into dfs by variable & clean up headings
staff_tpserv<-staff_alloc[-c(1:3),-c(5:16)] %>% heading_clean()
staff_enforce<-staff_alloc[-c(1:3),-c(2:4,11:16)] %>% heading_clean()
staff_other<-staff_alloc[-c(1:3),-c(2:10,14:16)] %>% heading_clean()

#Read in ISORA2 - Tax administration expenditures (sheet 1)
expend<-myread(path,ISORA2,1)
#Split into dfs by variable & clean up headings
expend_op<-expend[-c(1:4),-c(5:13)] %>% heading_clean()
expend_cap<-expend[-c(1:4),-c(2:10)] %>% heading_clean()
expend_it<-expend[-c(1:4),-c(2:7,11:13)] %>% heading_clean()

#Read in ISORA3 - Staff Gender Distribution (sheet 5)
staff_gender<-myread(path,ISORA3,5)
#Split into dfs by variable & clean up headings
staff_m<-staff_gender[-c(1:5),-c(5:19)] %>% heading_clean()
staff_f<-staff_gender[-c(1:5),-c(2:4,8:19)] %>% heading_clean()
staff_o<-staff_gender[-c(1:5),-c(2:7,11:19)] %>% heading_clean()

#Read in ISORA3 - Staff Academic Qualifications (sheet 2)
staff_aq<-myread(path,ISORA3,2)
#Split into dfs by variable & clean up headings
staff_masters<-staff_aq[-c(1:4),-c(5:7)] %>% heading_clean()

#Read in WGI - Rule of Law (sheet 6)
wgi_rol<-myread(path,WGI,6)
#Split into dfs by variable & clean up headings
ruleoflaw<-wgi_rol[-c(1:12,14),-c(4:8,10:14,16:20,22:26,28:32,34:38,40:44,46:50,52:56,58:62,64:68,70:74,76:80,82:86,88:92,94:98,100:104,106:110,112:116,118:122,124:128,130:134,136:140)] %>% heading_clean()
names(ruleoflaw)[2]<-"ISO3"

#Read in WGI - Control of Corruption (sheet 7)
wgi_coc<-myread(path,WGI,7)
#Split into dfs by variable & clean up headings
contofcorrupt<-wgi_coc[-c(1:12,14),-c(4:8,10:14,16:20,22:26,28:32,34:38,40:44,46:50,52:56,58:62,64:68,70:74,76:80,82:86,88:92,94:98,100:104,106:110,112:116,118:122,124:128,130:134,136:140)] %>% heading_clean()
names(contofcorrupt)[2]<-"ISO3"

#Read in WVS
wvs<-myread(path,WVS,1)
#Split into dfs by variable & clean up headings
wvs_tax<-wvs[-c(1:3,5:18,20:21),-c(1:2)] %>% heading_clean()

#Read in ISO3 Key
iso3<-myread(path2,ISO3,1) 
iso3_1<-iso3[-c(1:5),] %>% heading_clean()
names(iso3_1)<-c("Code","Country_Name","ISORA_NAME","WBAC_NAME")

#convert wide dataframes into long dataframes & merge
#ISORA data frames + merge in ISO3 codes
revgdp_1<-pivotdata(revgdp,1) %>% isomerge()
coc_1<-pivotdata(coc,1) %>% isomerge()
fte_1<-pivotdata(fte,1) %>% isomerge()
staff_tpserv_1<-pivotdata(staff_tpserv,1) %>% isomerge()
staff_enforce_1<-pivotdata(staff_enforce,1) %>% isomerge()
staff_other_1<-pivotdata(staff_other,1) %>% isomerge()
expend_op_1<-pivotdata(expend_op,1) %>% isomerge()
expend_cap_1<-pivotdata(expend_cap,1) %>% isomerge()
expend_it_1<-pivotdata(expend_it,1) %>% isomerge()
staff_m_1<-pivotdata(staff_m,1) %>% isomerge()
staff_f_1<-pivotdata(staff_f,1) %>% isomerge()
staff_o_1<-pivotdata(staff_o,1) %>% isomerge()
staff_masters_1<-pivotdata(staff_masters,1) %>% isomerge()

#Other data frames
countryclass_1<-pivotdata(countryclass,2)
ruleoflaw_1<-pivotdata(ruleoflaw,2)
contofcorrupt_1<-pivotdata(contofcorrupt,2)
wvs_tax_1<-pivot_longer(wvs_tax,cols=colnames(wvs_tax),names_to="jurisdiction",values_to="mean_cheattax_17_22")


#Merge data frames together with SQLite
con <- dbConnect(SQLite(), dbname = "mydatabase.db")

# Write the data frames to tables in the SQLite database
dbWriteTable(con, "revgdp_table", revgdp_1, overwrite=TRUE)
dbWriteTable(con, "coc_table", coc_1, overwrite=TRUE)
dbWriteTable(con, "fte_table", fte_1, overwrite=TRUE)
dbWriteTable(con, "staff_tpserv_table", staff_tpserv_1, overwrite=TRUE)
dbWriteTable(con, "staff_enforce_table", staff_enforce_1, overwrite=TRUE)
dbWriteTable(con, "staff_other_table", staff_other_1, overwrite=TRUE)
dbWriteTable(con, "expend_op_table", expend_op_1, overwrite=TRUE)
dbWriteTable(con, "expend_cap_table", expend_cap_1, overwrite=TRUE)
dbWriteTable(con, "expend_it_table", expend_it_1, overwrite=TRUE)
dbWriteTable(con, "staff_m_table", staff_m_1, overwrite=TRUE)
dbWriteTable(con, "staff_f_table", staff_f_1, overwrite=TRUE)
dbWriteTable(con, "staff_o_table", staff_o_1, overwrite=TRUE)
dbWriteTable(con, "staff_masters_table", staff_masters_1, overwrite=TRUE)
dbWriteTable(con, "countryclass_table", countryclass_1, overwrite=TRUE)
dbWriteTable(con, "ruleoflaw_table", ruleoflaw_1, overwrite=TRUE)
dbWriteTable(con, "contofcorrupt_table", contofcorrupt_1, overwrite=TRUE)
dbWriteTable(con, "wvs_tax_table", wvs_tax_1, overwrite=TRUE)
dbWriteTable(con, "iso3_table", iso3_1, overwrite=TRUE)

# Merge based on the jurisdiction and year columns using a LEFT JOIN
wgi_merge <- dbGetQuery(con, "SELECT ruleoflaw_1.*,contofcorrupt_1.contofcorrupt 
                       FROM ruleoflaw_table ruleoflaw_1 
                       LEFT JOIN contofcorrupt_table contofcorrupt_1
                       ON ruleoflaw_1.ISO3=contofcorrupt_1.ISO3 AND ruleoflaw_1.year=contofcorrupt_1.year")

isora_merge <- dbGetQuery(con, "SELECT coc_1.*, revgdp_1.revgdp, fte_1.fte, staff_tpserv_1.staff_tpserv, staff_enforce_1.staff_enforce,
                                staff_other_1.staff_other,expend_op_1.expend_op,expend_cap_1.expend_cap,expend_it_1.expend_it,
                                staff_m_1.staff_m,staff_f_1.staff_f,staff_o_1.staff_o,staff_masters_1.staff_masters
                          FROM coc_table coc_1 
                          LEFT JOIN revgdp_table revgdp_1 on coc_1.Code = revgdp_1.Code AND coc_1.year = revgdp_1.year
                          LEFT JOIN fte_table fte_1 ON coc_1.Code = fte_1.Code AND coc_1.year = fte_1.year
                          LEFT JOIN staff_tpserv_table staff_tpserv_1 ON coc_1.Code = staff_tpserv_1.Code AND coc_1.year = staff_tpserv_1.year
                          LEFT JOIN staff_enforce_table staff_enforce_1 ON coc_1.Code = staff_enforce_1.Code AND coc_1.year = staff_enforce_1.year
                          LEFT JOIN staff_other_table staff_other_1 ON coc_1.Code = staff_other_1.Code AND coc_1.year = staff_other_1.year
                          LEFT JOIN expend_op_table expend_op_1 ON coc_1.Code = expend_op_1.Code AND coc_1.year = expend_op_1.year
                          LEFT JOIN expend_cap_table expend_cap_1 ON coc_1.Code = expend_cap_1.Code AND coc_1.year = expend_cap_1.year
                          LEFT JOIN expend_it_table expend_it_1 ON coc_1.Code = expend_it_1.Code AND coc_1.year = expend_it_1.year
                          LEFT JOIN staff_m_table staff_m_1 ON coc_1.Code = staff_m_1.Code AND coc_1.year = staff_m_1.year
                          LEFT JOIN staff_f_table staff_f_1 ON coc_1.Code = staff_f_1.Code AND coc_1.year = staff_f_1.year
                          LEFT JOIN staff_o_table staff_o_1 ON coc_1.Code = staff_o_1.Code AND coc_1.year = staff_o_1.year
                          LEFT JOIN staff_masters_table staff_masters_1 ON coc_1.Code = staff_masters_1.Code AND coc_1.year = staff_masters_1.year")

dbWriteTable(con, "isora_merge_table", isora_merge, overwrite=TRUE)
dbWriteTable(con, "wgi_merge_table", wgi_merge, overwrite=TRUE)

full_merge <- dbGetQuery(con, "SELECT isora_merge.*,wgi_merge.contofcorrupt,wgi_merge.ruleoflaw,countryclass_1.countryclass,wvs_tax_1.mean_cheattax_17_22
                          FROM isora_merge_table isora_merge
                          LEFT JOIN wgi_merge_table wgi_merge ON isora_merge.Code = wgi_merge.ISO3 AND isora_merge.year = wgi_merge.year
                          LEFT JOIN countryclass_table countryclass_1 ON isora_merge.Code = countryclass_1.ISO3 AND isora_merge.year = countryclass_1.year
                          LEFT JOIN wvs_tax_table wvs_tax_1 ON isora_merge.COUNTRY_NAME = wvs_tax_1.jurisdiction")

# Disconnect from the database
dbDisconnect(con)

# Convert n columns to numeric instead of character
full_merge<-full_merge %>%
  mutate(revgdp = as.numeric(unlist(revgdp)),
         coc = as.numeric(unlist(coc)),
         fte = as.numeric(unlist(fte)),
         staff_tpserv = as.numeric(unlist(staff_tpserv)),
         staff_enforce = as.numeric(unlist(staff_enforce)),
         staff_other = as.numeric(unlist(staff_other)),
         expend_op = as.numeric(unlist(expend_op)),
         expend_cap = as.numeric(unlist(expend_cap)),
         expend_it = as.numeric(unlist(expend_it)),
         staff_m = as.numeric(unlist(staff_m)),
         staff_f = as.numeric(unlist(staff_f)),
         staff_o = as.numeric(unlist(staff_o)),
         staff_masters = as.numeric(unlist(staff_masters)),
         contofcorrupt = as.numeric(unlist(contofcorrupt)),
         ruleoflaw = as.numeric(unlist(ruleoflaw)),
         mean_cheattax_17_22 = as.numeric(unlist(mean_cheattax_17_22)))

#create new data
full_merge<-full_merge %>%
  mutate(revgdp_dec=revgdp/100,
         fte_per = 1/fte,
         staff_tpserv_dec = staff_tpserv/100,
         staff_enforce_dec = staff_enforce/100,
         staff_other_dec = staff_other/100,
         tot_expend = expend_op + expend_cap,
         expend_cap_dec = expend_cap/tot_expend,
         expend_it_dec = expend_it/tot_expend,
         staff_tot = staff_m + staff_f,
         staff_f_dec = staff_f/staff_tot,
         staff_masters_dec = staff_masters/staff_tot,
         ruleoflaw_med = ifelse(ruleoflaw > median(ruleoflaw, na.rm=TRUE),1,0),
         contofcorrupt_med = ifelse(contofcorrupt > median(contofcorrupt, na.rm=TRUE),1,0))

#Datasets for Analysis
allvars <- c("Code","year","revgdp_dec","coc","fte_per","staff_tpserv_dec","staff_enforce_dec","expend_cap_dec","expend_it_dec","staff_f_dec","staff_masters_dec","contofcorrupt","contofcorrupt_med","ruleoflaw","ruleoflaw_med","countryclass","mean_cheattax_17_22")
xvars <- c("revgdp_dec","contofcorrupt","ruleoflaw","countryclass","mean_cheattax_17_22")
taxenforce_full <- na.omit(full_merge) %>% subset(select=allvars)
taxenforce_x <- na.omit(full_merge) %>% subset(select=xvars)

#Save loaded external data sets as RData object
save(rev_ratio,resource_ratios,country_ah,staff_alloc,expend,staff_gender,staff_aq,wgi_rol,wgi_coc,wvs, file="ExternalData.RData")

#Save cleaned data sets as RData object
save(revgdp,coc,fte,countryclass,staff_tpserv,staff_enforce,staff_other,expend_op,expend_cap,expend_it,staff_m,staff_f,staff_o,staff_masters,ruleoflaw,contofcorrupt,wvs_tax, file="SeparatedVars.RData")

library(mclust)
library(datawizard)
library(dplyr)
library(knitr)
library(modelsummary)


############################################################
# Full Sample
############################################################

#summary statistics for full sample
summary(taxenforce_full)
taxenforce_fullw<-winsorize(taxenforce_full,threshold=.02,method='percentile')

#estimate simple model of optimal taxation and enforcement
alt_model_1a <- with(taxenforce_fullw, lm(revgdp_dec ~ ruleoflaw + contofcorrupt + mean_cheattax_17_22 + as.factor(countryclass)))
plot(alt_model_1a, which=1)
altmodel_1a_res<-alt_model_1a$residuals
modelsummary(alt_model_1a,stars=TRUE,output="latex")

#use EM algorithm to understand latent class tax evasion
BIC <- mclustBIC(altmodel_1a_res)
summary(BIC)

#run EM analysis on model residuals
clusters_alt1a <- Mclust(altmodel_1a_res)
clusters_alt1a$G
clusters_alt1a$parameters$pro
clusters_alt1a$parameters$mean
head(clusters_alt1a$z)
postprob_alt1a<-clusters_alt1a$z

#add groupings to original data set
assigned_group_alt1a <- apply(postprob_alt1a, 1, which.max)
taxenforce_grouped_alt1a<-cbind(taxenforce_fullw,assigned_group_alt1a)

#calculate summary statistics for each group
group_stats_alt1a <- aggregate(taxenforce_grouped_alt1a, by = list(assigned_group_alt1a), FUN = summary)
group_stats_alt1a <- t(group_stats_alt1a) %>% heading_clean()

#separate data set into data sets for each group
te_group1_alt1a <- taxenforce_grouped_alt1a[taxenforce_grouped_alt1a$assigned_group_alt1a==1,]
te_group2_alt1a <- taxenforce_grouped_alt1a[taxenforce_grouped_alt1a$assigned_group_alt1a==2,]

#run separate regression models for each group
lm_g1_alt1a <- with(te_group1_alt1a, lm(revgdp_dec ~ coc + expend_cap_dec + expend_it_dec + staff_tpserv_dec + staff_enforce_dec + staff_f_dec + staff_masters_dec + ruleoflaw_med + contofcorrupt_med + mean_cheattax_17_22 + as.factor(countryclass) + as.factor(Code)))
lm_g2_alt1a <- with(te_group2_alt1a, lm(revgdp_dec ~ coc + expend_cap_dec + expend_it_dec + staff_tpserv_dec + staff_enforce_dec + staff_f_dec + staff_masters_dec + ruleoflaw_med + contofcorrupt_med + mean_cheattax_17_22 + as.factor(countryclass) + as.factor(Code)))

#combine regressions into one table
models_alt1a<-list()
models_alt1a[['Group 1']]<-lm_g1_alt1a
models_alt1a[['Group 2']]<-lm_g2_alt1a
modelsummary(models_alt1a,stars=TRUE,output="latex")

