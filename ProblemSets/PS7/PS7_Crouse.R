library(mice)
library(modelsummary)
library(tidyverse)
library(tidyr)

#load csv file
wages<-as.data.frame(read.csv("~/DScourseS23/ProblemSets/PS7/wages.csv"))

#drop missing x values
wages<-wages %>% drop_na(hgc) %>% drop_na(tenure) 

#create summary table
datasummary_skim(wages, output='latex')

#imputation 1: drop all missing values (R does this automatically)
lm_MCAR<-with(wages, lm(logwage~hgc+college+tenure+I(tenure^2)+age+married))
summary(lm_MCAR)

#imputation 2: replace missing values with mean
mlogwage<-mean(wages$logwage, na.rm=TRUE)
wages2<-wages
wages2$logwage<-replace(wages2$logwage,is.na(wages2$logwage),mlogwage)
lm_MI<-with(wages2, lm(logwage~hgc+college+tenure+I(tenure^2)+age+married))
summary(lm_MI)

#imputation 3: replace missing values with fitted values calculated from lm_MCAR
wages3<-wages
wages3$logwage_fitted<-predict(lm_MCAR,newdata=wages3)
wages3$logwage[is.na(wages3$logwage)]<-wages3$logwage_fitted[is.na(wages3$logwage)]
lm_FV<-with(wages3, lm(logwage~hgc+college+tenure+I(tenure^2)+age+married))
summary(lm_FV)

#imputation 4: mice package
wages4<-mice(wages)
lm_mice<-with(wages4, lm(logwage~hgc+college+tenure+I(tenure^2)+age+married))
lm_mice<-mice::pool(lm_mice)

#combine regressions into one table
models<-list()
models[['Complete Cases']]<-lm_MCAR
models[['Mean Imputation']]<-lm_MI
models[['Fitted Value Imputation']]<-lm_FV
models[['Multiple Imputation']]<-lm_mice
modelsummary(models,stars=TRUE,output="latex")

