setwd("C:\\Jig12681")

# Reading the data
telecom<-read.csv("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 13 -  Final Case Study Course Wrap up\\telecomfinal.csv")

# Understanding data: Creating data quality report
# Capturing all variable names
VariableName<-names(telecom)
class(VariableName)
VariableName<-data.frame(VariableName)
class(VariableName)

# Data Type extraction
VariableName$DataType<-sapply(telecom,class)

# No. of Recorde
VariableName$No.ofRecordes<-nrow(telecom)

# No of  unique records present in the variable
for(i in 1:length(telecom))
{
  VariableName$UniqueRecords[i]<-length(unique(telecom[,i]))
}

# Data Available
VariableName$DataAvailable<-VariableName$No.ofRecordes-colSums(is.na(telecom))
# Data Available Percent
VariableName$AvailablePercent<-round(VariableName$DataAvailable/nrow(telecom),3)

# No of Missing value in each variable
VariableName$Missing<-colSums(is.na(telecom))

# Percentage of Missing values
VariableName$MissingPercent<-round(VariableName$Missing/nrow(telecom),3)

# Min, Max, Mean, 5th Percentile, 10th Percentile, 
# 25th Percentile, 50th Percentile, 75th Percentile, 90th Percentile, 95th Percentile

for(i in 1:length(telecom))
{
  VariableName$Minimum[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",min(telecom[,i],na.rm=T),0),2)
  VariableName$Maximum[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",max(telecom[,i],na.rm=T),0),2)
  VariableName$Mean[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",mean(telecom[,i],na.rm=T),0),2)
  VariableName$fifthpercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.05,na.rm=T),0),2)
  VariableName$tenthpercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.10,na.rm=T),0),2)
  VariableName$twentyfifthpercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.25,na.rm=T),0),2)
  VariableName$fiftythpercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.50,na.rm=T),0),2)
  VariableName$sevenyfifthpercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.75,na.rm=T),0),2)
  VariableName$ninethpercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.90,na.rm=T),0),2)
  VariableName$ninetyfifthpercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.95,na.rm=T),0),2) 
  }

# Write data quality report into a csv file
write.csv(VariableName,"Data Quality Report.csv",row.names = T)
# Removing the column which have more than 20% of missing value
# apply(telecom, 2, function(x)sum(is.na(x))/length(x))*100
telnew<-telecom[,-c(12,46,47,48,49,52,55,61,62,63,64,66,72)]

# As per data dictionary the retention day's missing value means 
# no retaintion call so instead of removing we will consider NA as 0
telnew$retcall<-ifelse(is.na(telnew$retdays)==TRUE,0,1)

# Removing retday as it capture by 
telnew<-telnew[,-47]

# Removing few observation which have more than 6 same variable missing
index<-which(is.na(telnew$da_Mean))
telnew<-telnew[-index,]

# Again checking for missing value
apply(telnew, 2, function(x)sum(is.na(x))/length(x))*100
# Variable profiling: Continous variable
# mou_Mean (col no. 1)
library(dplyr)
telnew%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat01
dat01$N<-unclass(telnew%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat01$churn_perc<-dat01$n/dat01$N
dat01$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat01$LessThan<-unclass(telnew%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat01$varname<-rep("mou_Mean",nrow(dat01))

# totmrc_Mean (Col No.2) 
telnew%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat02  
dat02$N<-unclass(telnew%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat02$churn_perc<-dat02$n/dat02$N
dat02$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat02$LessThan<-unclass(telnew%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat02$varname<-rep("totmrc_Mean",nrow(dat02))
# rev_Range (Col No. 3)
telnew%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat03
dat03$N<-unclass(telnew%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat03$churn_perc<-dat03$n/dat03$N
dat03$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat03$LessThan<-unclass(telnew%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat03$varname<-rep("rev_Range",nrow(dat03))
# Mou Range (col no. 4)
telnew%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat04
dat04$N<-unclass(telnew%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat04$churn_perc<-dat04$n/dat04$N
dat04$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat04$LessThan<-unclass(telnew%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat04$varname<-rep("mou_Range",nrow(dat04))
# change_mou (col no. 5)
telnew%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat05
dat05$N<-unclass(telnew%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat05$churn_perc<-dat05$n/dat05$N
dat05$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat05$LessThan<-unclass(telnew%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat05$varname<-rep("change_mou",nrow(dat05))
# drop_blk_mean (col no. 6)
telnew%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat06
dat06$N<-unclass(telnew%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat06$churn_perc<-dat06$n/dat06$N
dat06$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dat06$LessThan<-unclass(telnew%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dat06$varname<-rep("drop_blk_mean",nrow(dat06))
# drop_vce_Range (col no. 7)
telnew%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat07
dat07$N<-unclass(telnew%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat07$churn_perc<-dat07$n/dat07$N
dat07$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat07$LessThan<-unclass(telnew%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat07$varname<-rep("drop_vce_Range",nrow(dat07))
# owylis_vce_Range (col no. 8)
telnew%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat08
dat08$N<-unclass(telnew%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat08$churn_perc<-dat08$n/dat08$N
dat08$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat08$LessThan<-unclass(telnew%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat08$varname<-rep("owylis_vce_Range",nrow(dat08))
# mou_opkv_Range (col no. 9)
telnew%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat09
dat09$N<-unclass(telnew%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat09$churn_perc<-dat09$n/dat09$N
dat09$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat09$LessThan<-unclass(telnew%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat09$varname<-rep("mou_opkv_Range",nrow(dat09))
# month (col no. 10)
telnew%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat10
dat10$N<-unclass(telnew%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat10$LessThan<-unclass(telnew%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat10$varname<-rep("months",nrow(dat10))
# totcalls (col no. 11)
telnew%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat11
dat11$N<-unclass(telnew%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
dat11$LessThan<-unclass(telnew%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat11$varname<-rep("totcalls",nrow(dat11))
# eqpdays (col no. 12)
# one missing value observed. To move forward we need to remove it
index<-which(is.na(telnew$eqpdays))
telnew<-telnew[-index,]
telnew%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat12
dat12$N<-unclass(telnew%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat12$LessThan<-unclass(telnew%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
dat12$varname<-rep("epqdays",nrow(dat12))
# custcare_Mean (col no. 13)
telnew%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat13
dat13$varname<-rep("custcare_mean",nrow(dat13))
# Only 7 deciles are available


# callwait_Mean (col no. 14)]: 4 deciles
telnew%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat14
dat14$N<-unclass(telnew%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
dat14$LessThan<-unclass(telnew%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
dat14$varname<-rep("callwait_Mean",nrow(dat14))
# iwylis_vce_Mean (col no. 15): 6 decile
telnew%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat15
dat15$N<-unclass(telnew%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat15$LessThan<-unclass(telnew%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat15$varname<-rep("iwylis_vce_Mean",nrow(dat15))
# callwait_Range (col no. 16) getting less than 4 deciles only therefore omit
telnew%>%mutate(dec=ntile(callwait_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat16
dat16$varname<-rep("callwait_Range",nrow(dat16))
# ccrndmou_Range (col no. 17) getting less than 4 deciles only therefore omit.
telnew%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat17
dat17$varname<-rep("ccrndmou_Range",nrow(dat17))
# adjqty (col no. 18)
telnew%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat18
dat18$N<-unclass(telnew%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat18$LessThan<-unclass(telnew%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat18$varname<-rep("adjqtyn",nrow(dat18))
# ovrrev_Mean (col no. 19) : 4 quantile 
telnew%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat19
dat19$N<-unclass(telnew%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat19$LessThan<-unclass(telnew%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat19$varname<-rep("ovrrev_Mean",nrow(dat19))
# rev_Mean (col no. 20)
telnew%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(telnew%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat20$LessThan<-unclass(telnew%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat20$varname<-rep("rev_Mean",nrow(dat20))
# ovrmou_Mean (col no. 21): 4 quantile
telnew%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat21
dat21$N<-unclass(telnew%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat21$LessThan<-unclass(telnew%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat21$varname<-rep("ovrmou_mean",nrow(dat21))

# comp_vce_Mean (col no. 22)
telnew%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat22
dat22$N<-unclass(telnew%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat22$LessThan<-unclass(telnew%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat22$varname<-rep("comp_vce_Mean",nrow(dat22))

# plcd_vce_Mean (col no. 23)
telnew%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat23
dat23$N<-unclass(telnew%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat23$churn_perc<-dat23$n/dat23$N
dat23$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat23$LessThan<-unclass(telnew%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat23$varname<-rep("plcd_vce_Mean",nrow(dat23))

# avg3mou (col no. 24)
telnew%>%mutate(dec=ntile(avg3mou,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat24
dat24$N<-unclass(telnew%>%mutate(dec=ntile(avg3mou,n=4))%>%count(dec)%>%unname())[[2]]
dat24$churn_perc<-dat24$n/dat24$N
dat24$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(avg3mou,n=4))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat24$LessThan<-unclass(telnew%>%mutate(dec=ntile(avg3mou,n=4))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat24$varname<-rep("avg3mou",nrow(dat24))


# avgmou (col no. 25)
telnew%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat25
dat25$N<-unclass(telnew%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat25$LessThan<-unclass(telnew%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat25$varname<-rep("avgmou",nrow(dat25))

# avg3qty (col no. 26)
telnew%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat26
dat26$N<-unclass(telnew%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat26$LessThan<-unclass(telnew%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat26$varname<-rep("avg3qty",nrow(dat26))

# avgqty (col no. 27)
telnew%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat27
dat27$N<-unclass(telnew%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat27$LessThan<-unclass(telnew%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat27$varname<-rep("avgqty",nrow(dat27))

# avg6mou (col no. 28)
telnew%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(telnew%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat28$LessThan<-unclass(telnew%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat28$varname<-rep("avg6mou",nrow(dat28))

# avg6qty (col no. 29)
telnew%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(telnew%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat29$LessThan<-unclass(telnew%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat29$varname<-rep("avg6qty",nrow(dat29))

# age1 (col no. 38): qutile:6
telnew%>%mutate(dec=ntile(age1,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$N<-unclass(telnew%>%mutate(dec=ntile(age1,n=6))%>%count(dec)%>%unname())[[2]]
dat38$churn_perc<-dat38$n/dat38$N
dat38$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
dat38$LessThan<-unclass(telnew%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
dat38$varname<-rep("age1",nrow(dat38))


# age2 (col no. 39): less than 4 decile available so better use it as factor
telnew%>%mutate(dec=ntile(age2,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat39
dat39$varname<-rep("age2",nrow(dat39))
# models (col no 40): only five decile available will use it as a factor
telnew%>%mutate(dec=ntile(models,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat40

# hnd_price (col no. 41)
telnew%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat41
summary(telnew$hnd_price)
dat41$N<-unclass(telnew%>%mutate(dec=ntile(hnd_price,n=9))%>%count(dec)%>%unname())[[2]]
dat41$churn_perc<-dat41$n/dat41$N
dat41$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(hnd_price,n=9))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
dat41$LessThan<-unclass(telnew%>%mutate(dec=ntile(hnd_price,n=9))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
dat41$varname<-rep("hnd_price",nrow(dat41))

# actvsubs (col no. 42):getting  only four decile. So we will use it as factor variable
telnew%>%mutate(dec=ntile(actvsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat42
dat42$varname<-rep("actvsubs",nrow(dat42))

# uniqsubs (col no. 43): getting only four decile. So we will use it as factor
telnew%>%mutate(dec=ntile(uniqsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat43
dat43$varname<-rep("uniqsubs",nrow(dat43))


# forgntvl (col no. 44): getting only four decile. So we will use it as factor
telnew%>%mutate(dec=ntile(forgntvl,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat44
dat44$varname<-rep("forgntvl",nrow(dat44))

# opk_dat_Mean (col no. 45): getting less than four decile. As it is a numeric variable we will not consider it for further quatile analysis
telnew%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$varname<-rep("opk_dat_Mean",nrow(dat45))

# mtrcycle (col no. 46): getting less than four decile. So we will use it as factor
telnew%>%mutate(dec=ntile(mtrcycle,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat46
dat46$varname<-rep("mtrcycle",nrow(dat46))

# truck (col no. 47): getting only four decile. So we will use it as factor
telnew%>%mutate(dec=ntile(truck,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat47
dat47$varname<-rep("truck",nrow(dat47))

# roam_Mean (col no. 48): getting less than four decile . As it is a numeric variable we will not consider it for further quatile analysis
telnew%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat48
dat48$varname<-rep("roam_mean",nrow(dat48))

# recv_sms_Mean (col no. 49): getting less than four deciles. As it is a numeric variable we will not consider it for further quatile analysis
telnew%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat49
dat49$varname<-rep("recv_sms_mean",nrow(dat49))

#blck_dat_Mean (col no. 50): getting less than four deciles. As it is a numeric variable we will not consider it for further quatile analysis
telnew%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat50
dat50$varname<-rep("blck_dat_mean",nrow(dat50))

# mou_pead_Mean (col no. 51): getting less four decile. As it is a numeric variable we will not consider it for further quatile analysis
telnew%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat51
dat51$varname<-rep("mou_pred_mean",nrow(dat51))

# da_Mean (col no. 55): four decile
telnew%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat55
dat55$N<-unclass(telnew%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat55$churn_perc<-dat55$n/dat55$N
dat55$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat55$LessThan<-unclass(telnew%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat55$varname<-rep("da_mean",nrow(dat55))

# da_Range (Col no.56): four decile
telnew%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat56
dat56$N<-unclass(telnew%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
dat56$churn_perc<-dat56$n/dat56$N
dat56$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat56$LessThan<-unclass(telnew%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat56$varname<-rep("da_Range",nrow(dat56))

# datovr_Mean (col no. 57): getting less than four decile. As it is a numeric variable we will not consider it for further quatile analysis
telnew%>%mutate(dec=ntile(datovr_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat57
dat57$varname<-rep("datovr_Mean",nrow(dat57))

# datovr_Range (col no. 58): getting less than four decile. As it is a numeric variable we will not consider it for further quatile analysis
telnew%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat58
dat58$varname<-rep("datovr_Range",nrow(dat58))

# drop_dat_Mean (col no. 59): getting only four decile. As it is a numeric variable we will not consider it for further quatile analysis
telnew%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat59
dat59$varname<-rep("drop_dat_mean",nrow(dat59))

# drop_vce_Mean (Col no.60)
telnew%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat60
dat60$N<-unclass(telnew%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat60$churn_perc<-dat60$n/dat60$N
dat60$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_dat_Mean)))[[2]]
dat60$LessThan<-unclass(telnew%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_dat_Mean)))[[2]]
dat60$varname<-rep("da_Range",nrow(dat60))

# adjmou (Col no.61)
telnew%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat61
dat61$N<-unclass(telnew%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat61$churn_perc<-dat61$n/dat61$N
dat61$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat61$LessThan<-unclass(telnew%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat61$varname<-rep("adjmou",nrow(dat61))

# totrev (Col no.62)
telnew%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat62
dat62$N<-unclass(telnew%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat62$churn_perc<-dat62$n/dat62$N
dat62$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat62$LessThan<-unclass(telnew%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat62$varname<-rep("totrev",nrow(dat62))


# adjrev (Col no.63)
telnew%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat63
dat63$N<-unclass(telnew%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat63$churn_perc<-dat63$n/dat63$N
dat63$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat63$LessThan<-unclass(telnew%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat63$varname<-rep("adjrev",nrow(dat63))


# avgrev (Col no.64)
telnew%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat64
dat64$N<-unclass(telnew%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat64$churn_perc<-dat64$n/dat64$N
dat64$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat64$LessThan<-unclass(telnew%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat64$varname<-rep("avgrev",nrow(dat64))


# comp_dat_Mean (col no. 66): getting only four decile. As it is a numeric variable we will not consider it for further quatile analysis
telnew%>%mutate(dec=ntile(comp_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat66
dat66$varname<-rep("comp_dat_mean",nrow(dat66))

# plcd_dat_Mean (col no. 67): getting only four decile. As it is a numeric variable we will not consider it for further quatile analysis
telnew%>%mutate(dec=ntile(plcd_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat67
dat67$varname<-rep("plcd_dat_mean",nrow(dat67))

######### Creating Dummy and Derived Variable ########################

# creating Derived variable for completed data and voice call as comp_vce_dat_mean as dat69
telnew$comp_vce_dat_mean<-telnew$comp_vce_Mean+telnew$comp_dat_Mean
# comp_vce_dat_mean (Col no.69)
telnew%>%mutate(dec=ntile(comp_vce_dat_mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat69
dat69$N<-unclass(telnew%>%mutate(dec=ntile(comp_vce_dat_mean,n=10))%>%count(dec)%>%unname())[[2]]
dat69$churn_perc<-dat69$n/dat69$N
dat69$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(comp_vce_dat_mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_dat_mean)))[[2]]
dat69$LessThan<-unclass(telnew%>%mutate(dec=ntile(comp_vce_dat_mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_dat_mean)))[[2]]
dat69$varname<-rep("comp_vce_dat_mean",nrow(dat69))

############## creating Derived variable for placed data and voice call as plcd_vce_dat_mean as dat70####################
telnew$plcd_vce_dat_mean<-telnew$plcd_vce_Mean+telnew$plcd_dat_Mean
# comp_vce_dat_mean (Col no.69)
telnew%>%mutate(dec=ntile(plcd_vce_dat_mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat70
dat70$N<-unclass(telnew%>%mutate(dec=ntile(plcd_vce_dat_mean,n=10))%>%count(dec)%>%unname())[[2]]
dat70$churn_perc<-dat70$n/dat70$N
dat70$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(plcd_vce_dat_mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_dat_mean)))[[2]]
dat70$LessThan<-unclass(telnew%>%mutate(dec=ntile(plcd_vce_dat_mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_dat_mean)))[[2]]
dat70$varname<-rep("comp_vce_dat_mean",nrow(dat70))

############# Creating  new variable for quality,billing,adjustment,overage and churn_family ####################
telnew$drop_block<-telnew$drop_blk_Mean/telnew$totcalls
# drop_block (Col no.71)
# Remove na values
summary(telnew)
index1<-which(is.na(telnew$drop_block))
telnew<-telnew[-index1,]
telnew%>%mutate(dec=ntile(drop_block,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat71
dat71$N<-unclass(telnew%>%mutate(dec=ntile(drop_block,n=10))%>%count(dec)%>%unname())[[2]]
dat71$churn_perc<-dat71$n/dat71$N
dat71$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(drop_block,n=10))%>%group_by(dec)%>%summarise(min(drop_block)))[[2]]
dat71$LessThan<-unclass(telnew%>%mutate(dec=ntile(drop_block,n=10))%>%group_by(dec)%>%summarise(max(drop_block)))[[2]]
dat71$varname<-rep("drop_block",nrow(dat71))

# overage (Col No. 72)
telnew$overage<-telnew$ovrrev_Mean/telnew$rev_Mean
# Remove na values
summary(telnew)
index2<-which(is.na(telnew$overage))
telnew<-telnew[-index2,]
telnew%>%mutate(dec=ntile(overage,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat72
dat72$N<-unclass(telnew%>%mutate(dec=ntile(overage,n=4))%>%count(dec)%>%unname())[[2]]
dat72$churn_perc<-dat72$n/dat72$N
dat72$GreaterThan<-unclass(telnew%>%mutate(dec=ntile(overage,n=4))%>%group_by(dec)%>%summarise(min(overage)))[[2]]
dat72$LessThan<-unclass(telnew%>%mutate(dec=ntile(overage,n=4))%>%group_by(dec)%>%summarise(max(overage)))[[2]]
dat72$varname<-rep("overage",nrow(dat72))




# Putting all dat object into a single object to export in a csv file
datfn<-(rbind(dat01,dat02,dat03,dat04,dat05,dat06,dat07,dat08,dat09,dat10,dat11,dat12,dat13,dat14,
            dat15,dat16,dat17,dat18,dat19,dat20,dat21,dat22,dat23,dat24,dat25,dat26,dat27,dat28,dat29,
            dat38,dat39,dat40,dat41,dat42,dat43,dat44,dat45,dat46,dat47,dat48,dat49,dat50,dat51,dat55,
            dat56,dat57,dat58,dat59,dat60,dat61,dat62,dat63,dat64,dat66,dat67,dat69,dat70,dat71,dat72))

# Exporting datfn deciled observations in to csv
write.csv(datfn,"continuous Variables.csv",row.names = F)

############################ Catagorical Variables ##############################

# crclscod (col No. 30):few observations have less than 5% churn rate. So we will omit this variable.
telnew%>%count(churn,levels=crclscod)%>%filter(churn==1)->dat30
dat30$N<-unclass(telnew%>%filter(crclscod%in%dat30$levels)%>%count(crclscod))[[2]]
dat30$churn_perc<-dat30$n/dat30$N
dat30$varname<-rep("crclscod",nrow(dat30))

# asl_flag (col No. 31)
telnew%>%count(churn,levels=asl_flag)%>%filter(churn==1)->dat31
dat31$N<-unclass(telnew%>%filter(asl_flag%in%dat31$levels)%>%count(asl_flag))[[2]]
dat31$churn_perc<-dat31$n/dat31$N
dat31$varname<-rep("asl_flag",nrow(dat31))

# prizm_social_one (col No. 32)
telnew%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->dat32
dat32$N<-unclass(telnew%>%filter(prizm_social_one%in%dat32$levels)%>%count(prizm_social_one))[[2]]
dat32$churn_perc<-dat32$n/dat32$N
dat32$varname<-rep("prizm_social_one",nrow(dat32))

# area (col No. 33)
telnew%>%count(churn,levels=area)%>%filter(churn==1)->dat33
dat33$N<-unclass(telnew%>%filter(area%in%dat33$levels)%>%count(area))[[2]]
dat33$churn_perc<-dat33$n/dat33$N
dat33$varname<-rep("area",nrow(dat33))

# refurb_new (col No. 34)
telnew%>%count(churn,levels=refurb_new)%>%filter(churn==1)->dat34
dat34$N<-unclass(telnew%>%filter(refurb_new%in%dat34$levels)%>%count(refurb_new))[[2]]
dat34$churn_perc<-dat34$n/dat34$N
dat34$varname<-rep("refurb_new",nrow(dat34))

# hnd_webcap (col No. 35)
telnew%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->dat35
dat35$N<-unclass(telnew%>%filter(hnd_webcap%in%dat35$levels)%>%count(hnd_webcap))[[2]]
dat35$churn_perc<-dat35$n/dat35$N
dat35$varname<-rep("hnd_webcap",nrow(dat35))

# marital (col No. 36)
telnew%>%count(churn,levels=marital)%>%filter(churn==1)->dat36
dat36$N<-unclass(telnew%>%filter(marital%in%dat36$levels)%>%count(marital))[[2]]
dat36$churn_perc<-dat36$n/dat36$N
dat36$varname<-rep("marital",nrow(dat36))

# ethnic (col No. 37)
telnew%>%count(churn,levels=ethnic)%>%filter(churn==1)->dat37
dat37$N<-unclass(telnew%>%filter(ethnic%in%dat37$levels)%>%count(ethnic))[[2]]
dat37$churn_perc<-dat37$n/dat37$N
dat37$varname<-rep("ethnic",nrow(dat37))

# car_buy (col No. 53)
telnew%>%count(churn,levels=car_buy)%>%filter(churn==1)->dat53
dat53$N<-unclass(telnew%>%filter(car_buy%in%dat53$levels)%>%count(car_buy))[[2]]
dat53$churn_perc<-dat53$n/dat53$N
dat53$varname<-rep("car_buy",nrow(dat53))

# csa (col No. 54): Few levels has less than 5% therefore we will omit it.
telnew%>%count(churn,levels=csa)%>%filter(churn==1)->dat54
dat54$N<-unclass(telnew%>%filter(csa%in%dat54$levels)%>%count(csa))[[2]]
dat54$churn_perc<-dat54$n/dat54$N
dat54$varname<-rep("csa",nrow(dat54))

# retcall (col No. 68)
telnew%>%count(churn,levels=retcall)%>%filter(churn==1)->dat68
dat68$N<-unclass(telnew%>%filter(retcall%in%dat68$levels)%>%count(retcall))[[2]]
dat68$churn_perc<-dat68$n/dat68$N
dat68$varname<-rep("retcall",nrow(dat68))

####### age2,models,actvsubs,uniqsubs,forgntvl, mtrcycle,truck ############
# age2 (col No. 39)
telnew%>%count(churn,levels=age2)%>%filter(churn==1)->dat39n
dat39n$N<-unclass(telnew%>%filter(age2%in%dat39n$levels)%>%count(age2))[[2]]
dat39n$churn_perc<-dat39n$n/dat39n$N
dat39n$varname<-rep("age2",nrow(dat39n))

# model (col No. 40)
telnew%>%count(churn,levels=models)%>%filter(churn==1)->dat40n
dat40n$N<-unclass(telnew%>%filter(models%in%dat40n$levels)%>%count(models))[[2]]
dat40n$churn_perc<-dat40n$n/dat40n$N
dat40n$varname<-rep("models",nrow(dat40n))

# actvsubs (col No. 42)
telnew%>%count(churn,levels=actvsubs)%>%filter(churn==1)->dat42n
dat42n$N<-unclass(telnew%>%filter(actvsubs%in%dat42n$levels)%>%count(actvsubs))[[2]]
dat42n$churn_perc<-dat42n$n/dat42n$N
dat42n$varname<-rep("age2",nrow(dat42n))

# uniqsubs (col No. 43)
telnew%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->dat43n
dat43n$N<-unclass(telnew%>%filter(uniqsubs%in%dat43n$levels)%>%count(uniqsubs))[[2]]
dat43n$churn_perc<-dat43n$n/dat43n$N
dat43n$varname<-rep("uniqsubs",nrow(dat43n))

# forgntvl (col No. 44)
telnew%>%count(churn,levels=forgntvl)%>%filter(churn==1)->dat44n
dat44n$N<-unclass(telnew%>%filter(forgntvl%in%dat44n$levels)%>%count(forgntvl))[[2]]
dat44n$churn_perc<-dat44n$n/dat44n$N
dat44n$varname<-rep("forgntvl",nrow(dat44n))

# mtrcycle (col No. 46)
telnew%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->dat46n
dat46n$N<-unclass(telnew%>%filter(mtrcycle%in%dat46n$levels)%>%count(mtrcycle))[[2]]
dat46n$churn_perc<-dat46n$n/dat46n$N
dat46n$varname<-rep("mtrcycle",nrow(dat46n))

# truck (col No. 47)
telnew%>%count(churn,levels=truck)%>%filter(churn==1)->dat47n
dat47n$N<-unclass(telnew%>%filter(truck%in%dat47n$levels)%>%count(truck))[[2]]
dat47n$churn_perc<-dat47n$n/dat47n$N
dat47n$varname<-rep("truck",nrow(dat47n))



# Creating a single object by adding all the laveled wise catagorical and factor variables
datcatfn<-(rbind(dat30,dat31,dat32,dat33,dat34,dat35,dat36,dat37,dat53,dat54,dat68,
                 dat39n,dat40n,dat42n,dat43n,dat44n,dat46n,dat47n)) # Error due to numeric/character levels
# Creatind two separate objects for catagorical and numeric character levels
datcatfn1<-rbind(dat30,dat31,dat32,dat33,dat34,dat35,dat36,dat37,dat53,dat54)
# Creatind two separate objects for catagorical and numeric numeric levels
datcatfn2<-rbind(dat68,dat39n,dat40n,dat42n,dat43n,dat44n,dat46n,dat47n)

# Export laveled variables in to csv file
write.csv(datcatfn1,"catagorical variables cat1.csv",row.names = F)
write.csv(datcatfn2,"catagorical variables cat2.csv",row.names = F)

# Removing continuous variables which will come insignificant as these variable could not be deciled
# Removing all variable which are added under derived variable and customer_ID
telnew<-telnew[,-c(13,14,17,22,23,45,49,51,57,58,59,66,67)]

# Removing the catagorical variable which have less than 5% of churn rate
telnew<-telnew[,-c(25,46)]

# Outlier Treatment for continuous variables
list<-names(telnew)
list
# Removing categorical and factor variables
list<-list[-c(25:31,33,34,36:40,43,44,52,53)]
list
# Box plot
par("mar")
par(mar=c(1,1,1,1))

par(mfcol=c(4,10))
for(i in 1:length(list))
{
  boxplot(telnew[,list[i]],main=list[i])
}

# Removing the outlier
for(i in 1:length(list))
{
  x<-boxplot(telnew[,list[i]],main=list[i])
  out<-x$out
  index<-which(telnew[,list[i]]%in% x$out)
  telnew[index,list[i]]<-mean(telnew[,list[i]],na.rm = T)
  rm(x)
  rm(out)
}

# Final checking 
for(i in 1:length(list))
{
  boxplot(telnew[,list[i]],main=list[i])
}

dev.off()

# Removing the missing existing missing values before building the model
# checking for missing values
colSums(is.na(telnew))
# chage_mou: 233 missing
ind1<-which(is.na(telnew$change_mou))
telnew<-telnew[-ind1,]

# avg6mou and avg6qty have 2037 observation missing. Therefore we will impute insted of delete

# area: 18 missing
ind2<-which(is.na(telnew$area))
telnew<-telnew[-ind2,]

# hnd_webcap have 6019 observation missing. Therefore we will treat it as missing level

# marital, ethnic,age1,age2 all have 1148 no of missing for the same observation so deleting one will serve the rest
ind3<-which(is.na(telnew$marital))
telnew<-telnew[-ind3,]
# prizm_social_one have 4743 observation missing. Therefore we will treat it as missing level
# hnd_price
ind4<-which(is.na(telnew$hnd_price))
telnew<-telnew[-ind4,]


# Missing value imputation
# avg6mou :2037 missing
telnew$avg6mou[is.na(telnew$avg6mou)]<-median(telnew$avg6mou,na.rm = T)

# avg6qty :2037 missing
telnew$avg6qty[is.na(telnew$avg6qty)]<-median(telnew$avg6qty,na.rm = T)

# Treating NA for hnd_webcap and prizm_social_one as missing level
# hnd_webcap
telnew$hnd_webcap_new<-ifelse(is.na(telnew$hnd_webcap),"Missing",as.character(telnew$hnd_webcap))
telnew$hnd_webcap_new<-as.factor(telnew$hnd_webcap_new)
str(telnew$hnd_webcap_new)
summary(telnew$hnd_webcap_new)
# prizm_social_one
telnew$prizm_social_one_new<-ifelse(is.na(telnew$prizm_social_one),"Missing",as.character(telnew$prizm_social_one))
telnew$prizm_social_one_new<-as.factor(telnew$prizm_social_one_new)
str(telnew$prizm_social_one_new)
summary(telnew$prizm_social_one_new)

# Removing hnd_webcap and prizm_social_one original variable
telnew<-telnew[-c(26,29)]

summary(telnew)

# Exporting the prepared data set into csv 
write.csv(telnew,"telnew1.csv",row.names = F)


# Importing the prepared dataset
telnew1<-read.csv("C:\\Jig12681\\Project\\telnew1.csv")
summary(telnew1)
str(telnew1)

# Creating dummy variables and converting few variable to factor
# age1,age2,models,actvsubs,uniqsubs,forgntvl,mtrcycle,truck,churn

# age1
telnew1$age1_new<-ifelse(telnew1$age1==0,"Unknown",ifelse(telnew1$age1<=35,"Young",ifelse(telnew1$age1>35 & telnew1$age1<=50,"Middle Age","Old")))
telnew1$age1_new<-as.factor(telnew1$age1_new)
summary(telnew1$age1_new)
# Removing age1
telnew1<-telnew1[,-30]

# age2
telnew1$age2_new<-ifelse(telnew1$age2==0,"Unknown",ifelse(telnew1$age2<=35,"Young",ifelse(telnew1$age2>35 & telnew1$age2<=50,"Middle Age","Old")))
telnew1$age2_new<-as.factor(telnew1$age2_new)
summary(telnew1$age2_new)
# Removing age2
telnew1<-telnew1[,-30]



#
# hnd_price 
summary(telnew1$hnd_price)
telnew1$hnd_price_new<-ifelse(telnew1$hnd_price<=59.99,"Low price Hnd",ifelse(telnew1$hnd_price>59.99 & telnew1$hnd_price<=103.90, "Medium price Hnd",ifelse(telnew1$hnd_price>103.90 & telnew1$hnd_price<=150.00,"High price hnd","Very high price hnd")))
telnew1$hnd_price_new<-as.factor(telnew1$hnd_price_new)
summary(telnew1$hnd_price_new)
# Removing hnd_price
telnew1<-telnew1[-31]
# models
telnew1$models<-as.factor(telnew1$models)
summary(telnew1$models)

# actvsubs
telnew1$actvsubs<-as.factor(telnew1$actvsubs)
summary(telnew1$actvsubs)

# uniqsubs
telnew1$uniqsubs<-as.factor(telnew1$uniqsubs)
summary(telnew1$uniqsubs)


# forgntvl
telnew1$forgntvl<-as.factor(telnew1$forgntvl)
summary(telnew1$forgntvl)

# mtrcycle
telnew1$mtrcycle<-as.factor(telnew1$mtrcycle)
summary(telnew1$mtrcycle)


# actvsubs
telnew1$truck<-as.factor(telnew1$truck)
summary(telnew1$truck)





################################ Model Building ##################################################
set.seed(1000)

# Split into train and test dataset
index<-sample(nrow(telnew1),0.70*nrow(telnew1),replace = F)
train<-telnew1[index,]
test<-telnew1[-index,]

# Checking for Churn Rate in both samples
table(train$churn)/length(train$churn)
table(test$churn)/length(test$churn)

# Logistic Regression model
model1<-glm(churn~.,data = train[,-47],family = "binomial")
summary(model1)

step(model1, direction = "both") # Step wise operation can't performed well as System hangs and it takes long time to run. 

#### Creating Dummay variables significent factors #############

train$asl_flag_1<-ifelse(train$asl_flag=="Y",1,0)
test$asl_flag_1<-ifelse(test$asl_flag=="Y",1,0)

train$area_California_North<-ifelse(train$area=="CALIFORNIA NORTH AREA",1,0)
test$area_California_North<-ifelse(test$area=="CALIFORNIA NORTH AREA",1,0)

train$area_NEW_England<-ifelse(train$area=="NEW ENGLAND AREA",1,0)
test$area_NEW_ENGLAND<-ifelse(test$area=="NEW ENGLAND AREA",1,0)

train$area_Rocky_Mountain<-ifelse(train$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
test$area_Rocky_Mountain<-ifelse(test$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)

train$area_South_Florida<-ifelse(train$area=="SOUTH FLORIDA AREA",1,0)
test$area_South_Florida<-ifelse(test$area=="SOUTH FLORIDA AREA",1,0)

train$area_Tennessee<-ifelse(train$area=="TENNESSEE AREA",1,0)
test$area_Tennessee<-ifelse(test$area=="TENNESSEE AREA",1,0)

train$refub<-ifelse(train$refurb_new=="R",1,0)
test$refub<-ifelse(test$refurb_new=="R",1,0)


train$marital_S<-ifelse(train$marital=="S",1,0)
test$marital_S<-ifelse(test$marital=="S",1,0)

train$ethnic_C<-ifelse(train$ethnic=="C",1,0)
test$ethnic_C<-ifelse(test$ethnic=="C",1,0)

train$ethnic_G<-ifelse(train$ethnic=="G",1,0)
test$ethnic_G<-ifelse(test$ethnic=="G",1,0)


train$ethnic_H<-ifelse(train$ethnic=="H",1,0)
test$ethnic_H<-ifelse(test$ethnic=="H",1,0)


train$ethnic_N<-ifelse(train$ethnic=="N",1,0)
test$ethnic_N<-ifelse(test$ethnic=="N",1,0)


train$ethnic_P<-ifelse(train$ethnic=="P",1,0)
test$ethnic_P<-ifelse(test$ethnic=="P",1,0)

train$ethnic_S<-ifelse(train$ethnic=="S",1,0)
test$ethnic_S<-ifelse(test$ethnic=="S",1,0)


train$ethnic_U<-ifelse(train$ethnic=="U",1,0)
test$ethnic_U<-ifelse(test$ethnic=="U",1,0)


train$ethnic_Z<-ifelse(train$ethnic=="Z",1,0)
test$ethnic_Z<-ifelse(test$ethnic=="Z",1,0)

train$uniqsubs_2<-ifelse(train$uniqsubs=="2",1,0)
test$uniqsubs_2<-ifelse(test$uniqsubs=="2",1,0)

train$uniqsubs_3<-ifelse(train$uniqsubs=="3",1,0)
test$uniqsubs_3<-ifelse(test$uniqsubs=="3",1,0)

train$uniqsubs_4<-ifelse(train$uniqsubs=="4",1,0)
test$uniqsubs_4<-ifelse(test$uniqsubs=="4",1,0)

train$uniqsubs_5<-ifelse(train$uniqsubs=="5",1,0)
test$uniqsubs_5<-ifelse(test$uniqsubs=="5",1,0)

train$uniqsubs_7<-ifelse(train$uniqsubs=="7",1,0)
test$uniqsubs_7<-ifelse(test$uniqsubs=="7",1,0)

train$uniqsubs_8<-ifelse(train$uniqsubs=="8",1,0)
test$uniqsubs_8<-ifelse(test$uniqsubs=="8",1,0)

train$hnd_webcap_new_WC<-ifelse(train$hnd_webcap_new=="WC",1,0)
test$hnd_webcap_new_WC<-ifelse(test$hnd_webcap_new=="WC",1,0)

train$hnd_webcap_new_WCMB<-ifelse(train$hnd_webcap_new=="WCMB",1,0)
test$hnd_webcap_new_WCMB<-ifelse(test$hnd_webcap_new=="WCMB",1,0)

train$prizm_social_one_new_R<-ifelse(train$prizm_social_one_new=="R",1,0)
test$prizm_social_one_new_R<-ifelse(test$prizm_social_one_new=="R",1,0)

train$prizm_social_one_new_T<-ifelse(train$prizm_social_one_new=="T",1,0)
test$prizm_social_one_new_T<-ifelse(test$prizm_social_one_new=="T",1,0)

train$age1_new_unknown<-ifelse(train$age1_new=="Unknown",1,0)
test$age1_new_unknown<-ifelse(test$age1_new=="Unknown",1,0)

train$age1_new_old<-ifelse(train$age1_new=="Old",1,0)
test$age1_new_old<-ifelse(test$age1_new=="Old",1,0)

train$age1_new_young<-ifelse(train$age1_new=="Young",1,0)
test$age1_new_young<-ifelse(test$age1_new=="Young",1,0)

train$age2_new_old<-ifelse(train$age2_new=="Old",1,0)
test$age2_new_old<-ifelse(test$age2_new=="Old",1,0)

train$hnd_price_vhph<-ifelse(train$hnd_price_new=="Very high price hnd",1,0)
test$hnd_price_vhph<-ifelse(test$hnd_price_new=="Very high price hnd",1,0)

train$hnd_price_lph<-ifelse(train$hnd_price_new=="Low price Hnd",1,0)
test$hnd_price_lph<-ifelse(test$hnd_price_new=="Low price Hnd",1,0)
# Rerunning the model with Significant Variable

model2<- (glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+drop_blk_Mean+drop_vce_Range
              +drop_vce_Range+mou_opkv_Range+months+eqpdays+iwylis_vce_Mean+rev_Mean+ovrrev_Mean
              +avgmou+avg3qty+avgqty+asl_flag_1+area_California_North
              +area_Rocky_Mountain+area_South_Florida+refub+marital_S+ethnic_C
              +ethnic_G+ethnic_H+ethnic_N+ethnic_P+ethnic_S+ethnic_U+ethnic_Z
              +uniqsubs_2+uniqsubs_3+uniqsubs_4+uniqsubs_5+uniqsubs_7+roam_Mean+totrev+comp_vce_dat_mean+retcall+overage
              +hnd_webcap_new_WC+hnd_webcap_new_WCMB+prizm_social_one_new_R+prizm_social_one_new_T
              +age1_new_unknown+age1_new_young+age2_new_old+hnd_price_lph+hnd_price_vhph,data = train,family="binomial"))

summary(model2)

# Again rerunning the model with significant variable 
model3<- (glm(churn~mou_Mean+totmrc_Mean+mou_Range+change_mou+drop_blk_Mean+drop_vce_Range
               +drop_vce_Range+mou_opkv_Range+months+eqpdays+iwylis_vce_Mean+
               +avgmou+avg3qty+avgqty+asl_flag_1+area_California_North
               +area_Rocky_Mountain+area_South_Florida+refub+marital_S+ethnic_C
               +ethnic_G+ethnic_H+ethnic_N+ethnic_S+ethnic_U+ethnic_Z
               +uniqsubs_2+uniqsubs_3+uniqsubs_4+uniqsubs_7+totrev+comp_vce_dat_mean+retcall+overage
               +hnd_webcap_new_WC+hnd_webcap_new_WCMB+prizm_social_one_new_R+prizm_social_one_new_T
               +age1_new_unknown+age1_new_young+age2_new_old+hnd_price_lph+hnd_price_vhph,data = train,family="binomial"))

summary(model3)



# All variables are Significant. Now check for multicolineariry 
library(car)
vif(model3)

# There is no multicolinearity among the variables. So we can finalised the model




# Model Validation
# Predicted values
pred<-predict(model3,type = "response",newdata = test)
head(pred)

# Chosing Cut-off as per churn rate in data set
table(telnew1$churn)/length(telnew1$churn)

pred1<-ifelse(pred>=0.2388286,1,0)
table(pred1)
# Kappa MAtrix
library(irr)
kappa2(data.frame(test$churn,pred1))


# Confusion Matrix
library(caret)
confusionMatrix(pred1,test$churn,positive = "1") 
table(test$churn)

# ROCR Curve
library(ROCR)
pred2<-prediction(pred1,test$churn)
pref<-performance(pred2,"tpr","fpr")
plot(pref,col="blue")
abline(0,1,lty=8,col="red")

# Area under the curve
auc<-performance(pred2,"auc")
auc<-unlist(slot(auc,"y.values"))
auc


# Gain Chart
library(gains)
gains(test$churn,predict(model3,type="response",newdata = test),groups = 10)
# top 40% of the probablities containd 52.6% of customer who likely to churn

test$prob<-predict(model3,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
# Top 30% of the probablity scores are lie between 

#********************************************************************************#
################################## Question 1 ######################################
#********************************************************************************#
# What are the top five factor driving likelihood of churn at Mobicom?
options(scipen = 999)
head(sort(model3$coefficients,decreasing = T),50)
# The five main factors are
# 1. retcall-Retention Call--- Beta Coefficient( (0.72)
# 2. Uniquesubs_7 unique customer in the household---Beta Coefficient((0.6972829)
# 3. Overage--- Non optimal plan customers--------Beta Coefficient((0.65355451)
# 4. asl_flag_1--- Customer who have account spending limit----Beta Coefficient( (0.40)
# 5. Uniquesubs_4 unique customer in the household---Beta Coefficient((0.31)
########################### Factor 1 ######################################
# retcall: unit increse in days of retention call leads churn to increase 0.72 units
# Retation call is very impotent to reduce churn.

########################## Factor 2 #######################################
# uniqsubs_7: if no of unique subscriber increase in the house hold to level 7 then the 
# pprobability of churn also increase by 0.69 unit So spatial focus should be given
# Family with 7 unique subscriber. Family bundle pack may reduce churn
######################## Factor 3 #########################################
# Overage: ovrrev_Mean/totrev signifies that if overage cost increse by 1 unit, there is 0.65 unit increase in churn.
# Overage is the result for non optimal plan. Spatial care should be given to bring the non optimal plan customer under optimal rate plan
######################## Factor 4 #########################################
# asl_flag_1:customer with account spending limit are less likely to churn (churn id reduced by 0.40 unit)
# The customer who don't have any account spending limit shoud bring under the account spending limit
########################## Factor 5 #######################################
# uniqsubs_4: if no of unique subscriber increse in the house hold to level 4 then the 
# probability of churn also increase by 0.31 unit So spatial focus should be given
# Family with 4 unique subscriber. Family bundle pack/ spatial offer may reduce churn




##**********************************************************************##
############################ Question 2###################################
##**********************************************************************##
#2. Validation of survey findings.
# a) Whether "cost and billing" and "network and service quality" are important factors influencing churn behaviour.  
# "cost and billing"  can be explain by the variable totmrc_mean, oveage, totrev 
##### totmrc_mean #####
# Monthly  the base cost of the calling plan regardless of actual minutes used
# A unit increse in totmrc_Mean causing 0.005 unit decrese in churn

# Unit increase in overage leads to 0.64 unit increase in churn

# Unit increase in total revenue causes 0.0002 unit increase in churn

# Other than overage the "cost and billing" has no impact on churn.
# Overage billing can be decrease by offering optimal plan to the customer

# b) Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn? 
# following variable can explain data usage connectivity
#  drop_dat_mean - Mean no of dropped/ failed data calls.
#  datovr_Mean   - Mean data overage Revenue.
#  datover_Range - Mean overage  Revenue.
#  blck_dat_Mean - Mean no of blocked/failed data calls
#  plcd_dat_Mean - Mean number of attempted data calls placed
#  Comp_dat_Mean - Mean no of completed data calls
#  opk_dat_Mean  - Mean no. of off-peak data calls

# From Quality Report we can conclude that data call placed by only 9 to 15% of the customes.
# Only few customers are using internet so we dont have enough observation to conclude that data connectivity leading to churn.
# Data connectivity are not showing any impact on churn.


#**************************************************************************#
######################### Question 3 #######################################
#**************************************************************************#
# Would you recommend rate plan migration as a proactive retention strategy?

# Overage: ovrrev_Mean/totrev signifies that if overage cost increase by 1 unit, there is 0.65 unit increase in churn.
# Overage is the result for non optimal plan.
# Rate plan migration should be a proactive retention strategy.

#**************************************************************************#
###################### Question 4 ##########################################
#**************************************************************************#

# What would be your recommendation on how to use this churn model for prioritisation of customers for a
# proactive retention campaigns in the future?

# From the gain chart it is clear that top 20% of the probabilities lie between 0.3513250 and .6971534

# So we can apply these two value as cut off to predict  churn
pred2<-predict(model3,type = "response", newdata = test)
pred2<-ifelse(pred2>=0.3513250,1,0)
Targeted_seg<-test[test$prob> 0.3523250 & test$prob<=0.6971524 & test$churn=="1","Customer_ID"]
Targeted_seg<-data.frame(Targeted_seg)
nrow(Targeted_seg)
write.csv(Targeted_seg,"C:\\Jig12681\\Project\\Targeted_seg.csv",row.names=F)
# Recommendation  
# 1. Family Bundle Plan.
# 2. Optimal rate plan migration
# 3. Retention Call to customer

# From from the data set Targeted_seg.csv we can extract the customer_ids who are likely to churn
# So that we can have special focus on this segment only.

#*****************************************************************************#
#############################Question 5#######################################
#****************************************************************************#
# What would be the target segments for proactive retention campaigns? Falling ARPU forecast is also a
# a concern and therefore, Mobicom would like to save their high revenue customers besides managing
# churn. Given a budget constraint of a contact list of 20% of the subscriber pool, which subscribers should
# prioritized if "revenue saves" is also a priority besides controlling churn. In other words, controlling churn
# is the primary objective and revenue saves is the secondary objective

pred3<-predict(model3,type = "response", newdata = test)
# Extracting the test probablity
test$prob<-predict(model3,type = "response", newdata = test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
pred4<-ifelse(pred3<=0.2308856,"Low_Churn_Prob",ifelse(pred3>0.2308856 & pred3<=0.2750276,"Medium_Churn_Prob","High_Churn_Prob"))
table(pred4,test$churn)

# Creating Revenue levels
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
test$rev_lvl<-ifelse(test$totrev<=799.630,"low_rev",ifelse(test$totrev>799.630 & test$totrev<=1034.680,"medium_rev","high_rev"))
table(pred4,test$rev_lvl)

# Creating Churn Probablity level
test$prob_lvl<-ifelse(pred3<=0.2308856,"Low_Churn_Prob",ifelse(pred3>0.2308856 & pred3<=0.2750276,"Medium_Churn_Prob","High_Churn_Prob"))

# Extracting the customer segment with High Churn Probability with High Revenue
target_seg<-test[test$prob_lvl=="High_Churn_Prob" & test$rev_lvl=="high_rev","Customer_ID"]
target_seg<-data.frame(target_seg)
# Extracting the Customer ID in Targeted  Segment 
write.csv(target_seg,"C:\\Jig12681\\Project\\High_rev_tgt_seg.csv",row.names = F)
