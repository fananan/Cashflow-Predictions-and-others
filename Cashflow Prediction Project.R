#read the file
if ("package:readxl" %in% search()) { detach("package:readxl", unload=TRUE) }
if ("readxl" %in% rownames(installed.packages())) { remove.packages("readxl") }
install.packages("readxl")
library(readxl)
data<-read_excel("/Users/fanliu/Desktop/final\ dataset\ copy.xlsx") 
attach(data)

#sum up project amount
totalamt<-ddply(data,"projectpath",summarise,sum(projectamt)) 
colnames(totalamt)[2]<-"totalamt"
write.csv(totalamt, file = "totalamt")


#combine the address together
final<-read_excel("/Users/fanliu/Desktop/final\ dataset.xlsx")
final$location<-paste(final$street1, final$street2, final$city, final$state,sep = ",")
write.csv(final, file="finals")

#to figure out why 163 and 173
a163<-read_excel("/Users/fanliu/Desktop/163\ projects\ with\ project\ duration.xlsx") 
a173<-read_excel("/Users/fanliu/Desktop/173\ project\ duration.xlsx")
a<-unique(a163$projectpath)
b<-unique(a173$ProjectPath)
write.csv(a, file="a")

dataset<-read_excel("/Users/fanliu/Desktop/dataset.xlsx")
path<-unique(dataset$projectpath)
write.csv(path,file = "path")

sfdata<-read_excel("/Users/fanliu/Desktop/data\ from\ Salesforce.xlsx")
projectpath<-unique(sfdata$projectpath)
write.csv(projectpath,file = "projectpath")

#deduplicate the projectpath
sfdata$sf<-with(sfdata,paste(sfdata$firmcode,sfdata$projectpath,sfdata$projectname,sep=";")) #only paste can have the option that you can separate variables by comma
View(sfdata)
(uni_sfdata<-unique(sfdata$sf))
write.csv(uni_sfdata,file = "uni_sfdata")

#process match data
match<-read_excel("/Users/fanliu/Desktop/match\ project.xlsx")
deposit<-read_excel("deposit.xlsx")
deposit$projectpath<-substr(deposit$projectpath,1,5)

View(deposit)
#remove the 001 first
deposit<-subset(deposit,projectpath!="0001")
deposit<-deposit[order(deposit$projectpath),]
write.csv(deposit,file = "deposit_final")

list<-list(1,2,3,4,5)
lapply(list,"*",3)



#substrings of a character
deposit$projectpath<-substr(deposit$projectpath, 1,5)
#group deposite and amount by path
write.csv(deposit, file = "updated_deposite")
#delete the redundant rows in invoice
invoice<-read_excel("invoice.xlsx")
invoice<-invoice[order(invoice$projectpath),]
write.csv(invoice,file = "invoice_final")
#change amount =0 to amount =NA
invoice$invoiceamt[invoice$invoiceamt==0]=NA   
invoice<-invoice[complete.cases(invoice),]
invoice$invoicedate<-substr(invoice$invoicedate,1,10)
write.csv(invoice,file = "invoice")


#now use the max date-min to simplify the payment days issue
days<-read_excel("days.xlsx")
days<-days[order(days$path),]
View(days)
class(days$path)
uni_path<-unique(days$path)  


diffdays<-rep(NA,583)

for (i in 1:length(uni_path)){
  diffdays[i]<-as.numeric(max(days$date[days$path==uni_path[i]])-min(days$date[days$path==uni_path[i]]))  
}

paymentdays<-cbind.data.frame(uni_path,diffdays)
write.csv(paymentdays,file="paymentdays")


which(uni_path==14019) #get the position of a number in an vector
as.numeric(max(days$date[days$path==uni_path[248]])-min(days$date[days$path==uni_path[248]])) 
diffdays[248]

#test if there are only invoice dates, no deposite dates and vice versa. because under this situation, even though the resaults are non-zero, they are also wrong
depositpath<-read_excel("depositpath.xlsx")
invoicepath<-read_excel("invoicepath.xlsx")
depositpath<-unique(depositpath)
invoicepath<-unique(invoicepath)
write.csv(depositpath,file = "depositpath")
write.csv(invoicepath,file = "invoicepath")

#latest number of projects check:
cv<-read_excel("/Users/fanliu/Desktop/latest\ clearview\ client\ inquiry.xlsx")
cv_path<-unique(cv$projectpath)
cv_path[order(cv_path)]

#process cvdata to import in analysis dataset draft:
cvdata<-read_excel("cvdata.xlsx")
library(dplyr)
library(tidyverse)
cvdata<-cvdata %>%
  unite(new,projectpath,firmname,street1,street2,city,state,zip,sep=";")
View(cvdata)
cvdata<-unique(cvdata)
write.csv(cvdata,file = "cvdata_update")


data<-read_excel("finallly.xlsx")
View(data)
data<-subset(data,is.na(data$paymentdays)==FALSE)
data$projectpath<-as.character(data$projectpath)
finalpay<-data.frame(tapply(data$paymentdays,data$projectpath,mean))
write.csv(finalpay,file = "finalpay")

#if you do it in median
finalpay_median<-data.frame(tapply(data$paymentdays,data$projectpath,median))
write.csv(finalpay_median,file = "finalpay_median")

#process data for time series model
#table 1
raw<-read_excel("raw\ data.xlsx")
raw<-raw[order(raw$depositdate),]
View(raw)
raw<-raw[-(1:8),]
raw<-raw[-(2153:2156),]
raw$depositdate<-substr(raw$depositdate,1,7)
raw<-data.frame(tapply(raw$amount,raw$depositdate,sum))

write.csv(raw,file = "raw")

raw<-t(raw)
write.csv(raw,file = "table 1")

t3<-read_excel("3.xlsx")
t3<-t(t3)
write.csv(t3,file = "t3")



#ts for R
ts<-read_excel("ts\ for\ R.xlsx")
View(ts)

ts<-ts(ts,frequency=12)
plot.ts(ts)
logts<-log(ts)
plot.ts(logts)

plot(diff(logts,12))
install.packages("tseries")
library(tseries)
adf.test(logts,alternative = "stationary")

install.packages("forecast")
library(forecast)
frequency(logts)
nsdiffs(logts)

logdiff12=diff(logts,lag=12)
logdiff12=diff(logdiff12,lag=12)
plot(logdiff12)
acf(logdiff12)
pacf(logdiff12)


?Box.test
Box.test(logdiff12,lag=1,type="Ljung-Box")
adf.test(logdiff12,alternative = "stationary")


DB<-read_excel("DashBoard.xlsx")

library(readxl)
New<-read_excel("New.xlsx")
New<-tapply(New$paymentdays,New$`Account Name`,median)
New<-data.frame(New)
write.csv(New,file = "Median")

list<-read_excel("list.xlsx")
list<-list[,-2]

list<-split(list,list$`Account Name`)  
nrow(list[[1]])
avg<-rep(NA,38)
for (i in 1:38){
  if(nrow(list[[i]])>1){
    avg[i]<-mean(head(list[[i]]$paymentdays,-1))
  }
  else
  {avg[i]<-NA}
}

x<-c(1,2,3)
head(x,-1) 
tail(x,1)
avg

last<-rep(NA,38)
for (i in 1:38){
  last[i]<-tail(list[[i]]$paymentdays,1)
}
last

avg<-avg[-1]
last<-last[-1]

#error rate
error<-(avg-last)/last
error<-error[-9]
mean(error)
#[1] 1.387734


#try the median
median<-rep(NA,38)
for (i in 1:38){
  if(nrow(list[[i]])>1){
    median[i]<-median(head(list[[i]]$paymentdays,-1))
  }
  else
  {median[i]<-NA}
}

median<-median[-1]


#error rate
error<-(median-last)/last
error<-error[-9]
mean(error)
#[1] 1.02764

plot(last, col="black")
par(new=T) 
plot(avg,col="red")



#fill the matrix using loop
Amount<-read_excel("Amount.xlsx")
Rate<-read_excel("Rate.xlsx")
library(car)
View(Amount)
class(Amount)
Amount$Amount<-recode(Amount$Amount,"NA=0")
View(rate)
length(Rate$`Hit Ratio`)
length(Amount$Amount)

x=matrix(data=NA, nrow=1752,ncol=53)


for (i in 1:1752){
  for (j in 1:53){
    x[i,j]=Amount$Amount[i]*Rate$`Hit Ratio`[j]
  }
}

View(x)
write.csv(x,file = "Amount with hit rate")

#dates manipulation; SUM by month
library(lubridate)
date("2014-03-16"+7)
library(readxl)
close<-read_excel("date.xlsx")
View(close)
close$`Close Date`
close$`Close Date`<-substr(close$`Close Date`,1,10)
table<-matrix(data=NA, nrow = 1752, ncol = 53)
days<-seq(7,7*53,by=7)
?seq
c<-date(date(close$`Close Date`[1])+days[1])
c

for (i in 1:1752){
  for (j in 1:53){
    table[i,j]=as.character(date(close$`Close Date`[i])+days[j])  
  }
}

View(table)
table<-substr(table,1,7)

amt<-read_excel("Amount.xlsx")
View(amt)
amt<-data.frame(amt)
amt<-amt[,-1]
table<-data.frame(table)
colnames(table)
colnames(amt)<-colnames(table)


m<-matrix(c(1,2,3,4,5,6,7,8), nrow = 2, ncol = 4)
as.vector(m)

amt<-as.matrix(amt)
View(amt)
amt<-na.omit(amt)
v_amt<-as.vector(amt)
length(v_amt)

v_table<-as.vector(as.matrix(table))
length(v_table)

a<-cbind(v_amt,v_table)
a<-data.frame(a)
a$v_amt<-as.numeric(a$v_amt)
sum<-tapply(a$v_amt,a$v_table,sum)
sum<-data.frame(sum)
write.csv(sum,file = "sum by month")

head(v_table,500)
#count the number of elements in a vector
table(v_table) #it's impossible that we can get thousand opportunities per month
issued<-read_excel("issued.xlsx")
View(issued)
issued<-data.frame(issued)
issued$Proposal.Issued<-substr(issued$Proposal.Issued,1,7)
t_date<-table(issued$Proposal.Issued) 

class(issued$Stage)
issued$Stage<-as.character(issued$Stage)

list<-split(issued,issued$Proposal.Issued)
hit<-rep(NA,88)

for (i in 1:88){
  hit[i]<-length(which(list[[i]][,2]=="Closed Won"))/(1752-length(which(list[[i]][,2]==c("Closed Won","Closed Lost"))))
}

nrow(list[[4]])
length(which(list[[4]][,2]=="Closed Won"))/nrow(list[[4]])   


#hit rate analysis
class(hit)
uni<-unique(issued$Proposal.Issued)
month<-uni[order(uni)]
class(month)

hit_month<-cbind(month,hit)
write.csv(hit_month,file = "hit_month")

issued2<-read_excel("issued.xlsx")
View(issued2)
issued2<-data.frame(issued2)
issued2$Proposal.Issued<-substr(issued2$Proposal.Issued,1,7)
library(car)
issued2$Amount<-recode(issued2$Amount,"NA=0")
sum_month<-data.frame(tapply(issued2$Amount,issued2$Proposal.Issued,sum))
colnames(sum_month)<-"sum_amount"
length(sum_month$sum_amount)

cash<-sum_month$sum_amount*hit
write.csv(cash,file = "cash")

#use hit rate and number of proposals/month to do the prediction (sampling the amount number from the dataset 1752)
hit_month<-data.frame(hit_month)
avg_hit<-read_excel("Avg_hit.xlsx")
length(avg_hit$Avg_hit)  
t_date<-data.frame(t_date)
t_date$Var1<-substr(t_date$Var1,6,7)
proposal_month<-data.frame(tapply(t_date$Freq,t_date$Var1,mean))
colnames(proposal_month)<-"No.prop"
proposal_month$No.prop<-round(proposal_month$No.prop) #all proposals 
proposal_month<-as.vector(proposal_month) 
View(proposal_month)
length(proposal_month$No.prop) ######12

year_pred<-rep(NA,12)
sample<-data.frame(matrix(data=NA,12,500))

for (i in 1:12){
  for (j in 1:500){
  sample[i,j]<-sum(sample(issued2$Amount,proposal_month$No.prop[i]))
}
  year_pred[i]<-rowMeans(sample)[i]*avg_hit$Avg_hit[i]
}
?sample
year_pred

write.csv(year_pred,file = "month_pred")

c<-data.frame(matrix(c(1,2,3,4,4,6,5,7.8,3,4,6,3,5,4,5,3),4,4))


hit1<-rep(NA,88)
list<-split(issued,issued$Proposal.Issued)
for (i in 1:88){
  if(i==1){
    denom[i]<-1611
    hit1[i]<-length(which(list[[i]][,3]=="Closed Won"))/denom[i]
  }
  else{
    denom[i]<-denom[i-1]-length(which(list[[i-1]][,3]=="Closed Won"))-length(which(list[[i-1]][,3]=="Closed Lost"))
    hit1[i]<-length(which(list[[i]][,3]=="Closed Won"))/denom[i]
  }
}


nrow(list[[8]])
length(which(list[[8]][,3]=="Closed Won")) 
length(which(list[[8]][,3]=="Closed Won")) 

which(list[[6]][,3]=="Closed Won")
list

hit1 #this hit is all months spreaded in each recorded year



issued2<-read_excel("issued.xlsx")
View(issued2)
issued2$`Proposal Issued`<-substr(issued2$`Proposal Issued`,6,7)

list<-split(issued2,issued2$`Proposal Issued`)
hit2<-rep(NA,12) #only twleve months

#the issued dataset before didn't include "amount", so here the stage is on the 3rd column. 
denom<-rep(NA,12)

for (i in 1:12){
  if(i==1){
    denom[i]<-1611
    hit2[i]<-length(which(list[[i]][,3]=="Closed Won"))/denom[i]
  }
  else{
    denom[i]<-denom[i-1]-length(which(list[[i-1]][,3]=="Closed Won"))-length(which(list[[i-1]][,3]=="Closed Lost"))
    hit2[i]<-length(which(list[[i]][,3]=="Closed Won"))/denom[i]
  }
}

hit2  #hit rate by month
for (i in 1:12){
  list[[i]]<-data.frame(list[[i]])
}
length(which(list[[1]]$Stage=="Closed Won"))
51/1611
length(which(list[[2]]$Stage=="Closed Won"))
length(which(list[[1]]$Stage=="Closed Lost"))
58/(1611-79-51)
#this hit rate is correct
#recalculate the number of proposals by month
t_date<-table(issued$Proposal.Issued)
t_date<-data.frame(t_date)
t_date$Var1<-substr(t_date$Var1,6,7)
proposal_month<-data.frame(tapply(t_date$Freq,t_date$Var1,mean))
colnames(proposal_month)<-"No.prop"
proposal_month$No.prop<-round(proposal_month$No.prop) #all proposals 
View(proposal_month)
length(proposal_month$No.prop) ######12

#recode NA =0
library(car)
issued2$Amount<-recode(issued2$Amount,"NA=0")


year_pred<-rep(NA,12)
sample<-data.frame(matrix(data=NA,12,500))

for (i in 1:12){
  for (j in 1:500){
   sample[i,j]<-sum(sample(issued2$Amount,proposal_month$No.prop[i]))
}
  year_pred[i]<-rowMeans(sample)[i]*hit2
}

year_pred 


#if you only calculate each month : closed won / (closed won+closed lost)
hit3<-rep(NA,12)
for (i in 1:12){
    hit3[i]<-length(which(list[[i]][,3]=="Closed Won"))/(length(which(list[[i]][,3]=="Closed Won"))+length(which(list[[i]][,3]=="Closed Lost")))
}

hit3

year_pred<-rep(NA,12)
sample<-data.frame(matrix(data=NA,12,500))

for (i in 1:12){
  for (j in 1:500){
   sample[i,j]<-sum(sample(issued2$Amount,proposal_month$No.prop[i]))
}
  year_pred[i]<-rowMeans(sample)[i]*hit3
}

year_pred  #this is normal each month goal


issued<-read_excel("issued.xlsx")
View(issued)
issued$`Proposal Issued`<-substr(issued$`Proposal Issued`,1,7)
list<-split(issued,issued$`Proposal Issued`)


#start over
denom<-rep(NA,88)
hit2<-rep(NA,88)

for (i in 1:88){
  if(i==1){
    denom[i]<-1611
    hit2[i]<-length(which(list[[i]][,3]=="Closed Won"))/denom[i]
  }
  else{
    denom[i]<-denom[i-1]-length(which(list[[i-1]][,3]=="Closed Won"))-length(which(list[[i-1]][,3]=="Closed Lost"))
    hit2[i]<-length(which(list[[i]][,3]=="Closed Won"))/denom[i]
  }
}

hit2  #hit rate by month
uni<-unique(issued$`Proposal Issued`)
month<-uni[order(uni)]
class(month)

hit2_month<-cbind.data.frame(month,hit2) 
class(hit2_month$hit2)
class(hit2_month$month)
class(issued2$`Proposal Issued`)
hit2_month$month<-as.character(hit2_month$month)
hit2_month$month<-substr(hit2_month$month,6,7)
View(hit2_month)
hit2<-data.frame(tapply(hit2_month$hit2,hit2_month$month,mean))
colnames(hit2)<-"hit rate"

t(hit2)



#recalculate the number of proposals by month
t_date<-table(issued$Proposal.Issued)
t_date<-data.frame(t_date)
t_date$Var1<-substr(t_date$Var1,6,7)
proposal_month<-data.frame(tapply(t_date$Freq,t_date$Var1,mean))
colnames(proposal_month)<-"No.prop"
proposal_month$No.prop<-round(proposal_month$No.prop) #all proposals 
View(proposal_month)
length(proposal_month$No.prop) ######12

#recode NA =0
library(car)
issued$Amount<-recode(issued$Amount,"NA=0")


year_pred1<-rep(NA,12)
sample<-data.frame(matrix(data=NA,12,500))

for (i in 1:12){
  for (j in 1:500){
   sample[i,j]<-sum(sample(issued$Amount,proposal_month$No.prop[i]))
}
  year_pred1[i]<-rowMeans(sample)[i]*hit2$`hit rate`
}


year_pred1


#if you only calculate each month : closed won / (closed won+closed lost)
list<-split(issued,issued$Proposal.Issued)
hit3<-rep(NA,88)
for (i in 1:88){
    hit3[i]<-length(which(list[[i]][,3]=="Closed Won"))/(length(which(list[[i]][,3]=="Closed Won"))+length(which(list[[i]][,3]=="Closed Lost")))
}

hit3

uni<-unique(issued$`Proposal Issued`)
month<-uni[order(uni)]
class(month)

hit3_month<-cbind.data.frame(month,hit3) 
hit3_month$month<-as.character(hit3_month$month)
hit3_month$month<-substr(hit3_month$month,6,7)
View(hit3_month)
hit3<-data.frame(tapply(hit3_month$hit3,hit3_month$month,mean))
colnames(hit3)<-"hit rate"

t(hit3)

year_pred2<-rep(NA,12)
sample<-data.frame(matrix(data=NA,12,500))

for (i in 1:12){
  for (j in 1:500){
   sample[i,j]<-sum(sample(issued$Amount,proposal_month$No.prop[i]))
}
  year_pred2[i]<-rowMeans(sample)[i]*hit3$`hit rate`
}

year_pred2  #this is normal each month goal


#latest paymentdays
library(readxl)
data<-read_excel("paymentdays.xlsx")
data<-data.frame(data)
View(data)
data$path<-as.character(data$path)
tb<-data.frame(tapply(data$paymentdays,data$path,mean)) 

write.csv(tb,file = "final paymentdays")  












