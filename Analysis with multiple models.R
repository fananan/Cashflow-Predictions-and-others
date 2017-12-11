library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
data<-read_excel("/Users/fanliu/Desktop/dataset\ draft\ without\ extreme\ Y.xlsx")
attach(data)
View(data)

#to see Paymentdays distribution divided by Leadsource
ggplot(data=data)+geom_histogram(mapping = aes(x=PaymentDays),binwidth = 50)+facet_wrap(~LeadSource)+coord_cartesian(xlim = c(500,1000))
#divided by type
ggplot(data=data)+geom_histogram(mapping = aes(x=Amount),binwidth = 50)+facet_wrap(~Type)+coord_cartesian(xlim = c(0,500))
#divided by fiscal 
ggplot(data=data)+geom_histogram(mapping = aes(x=PaymentDays),binwidth = 50)+facet_wrap(~FiscalPeriod)+coord_cartesian(xlim = c(0,500))

#y and amount
ggplot(data=data) +geom_point(mapping = aes(x=Amount, y=PaymentDays), alpha=0.24)+coord_cartesian(xlim = c(0,20000))

ggplot(data=data,mapping = aes(x=Amount, y=PaymentDays)) +geom_point(alpha=0.24)+coord_cartesian(xlim = c(0,10000))+geom_smooth()

#do the map first 
library(dplyr)
library(readxl)
dataset<-read_excel("/Users/fanliu/Desktop/Cleaned\ Data.xlsx")
dataset<-data.frame(dataset)
class(dataset)
devtools::install_github("dkahle/ggmap")
install.packages("maps")
library(ggmap)
library(mapproj)
#see the location distribution 
map<-get_map(location = "New York")
ggmap(map)
us<-get_map(location = 'united states', zoom = 4, maptype = "terrain", source = 'google', color='color')
ggmap(us) 

#extract address data
library(car)
dataset$Street<-recode(dataset$Street, "'NA'=NA")
View(dataset)
dataset$City<-recode(dataset$City, "'NA'=NA")
dataset$State<-recode(dataset$State, "'NA'=NA")
dataset$Zipcode<-recode(dataset$Zipcode, "'NA'=NA")
address<-cbind.data.frame(dataset$Street, dataset$City,dataset$State)
address<-subset(address,is.na(dataset$Street)==FALSE)
address<-subset(address,is.na(dataset$City)==FALSE)
address<-subset(address,is.na(dataset$State)==FALSE)
View(address)
address<-address[complete.cases(address),]
class(address)
colnames(address)[c(1,2,3)]<-c("X1","X2","X3")
(address$address<-paste(address$X1, address$X2,address$X3,sep=","))
library(plyr)
address<-count(address,'address')
#the order of frequency
address[order(address$freq,decreasing = TRUE),]
class(address)
View(address)

library(httr)
library(rjson)
data <- paste0("[",paste(paste0("\"",address$address,"\""),collapse=","),"]")
url  <- "http://www.datasciencetoolkit.org/street2coordinates"
response <- POST(url,body=data)
json     <- fromJSON(content(response,type="text"))

geocode  <- as.data.frame(
  do.call(rbind,sapply(json,
                       function(x) c(address=x$address,lon=x$longitude,lat=x$latitude))))

geocode$address <- rownames(geocode)

View(geocode)

g<-merge(address, geocode, by = "address")
g$long<-as.numeric(g$lon)
g$lat<-as.numeric(g$lat)
ggmap(us) + geom_point(
  aes(x=lon, y=lat, colour=freq), data=g,size= g$freq,alpha=0.5) +
  scale_color_gradient(low = "green", high="red")

ny<-get_map(location = 'New York', zoom = 12, maptype = "terrain", source = 'google', color='color')
ggmap(ny) 
ggmap(ny) + geom_point(
  aes(x=lon, y=lat, colour=freq), data=g,size= g$freq,alpha=0.7) +
  scale_color_gradient(low = "green", high="red")

#
data<-read_excel("/Users/fanliu/Desktop/Processed\ Data.xlsx")
View(data)
attach(data)
data$Street<-recode(data$Street, "'NA'=NA")
data$City<-recode(data$City, "'NA'=NA")
data$State<-recode(data$State, "'NA'=NA")
data$Zipcode<-recode(data$Zipcode, "'NA'=NA")  



#all continuous variables
summary(lm(PaymentDays~`Number of Inquiries`+Amount_ln+Duration_sqr,data))
        
#
summary(lm(PaymentDays~`Number of Inquiries`+Amount_ln+Duration_sqr+Type+LeadSource_Ind+Fiscal,data))

#
summary(lm(PaymentDays~`Number of Inquiries`+State, data,na.action=na.exclude)) 

#
summary(lm(PaymentDays~`Number of Inquiries`+LeadSource_Ind+Zipcode,data, na.action=na.exclude))

#
summary(lm(PaymentDays~`Number of Inquiries`+LeadSource_Ind,data)) #fiscal Q4*Duration has sort of significant effect

#
summary(lm(PaymentDays~`Number of Inquiries`+LeadSource_Ind)) #Fiscal Q4 and Amount_ln*Fiscal Q4 are significant


#
summary(lm(PaymentDays~`Number of Inquiries`+LeadSource_Ind+Amount_ln*City, data, na.action=na.exclude))

#
ggplot(data=data) +geom_point(mapping = aes(x=Duration_sqr, y=PaymentDays), alpha=0.24)+coord_cartesian(xlim = c(0,20000))

ggplot(data=data) +geom_point(mapping = aes(x=Duration_sqr, y=PaymentDays), alpha=0.24)+coord_cartesian(xlim = c(0,5000))

ggplot(data=data,mapping = aes(x=Duration_sqr, y=PaymentDays)) +geom_point(alpha=0.24)+coord_cartesian(xlim = c(0,1000))+geom_smooth()

#new processed data
new<-read_excel("/Users/fanliu/Desktop/new\ processed\ data.xlsx")
View(new)
attach(new)
#that's why na.exclude doesn't work...
new$Street<-recode(new$Street, "'NA'=NA")
new$City<-recode(new$City, "'NA'=NA")
new$State<-recode(new$State, "'NA'=NA")
new$Zipcode<-recode(new$Zipcode, "'NA'=NA") 



summary(lm(PaymentDays~Amount_ln+Type+LeadSource_Ind+Duration_sqr+Fiscal+State_Ind+`Number of Inquiries`, new, na.action=na.exclude))

summary(lm(PaymentDays~LeadSource_Ind+`Number of Inquiries`,new, na.action=na.exclude)) #*inquiries

summary(lm(PaymentDays~`Number of Inquiries`,new,na.action=na.exclude)) #*

summary(lm(PaymentDays~LeadSource_Ind,new, na.action=na.exclude))  #.

summary(lm(PaymentDays~Type,new, na.action=na.exclude))  #*; negative correlation

summary(lm(PaymentDays~Amount_ln,new, na.action=na.exclude)) #*

summary(lm(PaymentDays~Duration_sqr,new, na.action=na.exclude))

summary(lm(PaymentDays~Duration,new, na.action=na.exclude))

summary(lm(PaymentDays~Fiscal,new, na.action=na.exclude)) 

summary(lm(PaymentDays~State_Ind,new, na.action=na.exclude))

summary(lm(PaymentDays~State,new, na.action=na.exclude)) 

summary(lm(PaymentDays~Zipcode,new, na.action=na.exclude)) 
#Zipcode11235       367.6667   122.7404   2.995  0.00307 **
#Zipcode10035       201.7381   106.2963   1.898  0.05907 .

summary(lm(PaymentDays~`Number of Inquiries`+LeadSource_Ind+Type+Amount_ln+Zipcode, new, na.action=na.exclude)) 

attach(new)
class(new)
new<-data.frame(new)
View(new)
hist(new$PaymentDays)
logdays<-log(new$PaymentDays)
hist(logdays)
new<-new[,-23]
which(colnames(new)=="logdays")


sqrtdays<-sqrt(new$PaymentDays)
hist(sqrtdays)
hist(PaymentDays)
new$sqrtdays<-sqrtdays

summary(lm(sqrtdays~Amount_ln+Type+LeadSource_Ind+Duration_sqr+Fiscal+State_Ind+Number.of.Inquiries, new)) #Amount_ln, Type, LeadSource_Ind, Number of Inquires

summary(lm(sqrtdays~Amount_ln, new)) #**

summary(lm(sqrtdays~Type, new)) #**

summary(lm(sqrtdays~LeadSource_Ind, new)) #**

summary(lm(sqrtdays~Duration_sqr, new))

summary(lm(sqrtdays~Fiscal, new))

summary(lm(sqrtdays~State_Ind, new))

summary(lm(sqrtdays~Number.of.Inquiries, new)) #***

summary(lm(sqrtdays~Zipcode, new)) #few are .

summary(lm(sqrtdays~Amount_ln+Type+LeadSource_Ind+Number.of.Inquiries, new))


class(new$Fiscal)
new$Fiscal<-as.factor(new$Fiscal)
new$Type<-as.factor(new$Type)
new$LeadSource_Ind<-as.factor(new$Fiscal)
new$LeadSouce_New<-as.factor(new$LeadSouce_New)
new$State_Ind<-as.factor(new$State_Ind)

sqrtdays<-sqrt(new$PaymentDays)
hist(sqrtdays)
new$sqrtdays<-sqrtdays
new$LeadSource_New<-recode(new$LeadSource_New, "'Steven'=NA") 
new$LeadSource_New<-recode(new$LeadSource_New, "'Herbert'=NA") 
which(colnames(new)=="LeadSouce_New")
colnames(new)[8]="LeadSource_New"
View(new)


summary(lm(sqrtdays~LeadSource_New+Type+Number.of.Inquiries, new,na.action=na.exclude)) 

summary(lm(sqrtdays~Amount_ln+Type+LeadSource_New+Duration_sqr+Fiscal+State_Ind+Number.of.Inquiries, new,na.action=na.exclude)) 

summary(lm(sqrtdays~Amount_ln, new)) #**

summary(lm(sqrtdays~Type, new)) #**

summary(lm(sqrtdays~LeadSource_New, new,na.action=na.exclude)) #**

summary(lm(sqrtdays~Duration_sqr, new))

summary(lm(sqrtdays~Fiscal, new))

summary(lm(sqrtdays~State_Ind, new))

summary(lm(sqrtdays~Number.of.Inquiries, new)) #***

summary(lm(sqrtdays~Zipcode, new)) #few are .

summary(lm(sqrtdays~Amount_ln+Type+LeadSource_New+Number.of.Inquiries, new,na.action=na.exclude))


#use decision tree
library(tree)
library(ISLR)
View(new)
tree.pay=tree(sqrtdays~Amount_ln+Type+LeadSource_New+Duration_sqr+Fiscal+State_Ind+Number.of.Inquiries,new)
plot(tree.pay)
tree.pred=predict(tree.pay,new)
class(tree.pred)
error_squre<-(tree.pred-new$sqrtdays)^2
RMSE<-sqrt(mean(error_squre)) #root mean square error
#[1] 3.739809


summary(new$sqrtdays)

#cross-validation
cv.new=cv.tree(tree.pay,FUN=prune.tree)
plot(cv.new$size,cv.new$dev,type="b")
prune.new=prune.tree(tree.pay,best=3)
plot(prune.new)
tree.pred=predict(prune.new,new)
error_square<-(tree.pred-new$sqrtdays)^2
RMSE<-sqrt(mean(error_squre))



tree.pay=tree(sqrtdays~Amount_ln+Type+LeadSource_New+Number.of.Inquiries,new,na.action = na.exclude)
tree.pred=predict(tree.pay,new)
class(tree.pred)
error_squre<-(tree.pred-new$sqrtdays)^2
RMSE<-sqrt(mean(error_squre))




###randome forest
install.packages("randomForest")
library(randomForest)
set.seed(12)
fit<-randomForest(sqrtdays~Amount_ln+Type+LeadSource_New+Duration_sqr+Fiscal+State_Ind+Number.of.Inquiries,data=new, importance=TRUE,ntree=500,na.action = na.exclude)
pred<-predict(fit,new)
class(pred)

which(is.na(pred))
class(new$sqrtdays)
sqrtdays<-new$sqrtdays[-c(38,43,157,260,315,326,330,331)]
pred<-pred[-c(38,43,157,260,315,326,330,331)]
error_squre<-(pred-sqrtdays)^2
RMSE<-sqrt(mean(error_squre))
#[1] 2.357037

#try more trees
View(new)
set.seed(1)
fit<-randomForest(sqrtdays~Amount_ln+Type+LeadSource_New+Duration_sqr+Fiscal+State_Ind+Number.of.Inquiries,data=new, importance=TRUE,ntree=2000,na.action = na.exclude)
pred<-predict(fit,new)   
which(is.na(pred))
class(new$sqrtdays)
sqrtdays<-new$sqrtdays[-c(38,43,157,260,315,326,330,331)]
pred<-pred[-c(38,43,157,260,315,326,330,331)]
error_squre<-(pred-sqrtdays)^2
RMSE<-sqrt(mean(error_squre))
#[1] 2.366139 doesn't become better


write.csv(new,file = "new")
new<-read_excel("/Users/fanliu/Desktop/new\ with\ complete\ Leadsource\ and\ SqrtDays.xlsx")
View(new)
new$Fiscal<-as.factor(new$Fiscal)
new$Type<-as.factor(new$Type)
new$LeadSource_Ind<-as.factor(new$Fiscal)
new$LeadSource_New<-as.factor(new$LeadSource_New)
new$State_Ind<-as.factor(new$State_Ind)
new$Street<-recode(new$Street, "'NA'=NA")
new$City<-recode(new$City, "'NA'=NA")
new$State<-recode(new$State, "'NA'=NA")
new$Zipcode<-recode(new$Zipcode, "'NA'=NA") 


set.seed(1)
fit<-randomForest(sqrtdays~Amount_ln+Type+LeadSource_New+Duration_sqr+Fiscal+State_Ind+Number.of.Inquiries,data=new, importance=TRUE,ntree=2000,na.action = na.exclude)
pred<-predict(fit,new)  
write.csv(pred,file = "pred")
which(is.na(pred))
class(new$sqrtdays)
sqrtdays<-new$sqrtdays[-260]
pred<-pred[-260]
error_squre<-(pred-sqrtdays)^2
RMSE<-sqrt(mean(error_squre))
#[1] 2.35505

#divide data set to training dataset and test dataset
set.seed(12)
train=sample(1:nrow(new),300)
new.test=new[-train,]
new.train=new[train,]

set.seed(1)
fit<-randomForest(sqrtdays~Amount_ln+Type+LeadSource_New+Duration_sqr+Fiscal+State_Ind+Number.of.Inquiries,data=new.train, importance=TRUE,ntree=3000,na.action = na.exclude)
pred<-predict(fit,new.test)   
which(is.na(pred))
sqrtdays<-new.test$sqrtdays[-37]
pred<-pred[-37]
error_squre<-(pred-sqrtdays)^2
RMSE<-sqrt(mean(error_squre))
#[1]4.173731  when the training dataset is 200, ntree=2000
#[1]3.875617  when the training dataset is 300, ntree=2000
#[1] 3.877704 when the training dataset is 300, ntree=3000


#negative binomial regression
update.packages("MASS")
if ("package:MASS" %in% search()) { detach("package:MASS", unload=TRUE) }
if ("MASS" %in% rownames(installed.packages())) { remove.packages("MASS") }

require(MASS)
library(MASS)
new<-read_excel("/Users/fanliu/Desktop/new\ processed\ data.xlsx")
View(new)
#change categorical variables to factor
new$Fiscal<-as.factor(new$Fiscal)
new$Type<-as.factor(new$Type)
new$LeadSource_Ind<-as.factor(new$LeadSource_Ind)
new$LeadSource_New<-as.factor(new$LeadSource_New)
new$State_Ind<-as.factor(new$State_Ind)
new$Street<-recode(new$Street, "'NA'=NA")
new$City<-recode(new$City, "'NA'=NA")
new$State<-recode(new$State, "'NA'=NA")
new$Zipcode<-recode(new$Zipcode, "'NA'=NA") 
#individual summary
summary(glm.nb(PaymentDays~Amount_ln, new)) 

summary(glm.nb(PaymentDays~Type, new)) 

summary(glm.nb(PaymentDays~LeadSource_New, new,na.action = na.exclude)) 

summary(glm.nb(PaymentDays~LeadSource_Ind, new,na.action = na.exclude)) 

summary(glm.nb(PaymentDays~Duration_sqr, new)) 

summary(glm.nb(PaymentDays~Fiscal, new)) 

summary(glm.nb(PaymentDays~State_Ind, new)) 

summary(glm.nb(PaymentDays~`Number of Inquiries`, new)) 

summary(glm.nb(PaymentDays~State, new, na.action = na.exclude)) 

?glm.nb

View(new)
attach(new)
set.seed(12)
train=sample(1:nrow(new),300)
new.test=new[-train,]
new.train=new[train,]

#model 1
summary(glm.nb(PaymentDays~Type+`Number of Inquiries`, new, na.action = na.exclude)) 

fit1<-glm.nb(PaymentDays~Type+`Number of Inquiries`, new, na.action = na.exclude)
pred1<-predict(fit1,new,type="link")
payment<-new$PaymentDays
pred1<-exp(pred1)
a<-abs(payment-pred1)/payment
which(a=="Inf")
a<-a[-c(150,206,292,293,299,322,325,334,341,343,347,348,349)]
mean(a)
#[1] 1.862925....you can examine accuracy in this way

hist(new$PaymentDays)
summary(new$PaymentDays)



#model 2
summary(glm.nb(PaymentDays~`Number of Inquiries`+LeadSource_Ind, new1, na.action = na.exclude)) 

fit2<-glm.nb(PaymentDays~Type+`Number of Inquiries`, new, na.action = na.exclude)
pred2<-predict(fit2,new,type="link")
payment<-new$PaymentDays
pred2<-exp(pred2)
a<-abs(payment-pred2)/payment
which(a=="Inf")
a<-a[-c(150,206,292,293,299,322,325,334,341,343,347,348,349)]
mean(a)
#[1] 1.862925....


#try linear regression
summary(lm(PaymentDays~Amount_ln, new)) #0.024 *

summary(lm(PaymentDays~Type, new)) #0.0257*
read_excel()

summary(lm(PaymentDays~LeadSource_New, new,na.action = na.exclude)) 

summary(lm(PaymentDays~LeadSource_Ind, new,na.action = na.exclude)) 

summary(lm(PaymentDays~Duration_sqr, new)) 

summary(lm(PaymentDays~Fiscal, new)) 

summary(lm(PaymentDays~State_Ind, new)) 

summary(lm(PaymentDays~`Number of Inquiries`, new)) #0.0295 *  

summary(lm(PaymentDays~State, new, na.action = na.exclude)) 

summary(PaymentDays)
hist(PaymentDays)

summary(lm(PaymentDays~Type+`Number of Inquiries`, new, na.action = na.exclude)) 

fit1<-lm(PaymentDays~Type+`Number of Inquiries`, new, na.action = na.exclude)
pred1<-predict(fit1,new)
payment<-new$PaymentDays
a<-(abs(payment-pred1)/payment)
a<-a[a<=1]
mean(a)
#[1] 0.4069393
which(a=="Inf")
a<-a[-c(150,206,292,293,299,322,325,334,341,343,347,348,349)]
mean(a)
#[1] 1.86481. no way...


#try randome forest
set.seed(1)
fit1<-randomForest(PaymentDays~Type+`Number of Inquiries`,data=new, importance=TRUE,ntree=2000,na.action = na.exclude)
pred1<-predict(fit1,new)   
payment<-new$PaymentDays
a<-(abs(payment-pred1)/payment)
write.csv(a,file = "a")
write.csv(pred1,file = "pred1")
which(a=="Inf")
a<-a[-c(150,206,292,293,299,322,325,334,341,343,347,348,349)]
mean(a)
#[1] 1.849634

a<-a[a<=3]
mean(a)
#[1] 0.7201365

a<-a[a<=1]
mean(a)
#[1] 0.3929841


#use values that paymentdays<=77 days as training dataset
class(new)
new<-data.frame(new)
new.train=subset(new, new$PaymentDays<=77)
new.test=subset(new, new$PaymentDays>77)

library(tree)
library(ISLR)
library(randomForest)
set.seed(1)
fit1<-randomForest(PaymentDays~Type+Number.of.Inquiries,data=new.train, importance=TRUE,ntree=5000,na.action = na.exclude)
pred1<-predict(fit1,new.test)   
payment<-new.test$PaymentDays
a<-(abs(payment-pred1)/payment)
mean(a)
#[1] 0.7130858  when ntree=2000

#[2]0.7130142  when ntree=5000



install.packages("boot")
library(boot)
library(readxl)
new<-read_excel("/Users/fanliu/Desktop/new\ processed\ data.xlsx")
View(new)
attach(new)
bootobject <- boot(data=new, statistic=median , R=1000, PaymentDays~`Number of Inquiries`) 

# function to obtain regression weights
set.seed(1)
train=sample(1:nrow(new),175)
new.test=new[-train,]
new.train=new[train,]



bs <- function(formula, new,train) {
  d <- new[train,] # allows boot to select sample 
  fit <- lm(formula=PaymentDays~Type+`Number of Inquiries`, data=d)
  return(coef(fit)) 
} 
# bootstrapping with 1000 replications 
results <- boot(data=new, statistic=bs, 
  	R=1000, formula=PaymentDays~Type+`Number of Inquiries`)
?boot
# view results
results
plot(results, index=1) # intercept 
plot(results, index=2) # wt 
plot(results, index=3) # disp 

# get 95% confidence intervals 
boot.ci(results, type="bca", index=1) # intercept 
boot.ci(results, type="bca", index=2) # wt 
boot.ci(results, type="bca", index=3) # disp


write.csv(new1.test,file = "test1")

new<-read_excel("/Users/fanliu/Desktop/new\ processed\ data.xlsx")

new$PD<-cut(new$PaymentDays,breaks = c(-Inf,30,60,Inf), labels = c("Due","Overdue","Deadbeat"))
new1<-new[new$PD==c("Overdue","Deadbeat"),]
View(new1)



set.seed(1)
train=sample(1:nrow(new1),70)
new1.test=new1[-train,]
new1.train=new1[train,]

fit1<-glm(PD~Type+`Number of Inquiries`,family = binomial(link = 'logit'),data=new1.train)
summary(fit1)
pred<-predict(fit1, new1.test)
probs<-exp(pred)/(1+exp(pred)) #the same as predict (fit1, new1.test, type="response")
str(new1.test$PD)
probs_pd<-ifelse(probs>0.51,"Deadbeat","Overdue")
cbind(probs_pd,new1.test$PD)

#multiple logsitics regression
View(new)
require(foreign)
require(nnet)
require(reshape2)
set.seed(1)
train=sample(1:nrow(new),175)   #this training dataset has lowest misclassification error
new.test=new[-train,]
new.train=new[train,]
model<-multinom(PD~Type+`Number of Inquiries`,data=new.train)
summary(model)
class(model)
pred<-predict(model,new.test,type="prob")
class(pred)
?apply

#calculate the cumulative probabilities associated with each option
predictMNL <- function(model, newdata) {
 
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
 
    # Draw random values
    vals <- runif(nrow(newdata))
 
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
 
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
 
    # Return the values
    return(ids)
  }
}

y_pred<-predictMNL(model, new.test)
new.test2<-cbind(new.test,y_pred)
View(new.test2)
#recode for categorical variables; cut for continuous to categorical
library(car)
new.test2$y_pred2<-recode(new.test2$y_pred, "1='Due';2='Overdue';3='Deadbeat'")
?recode
tab<-table(new.test2$y_pred2,new.test$PD)
mis_rate<-1-sum(diag(tab))/sum(tab)
#[1] 0.637931


#second try on multinomal regression
set.seed(1)
train=sample(1:nrow(new),250)
new.test=new[-train,]
new.train=new[train,]
model<-multinom(PD~Type+`Number of Inquiries`,data=new.train)
summary(model)
class(model)
pred<-predict(model,new.test,type="prob")
class(pred)
?apply

#calculate the cumulative probabilities associated with each option
predictMNL <- function(model, newdata) {
 
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
 
    # Draw random values
    vals <- runif(nrow(newdata))
 
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
 
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
 
    # Return the values
    return(ids)
  }
}

y_pred<-predictMNL(model, new.test)
new.test2<-cbind(new.test,y_pred)
View(new.test2)
#recode for categorical variables; cut for continuous to categorical
library(car)
new.test2$y_pred2<-recode(new.test2$y_pred, "1='Due';2='Overdue';3='Deadbeat'")
?recode
tab<-table(new.test2$y_pred2,new.test$PD)
mis_rate<-1-sum(diag(tab))/sum(tab)
#[1] 0.6666667

#third try 
new<-read_excel("/Users/fanliu/Desktop/new\ processed\ data.xlsx")

new$PD<-cut(new$PaymentDays,breaks = c(-Inf,30,60,Inf), labels = c("Due","Overdue","Deadbeat"))
View(new)
which(colnames(new)=="PD")
new<-new[,-24]

set.seed(1)
train=sample(1:nrow(new),280)
new.test=new[-train,]
new.train=new[train,]

model<-multinom(PD~Type+`Number of Inquiries`,data=new.train)

class(model)
pred<-predict(model,new.test,type="prob")
class(pred)
?apply

#calculate the cumulative probabilities associated with each option
predictMNL <- function(model, newdata) {
 
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
 
    # Draw random values
    vals <- runif(nrow(newdata))
 
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
 
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
 
    # Return the values
    return(ids)
  }
}

y_pred<-predictMNL(model, new.test)
new.test2<-cbind(new.test,y_pred)
View(new.test2)
#recode for categorical variables; cut for continuous to categorical
library(car)
new.test2$y_pred2<-recode(new.test2$y_pred, "1='Due';2='Overdue';3='Deadbeat'")
?recode
tab<-table(new.test2$y_pred2,new.test$PD)
mis_rate<-1-sum(diag(tab))/sum(tab)
#[1]0.7681159



#random forest/boostrap/boosting..in both classification and regression models 
###randome forest with regression
install.packages("randomForest")
library(randomForest)
View(new)


new1<-subset(new, is.na(new$LeadSource_New)==FALSE) #something wrong with na coercion
View(new1)
fit<-randomForest(PaymentDays~Amount_ln+Type+LeadSource_New+Duration_sqr+Fiscal+new1$`Number of Inquiries`,data=new1, importance=TRUE,ntree=500)
class(new1$Amount_ln)
a <- c("1", "2",letters[1:5], "3")
as.numeric(a)

new1$Type<-as.factor(new1$Type)
new1$LeadSource_New<-as.factor(new1$LeadSource_New)
new1$Fiscal<-as.factor(new1$Fiscal)



pred<-predict(fit,new1)
class(pred)
View(pred)
error<-(new1$PaymentDays-pred)/new1$PaymentDays
error<-subset(error,error!="-Inf")
mean(error)
#[1] -0.8891046  

pred<-predict(fit,interval="confidence") 

?predict

install.packages("randomForest")
library(randomForest)
View(new)
set.seed(12)

new1<-subset(new, is.na(new$LeadSource_New)==FALSE) 
View(new1)

set.seed(1)
train=sample(1:nrow(new1),280)
new1.test=new1[-train,]
new1.train=new1[train,]


fit2<-randomForest(PaymentDays~Amount_ln+Type+LeadSource_New+Duration_sqr+Fiscal+new1.train$`Number of Inquiries`,data=new1.train, importance=TRUE,ntree=500)

a <- c("1", "2",letters[1:5], "3")
as.numeric(a)


pred<-predict(fit2,newdata = new1.test) 
class(pred)
View(pred)
error<-(new1$PaymentDays-pred)/new1$PaymentDays
error<-subset(error,error!="-Inf")
mean(error)


###CI for linear regression
#model 1
fit1<-lm(PaymentDays~Type+new1$`Number of Inquiries`, new1)
View(new1)
predict(fit1,new1,interval = "confidence") #confidence interval for each prediction value
confint(fit1) #CI for the parameters

#model 2
fit2<-lm(PaymentDays~new1$`Number of Inquiries`+LeadSource_Ind, new1)
predict(fit2,new1,interval = "confidence") #confidence interval for each prediction value
confint(fit2) #CI for the parameters


###CI for negative binomial
#model 1

model<-multinom(PD~Type+`Number of Inquiries`,data=new1)

summary(model)
methods(confint) 
exp(coef(model))

ci<-confint(model)
exp(ci)

#model 2

model<-multinom(PD~new1$`Number of Inquiries`+LeadSource_Ind,data=new1)

summary(model)
methods(confint) 
exp(coef(model))

ci<-confint(model)
exp(ci) 


###CI for random forest

fit<-randomForest(PaymentDays~Amount_ln+Type+LeadSource_New+Duration_sqr+Fiscal+new1$`Number of Inquiries`,data=new1, keep.inbag=TRUE,importance=TRUE,ntree=500)

install.packages("RFinfer")
library(RFinfer)
rfPredVar(fit,new1,CI=TRUE,tree.type = 'rf')
#R package RFinfer for generating prediction and prediction variances from random forests. 

###CI for time series
#Quarter
q<-read_excel("Quarter.xlsx")
q<-data.frame(q)
View(q)
summary(lm(q$y~q$x.code,q))
lm1<-lm(q$y~q$x.code,q)
confint(lm1)
code<-data.frame(x.code=q$x.code) 
pred<-predict(lm1,code,interval = "confidence")
class(q$x.code)
class(q$y)
class(pred)
apply(pred,2,mean)

#no 2012
no_12<-read_excel("no\ 2012.xlsx")
no_12<-data.frame(no_12)
summary(lm(no_12$y~no_12$x,data))
lm2<-lm(no_12$y~no_12$x,data)
confint(lm2)
x<-data.frame(x=no_12$x)
pred<-predict(lm2,x,interval = "confidence")
apply(pred,2,mean)




###CI for linear regression
#model 1
fit1<-lm(PaymentDays~Type+new1$`Number of Inquiries`, new1)
View(new1)
predict(fit1,new1,interval = "confidence") #confidence interval for each prediction value
confint(fit1) #CI for the parameters

###CI for negative binomial
#model 1

model<-multinom(PD~Type+`Number of Inquiries`,data=new1)

summary(model)
methods(confint) 
exp(coef(model))

ci<-confint(model)
exp(ci)

###CI for random forest

fit<-randomForest(PaymentDays~Amount_ln+Type+LeadSource_New+Duration_sqr+Fiscal+new1$`Number of Inquiries`,data=new1, keep.inbag=TRUE,importance=TRUE,ntree=500)

install.packages("RFinfer")
library(RFinfer)
rfPredVar(fit,new1,CI=TRUE,tree.type = 'rf')

###CI for time series
#Quarter
q<-read_excel("Quarter.xlsx")
q<-data.frame(q)
View(q)
summary(lm(q$y~q$x.code,q))
lm1<-lm(q$y~q$x.code,q)
confint(lm1)
code<-data.frame(x.code=q$x.code)
pred<-predict(lm1,code,interval = "confidence")
class(q$x.code)
class(q$y)
class(pred)
apply(pred,2,mean)

#no 2012
no_12<-read_excel("no\ 2012.xlsx")
no_12<-data.frame(no_12)
summary(lm(no_12$y~no_12$x,no_12))
lm2<-lm(no_12$y~no_12$x,no_12)
confint(lm2)
x<-data.frame(x=no_12$x)
pred<-predict(lm2,x,interval = "confidence")
apply(pred,2,mean)

#no 2013
no_13<-read_excel("no\ 2013.xlsx")
no_13<-data.frame(no_13)
summary(lm(no_13$y~no_13$x,no_13))
lm3<-lm(no_13$y~no_13$x,no_13)
confint(lm3)
x<-data.frame(x=no_13$x)
pred<-predict(lm3,x,interval = "confidence")
apply(pred,2,mean)

#all
all<-read_excel("all.xlsx")
all<-data.frame(all)
summary(lm(all$y~all$x.code,all))
lm4<-lm(all$y~all$x.code,all)
confint(lm4)
x<-data.frame(x=all$x.code)
pred<-predict(lm4,x,interval = "confidence")
apply(pred,2,mean)


