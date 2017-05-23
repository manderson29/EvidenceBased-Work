# Problem set 2

rm(list=ls())

co2_p1 = readLines("ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/law/law_co2.txt")
co2_p1 = co2_p1[278:424]
co2_p1 = as.numeric(unlist(strsplit(co2_p1," ")))
co2_p1 = co2_p1[complete.cases(co2_p1)]
co2_p1 = matrix(co2_p1,ncol=2,byrow=TRUE)

co2_p2 = readLines("ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_annmean_mlo.txt")
co2_p2 = co2_p2[58:(length(co2_p2))]
co2_p2 = as.numeric(unlist(strsplit(co2_p2," ")))
co2_p2 = co2_p2[complete.cases(co2_p2)]
co2_p2 = matrix(co2_p2,ncol=3,byrow=TRUE)

#grab years of both
ind1 = co2_p1[,1]
ind2 = co2_p2[,1]

#find intersection
int = intersect(ind1,ind2)

#grab parts of two matrices not in intersection
ind1 = !co2_p1[,1]%in%int
co2_p1c = co2_p1[ind1,]
ind2 = !co2_p2[,1]%in%int
co2_p2c = co2_p2[ind2,]

#now make means of middle
ind1 = co2_p1[,1]%in%int
ind2 = co2_p2[,1]%in%int
co2_p1int = co2_p1[ind1,]
co2_p2int = co2_p2[ind2,1:2]
co2_mean = (co2_p1int[,2]+co2_p2int[,2])/2
co2_mean = cbind(co2_p1int[,1],co2_mean)

#now put all parts together
#Q2a
frame = rbind(co2_p1c,co2_mean,co2_p2c[,1:2])
colnames(frame) = c("year","CO2")

#grab temp data
temp = readLines("http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.5.0.0.annual_ns_avg.txt")
temp = as.numeric(unlist(strsplit(temp," ")))
temp = temp[complete.cases(temp)]
temp = matrix(temp,ncol=12,byrow=T)
temp = temp[,1:2]

#combine temp and CO2
tempYears = temp[,1]
ind = frame[,1]%in%tempYears
frame = frame[ind,]
temp = temp[1:(nrow(temp)-1),]

frame = cbind(frame,temp[,2])
trainingset = frame[frame[,1]%in%1850:1974,]
testset = frame[frame[,1]%in%1975:2016,]
testset = data.frame(testset)
trainingset = data.frame(trainingset)
colnames(testset) = c("Year","MeanCO2","ChangesinTemp")
colnames(trainingset) =c("Year","MeanCO2","ChangesinTemp")



#2 Estimating the model
mod1<-lm(trainingset$ChangesinTemp~trainingset$MeanCO2)
summary(mod1)


#3

beta<-mod1$coefficients
beta<-as.data.frame(beta)
beta[-(1),]
beta<-t(beta)
beta<-as.data.frame(beta)
beta

predictions<-beta$`(Intercept)` +(beta$`trainingset$MeanCO2`*testset$MeanCO2)
n<-length(testset$Year)

#Confidence interval with uncertainty

t<-qt(.975,df=n-2)

xbar<- mean(trainingset$ChangesinTemp)

S2<-var(testset$ChangesinTemp)
S = sqrt(S2)
Sxx<-(sum(testset$ChangesinTemp^2)-(1/n)*sum(testset$ChangesinTemp)^2)#on page 575  

timesS<-(sqrt((1/n)+(((testset$ChangesinTemp-xbar)^2)/Sxx)))

se<-S*timesS


lb<-predictions-t*se
ub<-predictions+t*se

plot(testset$ChangesinTemp)
lines(lb,col=2)
lines(ub,col=2)

confint.t<-as.data.frame(cbind(lb,ub))


#without parameter uncertainty

z<-(1.96)

lbc<-predictions-z*S
ubc<-predictions+z*S

plot(testset$ChangesinTemp)
lines(lb2,col=5)
lines(ub2,col=5)




#4
#Now lets build prediction Intervals

### with parameter uncertainty
n2<-length(trainingset$Year)

t<-qt(.975,df=n2-2)

xbar<- mean(trainingset$ChangesinTemp)

S2<-var(testset$ChangesinTemp)
S<-sqrt(S2)
Sxx<-(sum(testset$ChangesinTemp^2)-(1/n2)*sum(testset$ChangesinTemp)^2)  

timesS<-(sqrt(1+ (1/n2)+(((testset$ChangesinTemp-xbar)^2)/Sxx)))

se2<-S*timesS

lbpuc<-predictions-t*se2
ubpuc<-predictions+t*se2

plot(testset$ChangesinTemp)
lines(lbpuc,col=5)############### ####Uncertainty######
lines(ubpuc,col=5)###############


#Now lets add parameter certainty

z<-1.96         ######you changed var to trainingset

xbar<- mean(trainingset$ChangesinTemp)

S2<-var(trainingset$ChangesinTemp)
Stest<-sqrt(S2)
#Sxx<-(sum(testset$ChangesinTemp^2)-(1/n2)*sum(testset$ChangesinTemp)^2)  

#timesS<-(sqrt(1+ (1/n2)+(((testset$ChangesinTemp-xbar)^2)/Sxx)))

#se2<-S*timesS

lbpc<-predictions-z*Stest########## ####Certainty#####
ubpc<-predictions+z*Stest##########

plot(predictions)
lines(lbpuc,col=2)
lines(ubpuc,col=2)
points(testset$ChangesinTemp,col="blue")
lines(lbpc)
lines(ubpc)


#5



totaldata<-rbind(trainingset,testset)
mod.p<-lm(trainingset$ChangesinTemp~trainingset$MeanCO2)
summary(mod.p)

predict.perf<-beta$`(Intercept)` +(beta$`trainingset$MeanCO2`*trainingset$MeanCO2)
totalpredict<-cbind(t(predict.perf),t(predictions))
totalpredict<-t(totalpredict)

totalpredict.year<-cbind(t(trainingset$Year),t(testset$Year))
totalpredict.year<-t(totalpredict.year)

plot(totalpredict,col=2)
points(totaldata$ChangesinTemp)





############
#need prediction intervals for training set
z<-1.96

xbar<- mean(trainingset$ChangesinTemp)

S2<-var(trainingset$ChangesinTemp)
S<-sqrt(S2)
#Sxx<-(sum(trainingset$ChangesinTemp^2)-(1/n2)*sum(trainingset$ChangesinTemp)^2)  

#timesS<-(sqrt(1+ (1/n2)+(((trainingset$ChangesinTemp-xbar)^2)/Sxx)))

se3<-S*timesS

lbptc<-predict -z*S ##########Certainty Training set
ubptc<-predict +z*S ##########

plot(predict.perf)
lines(ubptc)
lines(lbptc)

#############
#The idea make a bind of the total prediction interval col1 then replot training set prediction col2


######### Big plot
totalprediction.lb<-cbind(t(lbptc),t(lbpc))
totalprediction.lb<-t(totalprediction.lb)
totalprediction.ub<-cbind(t(ubptc),t(ubpc))
totalprediction.ub<-t(totalprediction.ub)

plot(totalpredict,ylim=c(-.7,.8),ylab="Changes in Temp",xlab="Year (Starting from 1850)")
points(totaldata$ChangesinTemp,col="blue")

lines(totalprediction.lb,col="red")
lines(totalprediction.ub,col="red")

lines(ubpt,col=5)#prediction intervals for training set
lines(lbpt,col=5)

legend('topleft',c("Training Set Interval","Test Set Interval","Data Points","Point Predictions"), col=c(5,2,"blue","black"),lty =c(1,1,0,0),pch = c(0,0,1,1),cex = .6)
#Ja boy

#6
#PIT what the f is that 

hist(totaldata$MeanCO2)

js<-summary(mod1)$sigma #mod 1 is the original ols 
#js is residual standard error of mod1

pit<-pnorm(testset$ChangesinTemp,predictions,js)#based on excel sheet
plot.ts(pit)
hist(pit) #Hist of pit



emppit<-ecdf(pit)
plot(emppit,do.points=F)





#testing to see how well our 
test<-lm(testset$ChangesinTemp~pit)
summary(test)

tbeta<-test$coefficients
tbeta<-as.data.frame(tbeta)
tbeta[-(1),]
tbeta<-t(tbeta)
tbeta<-as.data.frame(tbeta)
tbeta

t.p<-tbeta$`(Intercept)`+tbeta$pit*testset$MeanCO2
plot.ts(t.p) 
length(t.p)
f<-205:(205+42)
lines(f,col=2)
