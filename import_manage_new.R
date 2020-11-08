## ----setup, include=FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------

setwd('C:\Users\admin\Documents\Lehre\Lehre FSS 19\Data')



## --------------------------------------------------------------------------------------------------------

setwd('C:/Users/admin/Documents/topic_course')

list.files()



## --------------------------------------------------------------------------------------------------------

# go UP in the tree
setwd("..")

#go down into a subdirectory
setwd("./topic_course")


## --------------------------------------------------------------------------------------------------------

dir.create("files")

setwd("./files")



## --------------------------------------------------------------------------------------------------------
setwd("./files")
getwd()


## --------------------------------------------------------------------------------------------------------
getwd()


## --------------------------------------------------------------------------------------------------------

list.files()


## --------------------------------------------------------------------------------------------------------

#?read.table()



## --------------------------------------------------------------------------------------------------------

c1<-read.csv('presidential_national_toplines_2020.csv',sep=";")


cx<-read.csv('https://raw.githubusercontent.com/msaeltzer/topic_course/master/Session_6/presidential_national_toplines_2020.csv')

#
setwd("./session_6")




## --------------------------------------------------------------------------------------------------------

load('presidents.rdata')



## --------------------------------------------------------------------------------------------------------

save(c1,file='presidents.rdata')

## --------------------------------------------------------------------------------------------------------
r1<-readRDS('presidents.rds')

rm(r1)


## --------------------------------------------------------------------------------------------------------

saveRDS(r1,file='presidents.rds')

## --------------------------------------------------------------------------------------------------------

if(!require(readstata13)){install.packages("readstata13")}

## --------------------------------------------------------------------------------------------------------


library(foreign)
library(readstata13)

fr1<-read.dta13("ZA5861_v1-0-0.dta")
fr2<-read.spss("ZA5861_v1-0-0.sav",to.data.frame = T)


## --------------------------------------------------------------------------------------------------------

attributes(c1)


## --------------------------------------------------------------------------------------------------------
list.files()
c2<-read.csv("candidate_summary_2020.csv")



c2$Cand_Name

c2$Cand_Party_Affiliation

c2[,2:15]

c2[1:13,]

c2[,"Cand_Name"]

c2[,c("Cand_Name","Cand_Party_Affiliation")]


## --------------------------------------------------------------------------------------------------------

dems<-c2[c2$Cand_Party_Affiliation=="DEM",]

reps<-c2[c2$Cand_Party_Affiliation=="REP",]


repdem<-rbind(dems,reps)




## --------------------------------------------------------------------------------------------------------
lapply(dems,class)

table(c2$Cand_Party_Affiliation)


## --------------------------------------------------------------------------------------------------------

c2$Party<-as.factor(c2$Cand_Party_Affiliation)
c2$Party


## --------------------------------------------------------------------------------------------------------
c2$Incumbent<-factor(c2$Cand_Incumbent_Challenger_Open_Seat,labels=c("C","I","O"))



## --------------------------------------------------------------------------------------------------------

table(c2$Cand_Incumbent_Challenger_Open_Seat)

c2$Incumbent<-factor(c2$Cand_Incumbent_Challenger_Open_Seat,labels=c("","C","I","O"))
                


## --------------------------------------------------------------------------------------------------------

any(is.na(c2$Incumbent))

any(c2$Incumbent=="")


## --------------------------------------------------------------------------------------------------------

c2$Incumbent<-ifelse(c2$Cand_Incumbent_Challenger_Open_Seat=="",NA,c2$Cand_Incumbent_Challenger_Open_Seat)

c2$Incumbent<-factor(c2$Incumbent,labels=c("C","I","O"))



## --------------------------------------------------------------------------------------------------------

class(c1$modeldate)

c1$modeldate[1]

c1$modeldate<-as.Date(c1$modeldate,format="%m/%d/%Y")

class(c1$modeldate)

c1$modeldate[1]-c1$modeldate[10]


## --------------------------------------------------------------------------------------------------------

ger_birthdate<-c("01.01.2010","15.02.2015","01.07.1995","01.11.2003")

#df$ger_birthdate<-as.Date(df$ger_birthdate)

ger_birthdate<-as.Date(ger_birthdate,format = "%d.%m.%Y") # day, month, year 

as.Date("2020-02-11")-ger_birthdate[3]


## --------------------------------------------------------------------------------------------------------

plot(c1$modeldate,c1$national_voteshare_inc)



## --------------------------------------------------------------------------------------------------------

plot(c1$modeldate,c1$national_voteshare_inc,type="lines",ylab="Voteshare",xlab="Time",main="Standings")

## --------------------------------------------------------------------------------------------------------

plot(c1$modeldate,c1$national_voteshare_inc,type="lines",ylim=c(44,55),xlim=as.Date(c("2020-07-01","2020-09-01")),ylab="Voteshare",xlab="Time")



## --------------------------------------------------------------------------------------------------------

plot(c1$modeldate,c1$national_voteshare_inc,type="lines",ylim=c(44,55),col="red",ylab="Voteshare",xlab="Time")
lines(c1$modeldate,c1$national_voteshare_chal,col="blue")
abline(v=as.Date("2020-10-01"))
points(as.Date(c1$modeldate[14]),c1$national_voteshare_chal[14],col="yellow")


library(ggplot2)


a1<-aggregate(c2$Cand_Id~c2$Party,data=c2,FUN="length")

plot(a1[order(a1[,2],decreasing = T),][,2],names.arg =a1[order(a1[,2],decreasing = T),][,1],type="lines")

a1<-as.data.frame(table(c2$Party))
plot(a1[order(a1$Freq),][,2])


barplot(c(5,3,3,1),names.arg = )



## --------------------------------------------------------------------------------------------------------
barplot(table(c2$Incumbent),col=c("red","blue","yellow"))



## --------------------------------------------------------------------------------------------------------
hist(c1$popwin_inc)



## --------------------------------------------------------------------------------------------------------

plot(log(c2$Total_Receipt)~c2$Incumbent)



## --------------------------------------------------------------------------------------------------------



mod<-lm(c2$Total_Receipt~(c2$Party=="DEM")+c2$Incumbent)

# linear model 
# 

plot(density(c2$Total_Receipt))


c2<-c2[c2$Total_Receipt!=0,]
mod<-lm(log(c2$Total_Receipt)~(c2$Party=="DEM")+c2$Incumbent)

plot(density(log(c2$Total_Receipt)))
summary(mod)

plot(mod$residuals~mod$model$`log(c2$Total_Receipt)`)

plot(mod)

summary(c2)
table()

mean(c2$Total_Contribution)
median(c2$Total_Contribution)
sd(c2$Total_Contribution)
var(c2$Total_Contribution)

gmod<-glm(c2$Total_Receipt~(c2$Party=="DEM")+c2$Incumbent,family="poisson")
summary(gmod)
  
  
glmer.nb()

library(plm)

library(lme4)










## --------------------------------------------------------------------------------------------------------




## --------------------------------------------------------------------------------------------------------
frc<-read.csv("house_district_forecast.csv")


## --------------------------------------------------------------------------------------------------------
                      # this is a tilde!      
vs<-aggregate(voteshare~candidate,frc,FUN="mean")



## --------------------------------------------------------------------------------------------------------
                      # this is a tilde!      
vs<-aggregate(voteshare~candidate+party,frc,FUN="mean")



## --------------------------------------------------------------------------------------------------------
                      # this is a tilde!      
vs<-aggregate(voteshare~candidate+model,frc,FUN="mean")



## --------------------------------------------------------------------------------------------------------
library(reshape2)

d1<-reshape2::dcast(frc, candidate ~ model,fun=mean, value.var="voteshare",)




## --------------------------------------------------------------------------------------------------------
library(lubridate)

# first we round the date to make it
frc$week<-round_date(frc$forecastdate,"week")

d2<-reshape2::dcast(frc, candidate ~ week,fun=mean, value.var="voteshare",)



## --------------------------------------------------------------------------------------------------------
d4<-reshape2::melt(d2)



## --------------------------------------------------------------------------------------------------------

c1<-read.csv("congress1.csv")

names(c1)



## --------------------------------------------------------------------------------------------------------

c2<-merge(c1,frc,by="candidate")



## --------------------------------------------------------------------------------------------------------

c2<-merge(c1,frc,by="candidate",all.y=T)



## --------------------------------------------------------------------------------------------------------

c2<-merge(c1,frc,by.y="candidate",by.x="name",all.y=T)


