library(tidyverse)
#gettind rid of all the non-numeric values by replacing them with NA
PassRate = as.numeric(SOL$`2017-2018 Pass Rate`)
AdvPassRate = as.numeric(SOL$`2017-2018 Adv Pass Rate`)
SOL = mutate(SOL,PassRate)
SOL = mutate(SOL,AdvPassRate)
#removing all the NA values
SOL = SOL[!is.na(SOL$PassRate),]

#finding SOL pass and advance pass average from each county 
countyave = aggregate(SOL$PassRate,by = list(SOL$`Div Name`),FUN=mean)
countyadave = aggregate(SOL$AdvPassRate,by = list(SOL$`Div Name`),FUN=mean)
names(countyadave)[2]<-'AdvPassRate'
SOLAve = mutate(countyave,countyadave$AdvPassRate)
names(SOLAve)[1]<-'County'
names(SOLAve)[2]<-'PassRate'
names(SOLAve)[3]<-'AdvPassRate'

#deciding ranking criteria for children in poverty
povmax = max(CountyData$`Children in Poverty`)
povmin = min(CountyData$`Children in Poverty`)
povinterval = (povmax - povmin)/5
#creating the ranking column and adding it to the county data 
ChildPovertyRank = cut(CountyData$`Children in Poverty`, c(-Inf, povmin+ povinterval, povmin+ povinterval*2, povmin+povinterval*3,povmin+ povinterval*4, Inf), labels=  c("Very Low","Low","Moderate","High","Very High"))
CountyData = mutate(CountyData,ChildPovertyRank)

#deciding ranking criteria for high school graduation rate
gradmax = max(CountyData$`High School Graduation`)
gradmin = min(CountyData$`High School Graduation`)
gradinterval = (gradmax - gradmin)/5
#creating the ranking column and adding it to the county data 
GradRank = cut(CountyData$`High School Graduation`, c(-Inf, gradmin+gradinterval, gradmin+gradinterval*2, gradmin+gradinterval*3, gradmin+gradinterval*4, Inf), labels=  c("Very Low","Low","Moderate","High","Very High"))
CountyData = mutate(CountyData,GradRank)

FoodCol = CountyData$`Limited Access to Healthy Foods`[!is.na(CountyData$`Limited Access to Healthy Foods`)]
foodmax = max(FoodCol)
foodmin = min(FoodCol)
foodinterval = (foodmax-foodmin)/5

FoodRank = cut(CountyData$`Limited Access to Healthy Foods`, c(-Inf, foodmin+foodinterval, foodmin+foodinterval*2, foodmin+foodinterval*3, foodmin+foodinterval*4, Inf), labels=  c("Very Low","Low","Moderate","High","Very High"))
CountyData = mutate(CountyData,FoodRank)


#for(i in length(CountyData$`Limited Access to Healthy Foods`)){
#  if(is.na(CountyData$`Limited Access to Healthy Foods`[i]) ){
#    CountyData$`Limited Access to Healthy Foods`[i] <- 'Value Unknown'
#  }
#  if(!is.na(CountyData$`Limited Access to Healthy Foods`[i])){
#    CountyData$`Limited Access to Healthy Foods`[i] <- cut(CountyData$`Limited Access to Healthy Foods`[i], c(-Inf, foodmin+foodinterval, foodmin+foodinterval*2, foodmin+foodinterval*3, foodmin+foodinterval*4, Inf), labels=  c("Very Low","Low","Moderate","High","Very High"))
#  }
#}
