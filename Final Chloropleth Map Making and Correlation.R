
library(ggplot2)
library(usmap)

#Changing names to be one word
names(Binned_Data_Final)[17] <- ('SOL_Pass_Rate')
names(Binned_Data_Final)[18] <- ('SOL_Advance_Rate')
#Making the 'NA' values into actually being a NA value
Binned_Data_Final$WhiteBlackSeg[Binned_Data_Final$WhiteBlackSeg == 'NA'] <- NA
#making the column numeric so you can do correlation
Binned_Data_Final[7] <- as.numeric(Binned_Data_Final$WhiteBlackSeg)


#plotting and finding correlation between White Black Segregation and SOL pass rates
ggplot(data=Binned_Data_Final, aes(x=Binned_Data_Final$WhiteBlackSeg, y=Binned_Data_Final$SOL_Pass_Rate))+ geom_point()
cor(Binned_Data_Final$WhiteBlackSeg,Binned_Data_Final$SOL_Pass_Rate, use="pairwise.complete.obs")


#plotting and finding correlation between White Black Segregation and SOL advanced pass rate
ggplot(data=Binned_Data_Final, aes(x=Binned_Data_Final$WhiteBlackSeg, y=Binned_Data_Final$SOL_Advance_Rate))+ geom_point()
cor(Binned_Data_Final$WhiteBlackSeg,Binned_Data_Final$SOL_Advance_Rate, use="pairwise.complete.obs")

#plotting and finding correlation between White NonWhite and SOL pass rate
ggplot(data=Binned_Data_Final, aes(x=Binned_Data_Final$WhiteNonWhiteSeg, y=Binned_Data_Final$SOL_Pass_Rate))+ geom_point()
#making the 'NA's into NA values and making the column numeric
Binned_Data_Final$WhiteNonWhiteSeg[Binned_Data_Final$WhiteNonWhiteSeg == 'NA'] <- NA
Binned_Data_Final[9] <- as.numeric(Binned_Data_Final$WhiteNonWhiteSeg)

cor(Binned_Data_Final$WhiteNonWhiteSeg,Binned_Data_Final$SOL_Pass_Rate, use="pairwise.complete.obs")

#plotting and finding correlation between White NonWhite and SOL advanced pass rate
ggplot(data=Binned_Data_Final, aes(x=Binned_Data_Final$WhiteNonWhiteSeg, y=Binned_Data_Final$SOL_Advance_Rate))+ geom_point()
cor(Binned_Data_Final$WhiteNonWhiteSeg,Binned_Data_Final$SOL_Advance_Rate, use="pairwise.complete.obs")
library("ggpubr")
ggdensity(Binned_Data_Final$WhiteBlackSeg, 
          main = "Density plot",
          xlab = "Tooth length")

ggqqplot(Binned_Data_Final$WhiteNonWhiteSeg)

shapiro.test(Binned_Data_Final$WhiteNonWhiteSeg)


library("ggpubr")
ggscatter(Binned_Data_Final, x = "WhiteBlackSeg", y = "SOL_Pass_Rate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "White Black Segregationn", ylab = "SOL PAss Rate")

?cor
#probability of making a type 1 error, probability that you're gonna say there's correlation, 
#when there's not. below .05, you can reject the null, saying ther's correlation
#above .05, you cannot reject null. 

##mapping White Black Segregation
data <- data.frame(county = Binned_Data_Final$County, values = Binned_Data_Final$WhiteBlackSeg)

va <- us_map(include = "VA",regions = "counties")
loco <- unique(va$county)
dataset <- cbind(loco,Binned_Data_Final$WhiteBlackBin)
x <- data.frame(dataset)


fippy <- fips("VA", county = x$loco)
new_x<- data.frame(x,fippy)
newnew_x <- data.frame(new_x[,c(2,3)])
names(newnew_x) <- c("vals","fips")

newnew_x$vals <- factor(newnew_x$vals, levels = c("Very Low", "Low", "Moderate", "High", "Very High", "NA"))
plot_usmap(include= "VA", regions = "counties", data= newnew_x, values="vals")+ scale_fill_manual(values = c("thistle1", "rosybrown1", "lightsalmon1", "salmon3", "brown4", "gray30"), 
       name = "White-Black Segregation", guide = guide_legend(reverse = )) + theme(legend.position = "right") 



#Mapping White Nonwhite Bins
va <- us_map(include = "VA",regions = "counties")
loco <- unique(va$county)
dataset <- cbind(loco,Binned_Data_Final$WhiteNonWhiteBin)
x <- data.frame(dataset)


fippy <- fips("VA", county = x$loco)
new_x<- data.frame(x,fippy)
newnew_x <- data.frame(new_x[,c(2,3)])
names(newnew_x) <- c("vals","fips")

newnew_x$vals <- factor(newnew_x$vals, levels = c("Very Low", "Low", "Moderate", "High", "Very High", "NA"))
plot_usmap(include= "VA", regions = "counties", data= newnew_x, values="vals")+ scale_fill_manual(values = c("thistle1", "rosybrown1", "lightsalmon1", "salmon3", "brown4", "gray30"), 
                                                                                                  name = "White-NonWhite Segregation", guide = guide_legend(reverse = )) + theme(legend.position = "right") 

va <- us_map(include = "VA",regions = "counties")
loco <- unique(va$county)
dataset <- cbind(loco,Binned_Data_Final$WhiteNonWhiteBin)
x <- data.frame(dataset)


fippy <- fips("VA", county = x$loco)
new_x<- data.frame(x,fippy)
newnew_x <- data.frame(new_x[,c(2,3)])
names(newnew_x) <- c("vals","fips")

newnew_x$vals <- factor(newnew_x$vals, levels = c("Very Low", "Low", "Moderate", "High", "Very High", "NA"))
plot_usmap(include= "VA", regions = "counties", data= newnew_x, values="vals")+ scale_fill_manual(values = c("thistle1", "rosybrown1", "lightsalmon1", "salmon3", "brown4", "gray30"), 
                                                                                                  name = "White-NonWhite Segregation", guide = guide_legend(reverse = )) + theme(legend.position = "right") 

names(Income) <- c("County", "C/C", "Per_Capita_Income", "Median_Household_Income", "Median_Family_Income", "Population", "Number_Of_Households")
#Make sure Income data frame only has the values that're also in county

library(dplyr)
Income <- Income %>% semi_join(Binned_Data_Final, by = 'County')

ggplot(data=Binned_Data_Final,date = Income, aes(x=Binned_Data_Final$WhiteNonWhiteSeg, y=Binned_Data_Final$SOL_Advance_Rate))+ geom_point()

#Correlations for each category
cor(Income$Per_Capita_Income,Binned_Data_Final$SOL_Pass_Rate, use="pairwise.complete.obs")
cor(Income$Per_Capita_Income,Binned_Data_Final$SOL_Advance_Rate, use="pairwise.complete.obs")

cor(Income$Median_Household_Income,Binned_Data_Final$SOL_Advance_Rate, use="pairwise.complete.obs")

cor(Income$Median_Family_Income,Binned_Data_Final$SOL_Advance_Rate, use="pairwise.complete.obs")

cor(Income$Number_Of_Households,Binned_Data_Final$SOL_Advance_Rate, use="pairwise.complete.obs")

cor(Income$Population,Binned_Data_Final$SOL_Advance_Rate, use="pairwise.complete.obs")


scoremax <- max(score_data$TotalScore, na.rm = TRUE)

scoremin = min(score_data$TotalScore,  na.rm = TRUE)


scoreinterval = (scoremax - scoremin)/10

TotalScoreRank = cut(score_data$TotalScore, c(-Inf, scoremin+scoreinterval, scoremin+scoreinterval*2, scoremin+scoreinterval*3,scoremin+scoreinterval*4,scoremin+scoreinterval*5,scoremin+scoreinterval*6,scoremin+scoreinterval*7,scoremin+scoreinterval*8,scoremin+scoreinterval*9, Inf), labels=  c("1","2","3","4","5","6","7","8","9","10"))
score_data = mutate(score_data,TotalScoreRank)


write.csv(score_data,"score_data.csv")
