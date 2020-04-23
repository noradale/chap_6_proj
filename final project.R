library(ggplot2)
library(usmap)
setwd("C:/Users/Disha/Desktop/SYS2202")
binned_data_final <- read.csv("binned_data_final.csv", header=T, na.strings=c("NA","NA"))
#plotting and finding correlation between severe housing issues and SOL pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$Severe_Housing_Issues, y=binned_data_final$`SOL PassRate`))+ geom_point()
cor(binned_data_final$Severe_Housing_Issues,binned_data_final$`SOL PassRate`)

#plotting and finding correlation between severe housing issues and SOL advanced pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$Severe_Housing_Issues, y=binned_data_final$`SOL AdvPassRate`))+ geom_point()
cor(binned_data_final$Severe_Housing_Issues,binned_data_final$`SOL AdvPassRate`)

#plotting and finding correlation between child mortality and SOL pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$Child_Mortality, y=binned_data_final$`SOL PassRate`))+ geom_point()
binned_data_final <- transform(binned_data_final, Child_Mortality = as.numeric(Child_Mortality))
cor(binned_data_final$Child_Mortality, binned_data_final$SOL.PassRate, use= "pairwise.complete.obs")

#plotting and finding correlation between child mortality and SOL advanced pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$Child_Mortality, y=binned_data_final$`SOL AdvPassRate`))+ geom_point()
cor(binned_data_final$Child_Mortality,binned_data_final$SOL.AdvPassRate, use="pairwise.complete.obs")

##mapping severe housing issues
data <- data.frame(county = binned_data_final$County, values = binned_data_final$SHBIN)

va <- us_map(include = "VA",regions = "counties")
dataset <- cbind(binned_data_final$County,binned_data_final$SHBIN)
x <- data.frame(dataset)


fippy <- fips("VA", county = x$X1)
new_x<- data.frame(x,fippy)
newnew_x <- data.frame(new_x[,c(2,3)])
names(newnew_x) <- c("vals","fips")


newnew_x$vals <- factor(newnew_x$vals, levels = c("NA", "Very High", "High", "Moderate", "Low", "Very Low"))

plot_usmap(include= "VA", regions = "counties", data= newnew_x, values="vals")+ scale_fill_manual(values = c("thistle4", "thistle3", "thistle", "thistle2", "thistle1"), name = "Severe Housing Issues", guide = guide_legend(reverse = TRUE))+
  theme(legend.position = "right") 
