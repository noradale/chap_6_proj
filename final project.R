library(ggplot2)
library(usmap)

#plotting and finding correlation between severe housing issues and SOL pass rate
ggplot(data=binned_data, aes(x=binned_data$Severe_Housing_Issues, y=binned_data$`SOL PassRate`))+ geom_point()
cor(binned_data$Severe_Housing_Issues,binned_data$`SOL PassRate`)

#plotting and finding correlation between severe housing issues and SOL advanced pass rate
ggplot(data=binned_data, aes(x=binned_data$Severe_Housing_Issues, y=binned_data$`SOL AdvPassRate`))+ geom_point()
cor(binned_data$Severe_Housing_Issues,binned_data$`SOL PassRate`)

#plotting and finding correlation between child mortality and SOL pass rate
ggplot(data=binned_data, aes(x=binned_data$Child_Mortality, y=binned_data$`SOL PassRate`))+ geom_point()
cor(binned_data$Child_Mortality,binned_data$`SOL AdvPassRate`)

#plotting and finding correlation between child mortality and SOL advanced pass rate
ggplot(data=binned_data, aes(x=binned_data$Child_Mortality, y=binned_data$`SOL AdvPassRate`))+ geom_point()
cor(binned_data$Child_Mortality,binned_data$`SOL AdvPassRate`)

##mapping
data <- data.frame(county = binned_data$County, values = binned_data$Severe_Housing_Issues)

va <- us_map(include = "VA",regions = "counties")
loco <- unique(va$county)
dataset <- cbind(loco,binned_data$Severe_Housing_Issues)
x <- data.frame(dataset)


fippy <- fips("VA", county = x$loco)
new_x<- data.frame(x,fippy)
newnew_x <- data.frame(new_x[,c(2,3)])
names(newnew_x) <- c("vals","fips")

plot_usmap(include= "VA", regions = "counties", data= newnew_x, values="vals")+scale_fill_discrete(name="Severe Housing Issues")+theme(legend.position="bottom")
