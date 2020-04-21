library(ggplot2)
library(usmap)

#plotting and finding correlation between severe housing issues and SOL pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$Severe_Housing_Issues, y=binned_data_final$`SOL PassRate`))+ geom_point()
cor(binned_data_final$Severe_Housing_Issues,binned_data_final$`SOL PassRate`)

#plotting and finding correlation between severe housing issues and SOL advanced pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$Severe_Housing_Issues, y=binned_data_final$`SOL AdvPassRate`))+ geom_point()
cor(binned_data_final$Severe_Housing_Issues,binned_data_final$`SOL PassRate`)

#plotting and finding correlation between child mortality and SOL pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$Child_Mortality, y=binned_data_final$`SOL PassRate`))+ geom_point()
cor(binned_data_final$Child_Mortality,binned_data_final$`SOL AdvPassRate`)

#plotting and finding correlation between child mortality and SOL advanced pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$Child_Mortality, y=binned_data_final$`SOL AdvPassRate`))+ geom_point()
cor(binned_data_final$Child_Mortality,binned_data_final$`SOL AdvPassRate`)

##mapping
data <- data.frame(county = binned_data_final$County, values = binned_data_final$Severe_Housing_Issues)

va <- us_map(include = "VA",regions = "counties")
dataset <- cbind(binned_data_final$County,binned_data_final$Severe_Housing_Issues)
x <- data.frame(dataset)


fippy <- fips("VA", county = x$X1)
new_x<- data.frame(x,fippy)
newnew_x <- data.frame(new_x[,c(2,3)])
names(newnew_x) <- c("vals","fips")

plot_usmap(include= "VA", regions = "counties", data= newnew_x, values="vals")+scale_fill_discrete(name="Severe Housing Issues")+theme(legend.position="bottom")
