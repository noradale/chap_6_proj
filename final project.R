library(ggplot2)
library(usmap)

binned_data_final <- transform(binned_data_final, Child_Mortality = as.numeric(Child_Mortality))

#plotting and finding correlation between severe housing issues and SOL pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$Severe_Housing_Issues, y=binned_data_final$SOL.PassRate))+ geom_point()
cor(binned_data_final$Severe_Housing_Issues,binned_data_final$SOL.PassRate)

#plotting and finding correlation between severe housing issues and SOL advanced pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$Severe_Housing_Issues, y=binned_data_final$SOL.AdvPassRate))+ geom_point()
cor(binned_data_final$Severe_Housing_Issues,binned_data_final$SOL.AdvPassRate)

#plotting and finding correlation between child mortality and SOL pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$Child_Mortality, y=binned_data_final$SOL.PassRate))+ geom_point()
cor(binned_data_final$Child_Mortality, binned_data_final$SOL.PassRate, use= "pairwise.complete.obs")

#plotting and finding correlation between child mortality and SOL advanced pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$Child_Mortality, y=binned_data_final$SOL.AdvPassRate))+ geom_point()
cor(binned_data_final$Child_Mortality,binned_data_final$SOL.AdvPassRate, use="pairwise.complete.obs")

##mapping severe housing issues
SHI_dataset <- cbind(binned_data_final$County,binned_data_final$SHBIN)
SHI <- data.frame(SHI_dataset)


SHI_fip <- fips("VA", county = SHI$X1)
new_SHI<- data.frame(SHI,SHI_fip)
final_SHI <- data.frame(new_SHI[,c(2,3)])
names(final_SHI) <- c("vals","fips")


final_SHI$vals <- factor(final_SHI$vals, levels = c("Very Low", "Low", "Moderate", "High", "Very High", "NA"))

plot_usmap(include= "VA", regions = "counties", data= final_SHI, values="vals")+ scale_fill_manual(values = c("thistle1", "rosybrown1", "lightsalmon1", "salmon3", "brown4", "gray30"), name = "Severe Housing Issues", guide = guide_legend(reverse = FALSE), na.value="gray")+
  theme(legend.position = "right") 



##mapping child mortality
CM_dataset <- cbind(binned_data_final$County,binned_data_final$CMBIN)
CM <- data.frame(CM_dataset)


CM_fip <- fips("VA", county = CM$X1)
new_CM<- data.frame(CM,CM_fip)
final_CM <- data.frame(new_CM[,c(2,3)])
names(final_CM) <- c("vals","fips")


final_CM$vals <- factor(final_CM$vals, levels = c("Very Low", "Low", "Moderate", "High", "Very High", "NA"))

plot_usmap(include= "VA", regions = "counties", data= final_CM, values="vals")+ scale_fill_manual(values = c("thistle1", "rosybrown1", "lightsalmon1", "salmon3", "brown4", "gray30"), name = "Child Mortality", guide = guide_legend(reverse = FALSE), na.value="gray")+
  theme(legend.position = "right") 


