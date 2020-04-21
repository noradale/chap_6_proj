
library(ggplot2)
library(usmap)

#plotting and finding correlation between severe housing issues and SOL pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$WhiteBlackSeg, y=binned_data_final$SOL_Pass_Rate))+ geom_point()
cor(binned_data_final$WhiteBlackSeg,binned_data_final$SOL_Pass_Rate)


library(ggpubr)
ggscatter(binned_data_final, x = "WhiteBlackSeg", y = "SOL_Pass_Rate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "White Black Segregation", ylab = "Avg SOL Pass Rate")

#plotting and finding correlation between severe housing issues and SOL advanced pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$WhiteBlackSeg, y=binned_data_final$SOL_Advance_Rate))+ geom_point()
cor(binned_data_final$WhiteBlackSeg,binned_data_final$SOL_Advance_Rate)

#plotting and finding correlation between child mortality and SOL pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$WhiteNonWhiteSeg, y=binned_data_final$SOL_Pass_Rate))+ geom_point()
cor(binned_data_final$WhiteNonWhiteSeg,binned_data_final$SOL_Pass_Rate)

#plotting and finding correlation between child mortality and SOL advanced pass rate
ggplot(data=binned_data_final, aes(x=binned_data_final$WhiteNonWhiteSeg, y=binned_data_final$SOL_Advance_Rate))+ geom_point()
cor(binned_data_final$WhiteNonWhiteSeg,binned_data_final$SOL_Advance_Rate)

##mapping
data <- data.frame(county = binned_data_final$County, values = binned_data_final$WhiteBlackSeg)

va <- us_map(include = "VA",regions = "counties")
loco <- unique(va$county)
dataset <- cbind(loco,binned_data_final$WhiteBlackBin)
x <- data.frame(dataset)


fippy <- fips("VA", county = x$loco)
new_x<- data.frame(x,fippy)
newnew_x <- data.frame(new_x[,c(2,3)])
names(newnew_x) <- c("vals","fips")

newnew_x$vals <- factor(newnew_x$vals, levels = c("Very Low", "Low", "Moderate", "High", "Very High", "NA"))
plot_usmap(include= "VA", regions = "counties", data= newnew_x, values="vals")+ scale_fill_manual(values = c("thistle1", "rosybrown1", "lightsalmon1", "salmon3", "brown4", "gray30"), 
       name = "White-Black Segregation", guide = guide_legend(reverse = )) + theme(legend.position = "right") 

