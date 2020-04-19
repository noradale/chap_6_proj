names(WhiteNW) <- c("County", "Percent_Segregated")
names(WhiteBlack) <- c("County", "Percent_Segregated")


library(maps)
library(mapproj)

Seg <- WhiteNW$Percent_Segregated
cut(Seg, c(-Inf, 10, 20, 30, 40, Inf), labels=  c("Very Low","Low","Moderate","High","Very High"))
WhiteNW$BinSeg <- cut(Seg, c(-Inf, 10, 20, 30, 40, Inf), labels=  c("Very Low","Low","Moderate","High","Very High"))

library(ggplot2)
ggplot(WhiteNW, aes(x = County, fill = BinSeg)) +
  theme_bw() +
  geom_bar() +
  labs( y = "Segregation", x = "County") +
  scale_fill_discrete(name = 'WhiteNW', h = c(360,180) + 25, c = 100, l=65, h.start = 0,  direction = 1, na.value = "grey50") 

library(maps)
library(mapproj)


