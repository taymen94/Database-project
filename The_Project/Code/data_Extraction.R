library(tidyverse)
library("plyr")

#reading the GDELT Data
s <- read.csv('C:/Users/mnari/Documents/kraya/SS-Corona/DBPRO/project/gdelt_test (1)/Jul19-April20-count-Selected-themes-country.csv', sep='|', colClasses=c("character","numeric","character",rep("numeric",2), rep("character",2), rep("numeric",2)))
#extracting each data for those countries : germany , USA and Italy
germany<-filter(s,countryCode=='GM')
write.csv(germany,file = 'C:/Users/mnari/Documents/kraya/SS-Corona/DBPRO/project/gdelt_test (1)/themes_countries/DEU.csv')
usa<- filter(s,countryCode=='US')
write.csv(usa,file = 'C:/Users/mnari/Documents/kraya/SS-Corona/DBPRO/project/gdelt_test (1)/themes_countries/USA.csv')
italy <- filter(s,countryCode=='IT')
write.csv(italy,file = 'C:/Users/mnari/Documents/kraya/SS-Corona/DBPRO/project/gdelt_test (1)/themes_countries/ITA.csv')
