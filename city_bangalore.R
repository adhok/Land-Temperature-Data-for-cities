library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(scales)
library(data.table)

train <- fread("GlobalLandTemperaturesByCity.csv")
indianCity <- subset(train,Country=='India')
indianCity<-na.omit(indianCity)
#####################################################
chinesecity <- subset(train,Country=='China')
chinesecity <- na.omit(chinesecity)
chinesecity$dt <- as.POSIXct(chinesecity$dt,format='%Y-%m-%d')
chinesecity$Month <- month(chinesecity$dt)
chinesecity$Year <- year(chinesecity$dt)

###########################################################
australiancity <- subset(train,Country=='Australia')
australiancity <- na.omit(australiancity)
australiancity$dt <- as.POSIXct(australiancity$dt,format='%Y-%m-%d')
australiancity$Month <- month(australiancity$dt)
australiancity$Year <- year(australiancity$dt)
##################################################################


indianCity$dt <- as.POSIXct(indianCity$dt,format='%Y-%m-%d')
indianCity$Month <- month(indianCity$dt)
indianCity$Year <- year(indianCity$dt)

bangalore <- subset(indianCity,City=='Bangalore')
bangalore <- subset(bangalore,Year %in% c(1796,1846,1896,1946,1996,2012))
delhi <- subset(indianCity,City=='Delhi')
delhi <- subset(delhi,Year %in% c(1796,1846,1896,1946,1996,2012))
hyder <- subset(indianCity,City=='Hyderabad')
hyder <- subset(hyder,Year %in% c(1796,1846,1896,1946,1996,2012))
agartala <- subset(indianCity,City=='Agartala')
agartala <- subset(agartala,Year %in% c(1796,1846,1896,1946,1996,2012))
sydney <- subset(australiancity,City=='Sydney')
sydney <- subset(sydney,Year %in% c(1796,1846,1896,1946,1996,2012))
adelaide <- subset(australiancity,City=='Adelaide')
adelaide <- subset(adelaide,Year %in% c(1796,1846,1896,1946,1996,2012))
wollongong <- subset(australiancity,City=='Wollongong')
wollongong <- subset(wollongong,Year %in% c(1796,1846,1896,1946,1996,2012))
canberra <- subset(australiancity,City=='Canberra')
canberra <- subset(canberra , Year %in% c(1796,1846,1896,1946,1996,2012))






p_bangalore <- ggplot(bangalore, aes(x=(Month), y=AverageTemperature, color=as.factor(Year))) +
  geom_smooth(se=FALSE,fill=NA, size=2) +
  theme_light(base_size=20) +
  xlab("Month")+
  ylab("Average Temperature") +
  scale_color_discrete("") +
  ggtitle("Average Temperature Over the Years in Bangalore") +
  theme(plot.title=element_text(size=18))
ggsave('bangalore.png')
p_delhi <- ggplot(delhi, aes(x=(Month), y=AverageTemperature, color=as.factor(Year))) +
  geom_smooth(se=FALSE,fill=NA, size=2) +
  theme_light(base_size=20) +
  xlab("Month")+
  ylab("Average Temperature") +
  scale_color_discrete("") +
  ggtitle("Average Temperature Over the Years in Delhi") +
  theme(plot.title=element_text(size=18))
ggsave('delhi.png')
p_hyder <- ggplot(hyder, aes(x=(Month), y=AverageTemperature, color=as.factor(Year))) +
  geom_smooth(se=FALSE,fill=NA, size=2) +
  theme_light(base_size=20) +
  xlab("Month")+
  ylab("Average Temperature") +
  scale_color_discrete("") +
  ggtitle("Average Temperature Over the Years in Hyderabad") +
  theme(plot.title=element_text(size=18))
ggsave('hyderabad.png')
p_agartala <- ggplot(agartala, aes(x=(Month), y=AverageTemperature, color=as.factor(Year))) +
  geom_smooth(se=FALSE,fill=NA, size=2) +
  theme_light(base_size=20) +
  xlab("Month")+
  ylab("Average Temperature") +
  scale_color_discrete("") +
  ggtitle("Average Temperature Over the Years in Agartala") +
  theme(plot.title=element_text(size=18))
ggsave('agartala.png')
p_sydney <- ggplot(sydney, aes(x=(Month), y=AverageTemperature, color=as.factor(Year))) +
  geom_smooth(se=FALSE,fill=NA, size=2) +
  theme_light(base_size=20) +
  xlab("Month")+
  ylab("Average Temperature") +
  scale_color_discrete("") +
  ggtitle("Average Temperature Over the Years in Sydney") +
  theme(plot.title=element_text(size=18))
ggsave('Sydney.png')
p_adelaide <- ggplot(adelaide, aes(x=(Month), y=AverageTemperature, color=as.factor(Year))) +
  geom_smooth(se=FALSE,fill=NA, size=2) +
  theme_light(base_size=20) +
  xlab("Month")+
  ylab("Average Temperature") +
  scale_color_discrete("") +
  ggtitle("Average Temperature Over the Years in Adelaide") +
  theme(plot.title=element_text(size=18))
ggsave('Adelaide.png')
p_wollongong <- ggplot(wollongong, aes(x=(Month), y=AverageTemperature, color=as.factor(Year))) +
  geom_smooth(se=FALSE,fill=NA, size=2) +
  theme_light(base_size=20) +
  xlab("Month")+
  ylab("Average Temperature") +
  scale_color_discrete("") +
  ggtitle("Average Temperature Over the Years in Wollongong") +
  theme(plot.title=element_text(size=18))
ggsave('Wollongong.png')
#############################################################################
##########################################################################33#




library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(animation)
library(data.table)

city <- fread("GlobalLandTemperaturesByCity.csv")
city <- na.omit(city)
city$year <- substr(city$dt,1,4)

city <- subset(city,Country=='India')
australia <- subset(city, Country=='Australia')

bangalore <- subset(city,City=='Bangalore')
hyderabad <- subset(city,City=='Hyderabad')
delhi <- subset(city,City=='Delhi')
agartala <- subset(city,City=='Agartala')
adelaide <- subset(australia,City=='Adelaide')
sydney <- subset(australia, City == 'Sydney')
wollongong <- subset(australia,City == 'Wollongong')

allYears <- unique(wollongong$year)


saveGIF({
  for (thisYear in allYears) {
    
    thisYearCity <- wollongong[wollongong$year == thisYear, ]
    
    m <- ggplot(thisYearCity, aes(x=AverageTemperature))
    m <- m + ggtitle(paste("Wollongong - Average Temperature Histogram -", thisYear))
    m <- m + geom_density(alpha=.5, fill = "gray")
    m <- m + xlim(c(-40, 50))
    m <- m + ylim(c(0, 0.1))
    m <- m + geom_vline(aes(xintercept=mean(AverageTemperature, na.rm=T)),   # Ignore NA values for mean
                        color="red", linetype="dashed", size=1)
    m <- m + geom_vline(aes(xintercept=median(AverageTemperature, na.rm=T)),   # Ignore NA values for mean
                        color="blue", linetype="dashed", size=1)
    
    
    print(m)
    
  } 
  
}, interval = 0.5, movie.name = "tempDensity_wollongong.gif", ani.width = 800, ani.height = 600 )
##########################################################################################################
##########################################################################################################
##########################################################################################################
library(ggplot2)
library(ggmap)
library(maps)
library(data.table)
library(animation)

GlobalLandTemperaturesByCity <- fread("GlobalLandTemperaturesByCity.csv")

#Create some useful data points
GlobalLandTemperaturesByCity$dt<-as.Date(GlobalLandTemperaturesByCity$dt,"%Y-%m-%d")
GlobalLandTemperaturesByCity$lat<-as.numeric(gsub("N|E|S|W", "",GlobalLandTemperaturesByCity$Latitude))*ifelse(grepl("S",GlobalLandTemperaturesByCity$Latitude),-1,1)
GlobalLandTemperaturesByCity$long<-as.numeric(gsub("N|E|S|W", "", GlobalLandTemperaturesByCity$Longitude))*ifelse(grepl("W",GlobalLandTemperaturesByCity$Longitude),-1,1)
GlobalLandTemperaturesByCity$Month<-as.numeric(format(GlobalLandTemperaturesByCity$dt,"%m"))
GlobalLandTemperaturesByCity$Year<-as.numeric(format(GlobalLandTemperaturesByCity$dt,"%Y"))
GlobalLandTemperaturesByCity <- na.omit(GlobalLandTemperaturesByCity)



years <- c(1796,1846,1896,1946,1996,2012)
saveGIF({
  for(y in years)
  {
  for (i in 1:12) {
    m<-ggplot()+borders("world",colour="grey75",fill="black")+
      theme(panel.background=element_rect(fill = "gray93"))+
      geom_point(data=subset(GlobalLandTemperaturesByCity, Month==i,Year=y),aes(x=long,y=lat,colour=AverageTemperature),size=3)+
      scale_colour_gradient(low="yellow",high ="red")+
      ggtitle(paste("Average Annual Increase in Temperature in month",i ,'and Year',y))+
      labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
    print(m)
  }
    
  } 
  
}, interval = 1, movie.name = "world_temp_change.gif", ani.width = 800, ani.height = 600 )






















ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==1),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - January")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==2),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - February")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==3),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - March")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==4),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - April")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==5),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - May")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==6),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - June")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==7),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - July")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==8),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - August")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==9),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - September")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==10),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - October")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==11),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - November")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")

ggplot()+borders("world",colour="grey75",fill="black")+
  theme(panel.background=element_rect(fill = "gray93"))+
  geom_point(data=subset(meta.city, Month==12),aes(x=long,y=lat,colour=year.coef),size=3)+
  scale_colour_gradient(low="yellow",high ="red")+
  ggtitle("Average Annual Increase in Temperature - December")+
  labs(colour='Average Annual \nTemperature Increase (°C)')+xlab("Longitude")+ylab("Latitude")
