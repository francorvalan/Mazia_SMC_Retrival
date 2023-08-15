library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(caret)
library(h2o)
data <- read_csv("./02_Data/04_Data_to_model/all_data_V_C.csv")
# Remove MODIS NDVI
data$NDVI <- NULL

#data <- data[,c("","Station_S1","Orbit","VV_multilook","obs_05")]
#data <- data[complete.cases(data),]
data$month <- as.numeric(format(data$Date_GTD,"%m"))
data <- data %>% 
    filter(Station_S1%in%c("B1","B2","I1","M5","P1","I3","P3","S3"))  #%>% 
  filter(NDVI_L8<0.8) #%>% 
  filter( month<=10 & month>=5 )

#data <- data %>% filter(Station_S1%in%c("S3"))
  # %>% 
  filter( month<=12 & month>=5 )
ggscatter(data%>% 
            filter( month<=11 & month>=3 ), x = "VV_multilook",  y = "obs_05",
          add = "reg.line",size = 0.05,
          panel.labs.background = list(fill = "skyblue3", color = "black"),
          add.params = list(color = "royalblue4",size=0.03),
          palette =  get_palette("Dark2", 3),ylim = c(0, .5),
          facet.by = c("Orbit","Station_S1"))+
  stat_cor(aes(label = paste(..rr.label..,..r.label..,sep = "~`,`~"),digits = 1),
           size=2.3,label.sep='\n',digits = 1,r.digits = 1, method = "spearman",label.y = c(0.5))+
  geom_abline(intercept=0, slope=1,col='red',lty=3)+
  
  labs(x='VV Sigma 0', y='SMC m³/m³',size=2)#+ ggtitle(paste0("Station ",i))

data$landuse <- as.factor(data$landuse)
set.seed(31415)
ggplot(data,aes(x=Station_S1,y=angle,group=Station_S1,fill=Orbit))+geom_boxplot()

ggplot(data %>% mutate(Orbit =as.factor(Orbit)), aes(x = Station_S1, y = angle, fill = Orbit)) +
  geom_boxplot() +
  labs(x = "Stations", y = "Incidence Angle") +
  ggtitle("Box Plots Grouped by Station_S1 and Orbit")
