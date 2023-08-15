library(purrr)
library(readr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(fuzzyjoin)
library(readr)
#a<- read_csv("./02_Data/06_GEE_Data_extractions/Sentinel_1/VH_1_month_temporalfilter/Sites_VH_1_month_temporalfilter.csv")


#reshape2::melt(a,id=cols)

all_files<- list.files("./02_Data/02_GEE_Data_extractions/",recursive = T,
                       pattern = ".csv$",full.names = T)
all_files<- grep("Landsat_8|Sentinel_1",all_files,invert = F,value = T)
all_files<- grep("shadow|no_data_mask|layover",all_files,invert = T,value = T)
VV_files <- grep("VV",all_files,invert = F,value = T)
VH_files <- grep("VH",all_files,invert = F,value = T)

cols<- c("system:index","Altitude_a","Aspect","Station na","Altitude")

S1<- map_df(all_files, ~melt(read_csv(.x),id=cols)%>% 
              mutate(Parameter=gsub("Sites_|.csv","",basename(.x))) 
            )

# VH<- map_df(VH_files, ~melt(read_csv(.x),id=cols)%>% 
#               mutate(Sigma=gsub("Sites_|.csv","",basename(.x))) 
#             )
df_process <- function(df){
  df$"system:index" <- NULL
  names(df)[which(names(df)=="Station na")] <- "Station"
  df$value <- as.numeric(df$value)
  df$Parameter <- as.factor(df$Parameter)
  df <- df[!is.na(df$value),]
  df$S1 <- df$variable
  df$S1 <-unlist(lapply((lapply(strsplit(as.character(df$S1),"_"),"[",1:10)), function(x){paste0(x,collapse = "_")}))
  df$Date <- unlist(lapply(strsplit(as.character(df$variable),"_"),"[",5))
  df$Date <- as.POSIXct(df$Date,tz="UTC",format=("%Y%m%dT%H%M%OS"))
  df$Orbit <- unlist(lapply(strsplit(as.character(df$variable),"_"),"[",10))
  df$variable <- NULL
  return(df)
}
S1 <- df_process(S1)
#VV <- df_process(VV)

#View(cbind(VV %>% group_by(Sigma,`Station na`) %>% summarise(n=n())
S1 %>% filter(Parameter=="VV")%>% group_by(Station,Orbit) %>% summarise(n=n())
S1 %>% filter(Parameter=="VH")%>% group_by(Station,Orbit) %>% summarise(n=n())


# VH has more data than VV, it's not normal
setdiff(as.character(unlist(S1 %>% filter(Parameter=="VV",Station=="B1",Orbit==15) %>% mutate(Date=paste0(Date,"")) %>% select(Date))), 
        as.character(unlist(S1 %>% filter(Parameter=="VH",Station=="B1",Orbit==15) %>% mutate(Date=paste0(Date,"")) %>% select(Date))))

setdiff(as.character(unlist(S1 %>% filter(Parameter=="VH",Station=="B1",Orbit==15) %>% mutate(Date=paste0(Date,"")) %>% select(Date))),
        as.character(unlist(S1 %>% filter(Parameter=="VV",Station=="B1",Orbit==15) %>% mutate(Date=paste0(Date,"")) %>% select(Date))))
# VH has the date "2020-07-30 17:15:27" in some points, visualizing the image in gee
# we can see than the VV is corrupted, let remove this day

S1 <- S1 %>% mutate(date2=as.character(Date))%>% filter(date2!="2020-07-30 17:15:27") %>% select(-c(date2))

Max_orbit_by_Station_VV<- S1 %>% filter(Parameter=="VV")%>% group_by(Station,Orbit) %>% 
  summarise(n=n()) %>%  
  group_by(Station)%>%
  filter(n == max(n))#['n'].max()

unique(Max_orbit_by_Station_VV$Orbit) # all has the same orbit

Max_orbit_by_Station_VH<- S1 %>% filter(Parameter=="VH") %>% group_by(Station,Orbit) %>% 
  summarise(n=n()) %>%  
  group_by(Station)%>%
  filter(n == max(n))#['n'].max()
unique(Max_orbit_by_Station_VH$Orbit)

S1_unmelt <-  (reshape2::dcast(S1, Altitude_a+Aspect+Station+Altitude+S1+Date+Orbit  ~ Parameter)
       )

# VH_Sigma_plots <- function(data,station){
#   Max_n_orbit<- Max_orbit_by_Station_VH[Max_orbit_by_Station_VH$`Station na`==station,]$Orbit
#   print(data <- data[data$`Station na`==station & data$Orbit==Max_n_orbit,]%>% 
#   ggplot(aes(Date, value, group = Sigma, color = Sigma)) +
#   geom_smooth(method = "loess", se = F, n = 50,span=0.39) +
#   geom_point(size = 0.1) +
#   labs(y = expression(sigma^0), size = 3, title = paste0("S1 VH, Station ",station,", Orbit ",Max_n_orbit)) +
#     scale_color_manual(
#       breaks = c("VH","VH_multilook","VH_6_month_temporalfilter","VH_12_month_temporalfilter","VH_24_month_temporalfilter"),
#       values = c("VH" = "royalblue4",
#                  "VH_multilook" = "gold1",
#                  "VH_6_month_temporalfilter" = "seagreen2",
#                  "VH_12_month_temporalfilter" = "deeppink", 
#                  "VH_24_month_temporalfilter" = "forestgreen"),
#       labels = c("VH" = "VH",
#                  "VH_multilook" = "VH multilook",
#                  "VH_6_month_temporalfilter" = "VH ± 6 month temporalfilter",
#                  "VH_12_month_temporalfilter" = "VH ± 12 month temporalfilter",
#                  "VH_24_month_temporalfilter" = "VH ± 24 month temporalfilter"
#                  )
#     ))
# 
# 
# }
# 
# VV_Sigma_plots <- function(data,station){
#   # Filter data by the max orbit data
#   Max_n_orbit<- Max_orbit_by_Station_VV[Max_orbit_by_Station_VV$`Station na`==station,]$Orbit
#   print(data <- data[data$`Station na`==station & data$Orbit==Max_n_orbit,]%>% 
#           ggplot(aes(Date, value, group = Sigma, color = Sigma)) +
#           geom_smooth(method = "loess", se = F, n = 50,span=0.39) +
#           geom_point(size = 0.1) +
#           labs(y = expression(sigma^0), size = 3, title = paste0("S1 VV, Station ",station,", Orbit ",Max_n_orbit)) +
#           scale_color_manual(
#             breaks = c("VV","VV_multilook","VV_6_month_temporalfilter","VV_12_month_temporalfilter","VV_24_month_temporalfilter"),
#               values = c("VV" = "royalblue4",
#                          "VV_multilook" = "gold1",
#                          "VV_6_month_temporalfilter" = "seagreen2", 
#                        "VV_12_month_temporalfilter" = "deeppink",
#                        "VV_24_month_temporalfilter" = "forestgreen"),
#             labels = c("VV" = "VV",
#                        "VV_multilook" = "VV multilook",
#                        "VV_6_month_temporalfilter" = "VV ± 6 month temporalfilter",
#                        "VV_12_month_temporalfilter" = "VV ± 12 month temporalfilter",
#                        "VV_24_month_temporalfilter" = "VV ± 24 month temporalfilter"
#                        )
#           ))
#   
#   
# }
# 
# VV_Sigma_plots(VV %>% filter(Sigma %in% c("VV","VV_multilook", "VV_6_month_temporalfilter","VV_12_month_temporalfilter","VV_24_month_temporalfilter")),"F4")
# VV_Sigma_plots(VV,"M2")
# Stations <- unique(VV$`Station na`)
# output_plots_dir <- "./04_Plots/S1_filters_D"
# if(!dir.exists(output_plots_dir))(dir.create(output_plots_dir))
# 
# for(i in Stations){
#   jpeg(paste0(output_plots_dir,"/",i,"_VV_Station.jpg"), 
#        width =1600, height = 1000,quality=100,res=250)
#   VV_Sigma_plots(VV%>% filter(Sigma %in% c("VV","VV_multilook", "VV_6_month_temporalfilter","VV_12_month_temporalfilter","VV_24_month_temporalfilter")),i)
#   dev.off()
#   
#   jpeg(paste0(output_plots_dir,"/",i,"_VH_Station.jpg"), 
#        width =1600, height = 1000,quality=100,res=250)
#   VH_Sigma_plots(VH%>% filter(Sigma %in% c("VH","VH_multilook", "VH_6_month_temporalfilter","VH_12_month_temporalfilter","VH_24_month_temporalfilter")),i)
#   dev.off()
# }
# 
#  VV %>% filter(`Station na`=="B1") %>% ggplot(aes(Date, value, group = Sigma, color = Sigma))+
#   geom_smooth(method = "loess", se = FALSE, n = 100)


#---------- SMC data ---------- 
 
local<- locale(
   date_names = "en",
   date_format = "%AD",
   time_format = "%AT",
   decimal_mark = ".",
   grouping_mark = ",",
   tz ="Europe/Rome",
   encoding = "UTF-8",
   asciify = FALSE
 )



all_GTD <- read_delim("./02_Data/03_Stations/Ground_truth_data.csv",
                       locale=local,delim =",")
 # Set time tolerance
 tolerance <- 3600 # in seconds
 
 # join two data frames with time tolerance
 df_joined<- fuzzy_join(S1_unmelt, all_GTD, 
                        by = c("Date"),
                        match_fun = function(x, y) abs(difftime(x,y,units = "secs")) <= tolerance)
 
 # Rename .y by "_obs", and ".x" by "_pred"
 names(df_joined)[grep(".y",names(df_joined))]<-
   gsub(".y","_GTD",grep(".y",names(df_joined),value = T))
 names(df_joined)[grep(".x",names(df_joined))]<-
   gsub(".x","_S1",grep(".x",names(df_joined),value = T))
 
 str(df_joined)
 
 # The dataframe has been joined only with Date,
 # but its necessary the same pred and obs station in the join 
 df_joined <- df_joined %>%
   # filter by stations
   filter(Station_S1  == Station_GTD )%>%
   # As the join of the SMC pred and obs was maded with +- 3600 seconds of tolerance 
   # each pred. has multiples joins with obs. 
   # If there are more than one GTD to per each prediction, the mean and sd of the GTD is computed
   # per day in the obs values
   dplyr::group_by(Date_S1,Station_S1 ,Orbit) %>% # Date_pred
   dplyr::mutate(sd_obs_02=sd(wc_02_av,na.rm =T),obs_02 =mean(wc_02_av, na.rm =T),
                 sd_obs_05=sd(wc_05_av, na.rm =T),obs_05 =mean(wc_05_av, na.rm =T),
                 Date_GTD=mean(Date_GTD))  %>% 
   # As we will work with an average of the obs, it is necessary to eliminate 
   # multiple records for each day.  I eliminate duplicate rows obs data per day. 
   # To do this, the columns that correspond to hourly records are eliminated, then 
   # the non-repeated rows are kept.
   select(-c("swc_wc_a_02_avg","swc_wc_a_05_avg",
             "swc_wc_b_02_avg","swc_wc_b_05_avg",
             "swc_wc_c_02_avg","swc_wc_c_05_avg","wc_05_av","wc_02_av",
             "swc_wc_02_avg","swc_wc_05_avg"
   )) %>% 
   distinct() # remove duplicates (created in the day agrupation)
 
 # df_joined %>% filter(Station=="B1",Orbit=="15")%>%  
 #   ggplot(aes(x=Date_GTD,y=obs_05))+geom_line()+
 #   geom_point(aes(x=Date_GTD,y=VV))
# ggplot(df_joined %>% filter(Parameter=="angle"),aes(x=value))+geom_histogram(bins=200)
 

 View( df_joined %>% arrange(Station_S1,Date_S1))

# Add sites properties
all_files<- list.files("./02_Data/02_GEE_Data_extractions/",recursive = T,
                        pattern = ".csv$",full.names = T)
all_files<- grep("Sentinel_1|Landsat_8|To_compare_joins",all_files,invert = T,value = T)

cols<- c("Altitude_a","Altitude","Aspect", "Station.na", "Land.use")

for (file in all_files) {
  # file <- all_files[[2]]
  # file$
  # Read the CSV file
  data <- read.csv(file)
  data[,c("system.index","Altitude_a","Altitude","Aspect", "Land.use")] <- NULL
  names(data) <- c(gsub(".csv|Sites_","",basename(file)),"Station")
  # Merge the data with the existing merged data
  if (file == all_files[[1]]) {
    merged_data <- data
  } else {
    merged_data <- merge(merged_data, data, by = "Station", all = TRUE)
  }
}


all_data <- merge(df_joined %>% select(-c("elevation","latitude","longitude")),merged_data,by.x="Station_S1",by.y="Station")
head(all_data)

all_data[,c("swc_wc_a_02_avg","swc_wc_a_05_avg" ,"swc_wc_b_02_avg","swc_wc_b_05_avg",
            "shadow","no_data_mask","l_GTDover","swc_wc_c_02_avg","swc_wc_c_05_avg",
            "swc_wc_02_avg","swc_wc_05_avg","Station_GTD","Altitude_a","Aspect", 
            "Altitude")] <- NULL
output_dir <- "./02_Data/04_Data_to_model/"
if(!dir.exists(output_dir))dir.create(output_dir,recursive = T)
write.csv(all_data,paste0(output_dir,"all_data_V_C.csv"))
write.csv(all_data[complete.cases(all_data),],paste0(output_dir,"all_data_complete_V_C.csv"))

all_data <- read_csv(paste0(output_dir,"all_data.csv"))
# ---------


# Filter the data
 # filtered_data <- df_joined %>%
 #   filter(Station == "B1", Orbit == "15",Sigma=="VV" || Sigma=="VH")
 # scale = -30

# ggplot(filtered_data, aes(x=Date_GTD,y=obs_05, color = "SMC")) +
#    geom_line() +
#    #geom_point(data=filtered_data %>% filter(Sigma=="VV"),aes(y =value/scale, color = "VV"),size=0.8) +
#    geom_point(data=filtered_data %>% filter(Sigma=="VH"),aes(y =value/scale, color = "VH"),size=0.8) +
#   #scale_x_continuous(breaks = seq(0, 336, 24)) +
#    scale_y_continuous(sec.axis = sec_axis(~.*scale, name="Sigma 0")) +
#    labs(x = "Date", y = "SMC m3/m3", color = "SMC") +
#    scale_color_manual(values = c("orange2", "gray30","red"))

# df_joined %>%
#   filter(Sigma=="VH", Orbit == "15",Station == "B1") %>% ggplot(aes(x=obs_05,y=value))+geom_point()
#
#
# # Scatter plots
# library(ggpubr)
# data_filter_scatter<- df_joined %>%  filter(Sigma %in% c("VH","VV",  "VH_multilook", "VV_multilook","VV_3_month_temporalfilter","VH_3_month_temporalfilter",
#
#                                                                                        "VV_12_month_temporalfilter","VH_12_month_temporalfilter"))
# data_filter_scatter<- df_joined %>%  filter(Sigma %in% c("VV_multilook"))
#
# output_dir <- "./04_Plots/S1_filters_D/Scatter_plots/"
# if(!dir.exists(output_dir))dir.create(output_dir)
# Stations<- unique(df_joined$Station)
#
#
# for (i in Stations) {
# jpeg(paste0(output_dir,"Station_",i,".jpg"),
#      width = 800, height = 500,quality=100,res=150)
# print(
# ggscatter(data_filter_scatter %>%
#             filter( Orbit == "15",Station == i), x = "value", size = 0.3,
#           y = "obs_05",# points
#           add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
#           add.params = list(color = "royalblue4",size=0.3),
#           facet.by = "Sigma")+
#   stat_cor(aes(label = paste(..rr.label..,..r.label..,sep = "~`,`~")),
#            size=3,label.sep='\n')+
#   geom_abline(intercept=0, slope=1,col='red',lty=3)+
#   labs(x='Sigma 0', y='SMC',size=2)+ ggtitle(paste0("Station ",i))
# )
# dev.off()
# }
#
# output_dir <- "./04_Plots/S1_filters_D/Scatter_plots_selected_stations/"
# if(!dir.exists(output_dir))dir.create(output_dir)
# i= "S3"
# ggscatter(data_filter_scatter %>%
#             filter(Station == i), x = "value",  y = "obs_05",
#           add = "reg.line",palette = "jco",ylim = c(0, .5),
#           combine = TRUE)+
#   stat_cor(aes(label = paste(..rr.label..,..r.label..,sep = "~`,`~")),size=3,label.sep='\n', method = "spearman")+
#   geom_abline(intercept=0, slope=1,col='red',lty=3)+
#   labs(x='Sigma 0', y='SMC',size=2)+ ggtitle(paste0("Station ",i))
#
ggscatter(data_filter_scatter %>%
            filter(Station == i), x = "value",  y = "obs_05",
          add = "reg.line",palette =  get_palette("Dark2", 3),ylim = c(0, .6),
          color  = "Orbit",combine = TRUE,ellipse = T)+
  stat_cor(aes(color = Orbit,label = paste(..rr.label..,..r.label..,sep = "~`,`~")),
           size=3.5,label.sep='\n', method = "spearman",label.y = c(0.6),label.x = c(-5,-10,-16))+
  geom_abline(intercept=0, slope=1,col='red',lty=3)+
  labs(x='Sigma 0', y='SMC',size=2)+ ggtitle(paste0("Station ",i))



ggscatter(data_filter_scatter %>%
            filter(Station == i), x = "value",  y = "obs_05",
          add = "reg.line",palette =  get_palette("Dark2", 3),ylim = c(0, .5),
          color  = "Orbit",combine = TRUE,ellipse = T)+
  stat_cor(aes(color = Orbit,label = paste(..rr.label..,..r.label..,sep = "~`,`~")),
           size=3.5,label.sep='\n', method = "spearman",label.y = c(0.5),label.x = c(-9,-13,-20))+
  geom_abline(intercept=0, slope=1,col='red',lty=3)+
  labs(x='Sigma 0', y='SMC',size=2)+ ggtitle(paste0("Station ",i))
#
#
# for (i in Stations) {
#   jpeg(paste0(output_dir,"Station_",i,".jpg"),
#        width = 800, height =400,quality=100,res=150)
#   print(
#     ggscatter(data_filter_scatter %>%
#                 filter(Station == i), x = "value", size = 0.3,
#               y = "obs_05",# points
#               add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
#               add.params = list(color = "royalblue4",size=0.3),
#               facet.by = "Orbit")+
#       stat_cor(aes(label = paste(..rr.label..,..r.label..,sep = "~`,`~")),
#                size=3,label.sep='\n')+
#       geom_abline(intercept=0, slope=1,col='red',lty=3)+
#       labs(x='Sigma 0', y='SMC',size=2)+ ggtitle(paste0("Station ",i))
#   )
#   dev.off()
# }
#
#
names(all_data)
df_unmelt<- (reshape2::dcast(all_data, S1 + Altitude_a + Aspect + `Station na` + Altitude  + Date_S1 + Orbit + Date_GTD + Station_S1 + landuse + elevation + latitude + longitude + sd_obs_02 + obs_02 + sd_obs_05 + obs_05 ~ Sigma)
     )
df_unmelt<- (reshape2::dcast(all_data, Station_S1 + S1+ Date_S1+ Orbit + angle ~ value)
)

df_unmelt <- all_data

Pearson_correlation<- df_unmelt %>% group_by(Station_S1,Orbit) %>% filter(complete.cases(obs_05))%>%
  summarise(VV = cor(x=obs_05,y=VV,method = "pearson"),
            VV_multilook_3 = cor(x=obs_05,y=VV_multilook,method = "pearson"),
            VV_multilook_5 = cor(x=obs_05,y=VV_multilook_5,method = "pearson"),
            VV_1_month_temporalfilter= cor(x=obs_05,y=VV_1_month_temporalfilter,method = "pearson"),
            VV_3_month_temporalfilter = cor(x=obs_05,y=VV_3_month_temporalfilter,method = "pearson"),
            VV_6_month_temporalfilter = cor(x=obs_05,y=VV_6_month_temporalfilter,method = "pearson"),
            VV_12_month_temporalfilter = cor(x=obs_05,y=VV_12_month_temporalfilter,method = "pearson"),
            VV_24_month_temporalfilter = cor(x=obs_05,y=VV_24_month_temporalfilter,method = "pearson"),
            VH = cor(x=obs_05,y=VH,method = "pearson"),
            VH_multilook_3 = cor(x=obs_05,y=VH_multilook,method = "pearson"),
            VH_multilook_5 = cor(x=obs_05,y=VH_multilook_5,method = "pearson"),
            VH_1_month_temporalfilter= cor(x=obs_05,y=VH_1_month_temporalfilter,method = "pearson"),
            VH_3_month_temporalfilter = cor(x=obs_05,y=VH_3_month_temporalfilter,method = "pearson"),
            VH_6_month_temporalfilter = cor(x=obs_05,y=VH_6_month_temporalfilter,method = "pearson"),
            VH_12_month_temporalfilter = cor(x=obs_05,y=VH_12_month_temporalfilter,method = "pearson"),
            VH_24_month_temporalfilter = cor(x=obs_05,y=VH_24_month_temporalfilter,method = "pearson"),
            LCC=unique(landuse),
            Aspect = unique(Aspect),
            Elevation = unique(elevation))
# output_dir = "./03_Results/Sentinel_1_SMC_correlations/"
# if(!dir.exists(output_dir))dir.create(output_dir)
# write_excel_csv2(Pearson_correlation,paste0(output_dir,"Pearson_correlation.csv"))
# # -- Plots
# df_pearson_multilook<- melt(Pearson_correlation %>%select(LCC,Station,VV_multilook_3,VH_multilook_3),
#      c("Station","LCC"))
#
# df_pearson_multilook$variable <- gsub("_multilook_3","",df_pearson_multilook$variable)
#
# df_pearson_multilook %>%
#   ggplot(aes(x =Station,y=value,fill=variable))+ geom_boxplot() +
#   ggtitle("Pearson correlation at differents S1 orbits")+ylab("R")
#
# df_pearson_multilook<- melt(Pearson_correlation %>%select(LCC,Station,VV_multilook_3,VV_multilook_5),
#                             c("Station","LCC"))
# df_pearson_multilook$variable <- gsub("_"," ",df_pearson_multilook$variable)
# df_pearson_multilook$variable <- gsub("VV","",df_pearson_multilook$variable)
#
# df_pearson_multilook %>%
#   ggplot(aes(x =Station,y=value,fill=variable))+ geom_boxplot() +
#   ggtitle("Pearson correlation at differents S1 orbits")+ylab("R")
#
# # --
# Pearson_correlation %>% group_by(Station) %>%
#   summarise(VV_3_mean =mean(VV_multilook_3),
#             VV_5_mean =mean(VV_multilook_5),
#             VH_3_mean =mean(VH_multilook_3),
#             VH_5_mean =mean(VH_multilook_5))

# Best Stations
#
# write.csv2(melt(Pearson_correlation,c("Station","Orbit","LCC","Aspect","Elevation")) %>%
#   group_by(Station) %>% filter(variable=="VV_sigma0_multilook") %>%
#   summarise(VV = mean(value),LCC=unique(LCC),
#             Aspect = unique(Aspect),
#             Elevation = unique(Elevation)) %>%
#   filter(VV>0.3),"./03_Results/Sentinel_1_SMC_correlations/Best_Sations.csv")
#
# # Best Multilooks in VV
# Pearson_correlation %>% group_by(Station) %>% summarise(max = max(VV_multilook))%>% filter(max>0.5)
# Pearson_correlation %>% group_by(Station) %>% summarise(max = max(VV_multilook))%>% filter(max>0.5)
# # Best Multilooks in VH
# Pearson_correlation %>% group_by(Station) %>% summarise(max = max(VH_multilook))%>% filter(max>0.2)
#
# ########------------
#
# df_original <- data.frame(ID=c(1:10),Año=c(31:40),Categoria=rep(c("a","b"),5),
#                           valor_1=rnorm(10,10,1),valor_2=rnorm(10,20,1),valor_3=rnorm(10,100,1))
# cols_keep <- c("ID","Año","Categoria")
# df_melt <- melt(df_original,cols_keep)
#
# dcast(df_melt, ID+Año+Categoria ~ variable)

#-----------

Pearson_correlation<- df_unmelt %>% group_by(Station,Orbit) %>% filter(complete.cases(obs_05))%>%
  summarise(VV_sigma0_multilook = cor(x=obs_05,y=VV_sigma0_multilook,method = "pearson"),
            VH_sigma0_multilook   = cor(x=obs_05,y=VH_sigma0_multilook  ,method = "pearson"),
            VH_gamma0surf_multilook = cor(x=obs_05,y=VH_gamma0surf_multilook,method = "pearson"),
            VH_gamma0vol_multilook = cor(x=obs_05,y=VH_gamma0vol_multilook,method = "pearson"),
            VV_gamma0surf_multilook= cor(x=obs_05,y=VV_gamma0surf_multilook,method = "pearson"),
            VV_gamma0vol_multilook = cor(x=obs_05,y=VV_gamma0vol_multilook,method = "pearson"),
            LCC=unique(landuse),
            Aspect = unique(Aspect),
            Elevation = unique(elevation))

df_pearson_multilook<- melt(Pearson_correlation,
                            c("Station","Orbit","LCC","Aspect","Elevation"))

df_pearson_multilook$variable <- gsub("_multilook","",df_pearson_multilook$variable)
df_pearson_multilook$value <- as.numeric(df_pearson_multilook$value)
df_pearson_multilook %>%
  ggplot(aes(x =Station,y=value,fill=variable))+ geom_boxplot() +
  ggtitle("Pearson correlation at differents S1 orbits")+ylab("R")

dev.off()
