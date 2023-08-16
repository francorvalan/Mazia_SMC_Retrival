library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(caret)
library(h2o)
library(ggpubr)
library(terra)
library(ggpubr)
library(randomForest)
library(tmap)
data <- read_csv("./02_Data/04_Data_to_model/all_data_V_C.csv")
# Remove MODIS NDVI
data$month <- as.numeric(format(data$Date_GTD,"%m"))
data$Orbit <- as.factor(data$Orbit)
data <- data %>% select(-c("NDVI"))%>% 
  filter( month<=11 & month>=3 )

data <- data %>% 
  filter( month<=11 & month>=3 )

#data <- data[,c("","Station_S1","Orbit","VV_multilook","obs_05")]


data$week <- as.numeric(format(data$Date_GTD,"%U"))


data <- data %>% 
    filter(Station_S1%in%c("B1","B2","I1","M2","P1","P2")) %>% #%>% 
    #filter(Station_S1%in%c("B1","B2","I1","M5","P1","S3")) #%>% 
    filter( month<=11 & month>= 3)
    #filter( month<=11 & month>=3 )
    #filter(NDVI_L8<0.8) #%>% 
data <- data[complete.cases(data),]    


#data <- data %>% filter(Station_S1%in%c("S3"))
  # %>% 
#  filter( month<=12 & month>=5 )
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
set.seed(314)
ggplot(data,aes(x=Station_S1,y=angle,group=Station_S1,fill=Orbit))+geom_boxplot()

ggplot(data %>% mutate(Orbit =as.factor(Orbit)), aes(x = Station_S1, y = angle, fill = Orbit)) +
  geom_boxplot() +
  labs(x = "Stations", y = "Incidence Angle") +
  ggtitle("Box Plots Grouped by Station_S1 and Orbit")


data %>%group_by(Station_S1,Orbit) %>% 
  summarise(min_ang = min(angle),max_ang = max(angle)) 

data %>%group_by(Station_S1) %>%ggplot(aes(x=Station_S1,y=angle))+geom_boxplot(fill="skyblue3")+
  xlab("Stations")+ylab("Incidence Angle °")+ theme_grey()

data$year_month <- format(data$Date_GTD,"%y_%m_%U")
groups <- unique(data$year_month)


# num_groups <- length(unique(data$year_month))
num_train_groups <- round(0.8 * length(unique(groups)))

train_groups <- sample(groups, size = num_train_groups)
 
print(paste0("Tests groups: ",paste0(train_groups, collapse = ", ")))
print(paste0("Tests groups: ",paste0(unique(groups)[!unique(groups)%in%train_groups], collapse = ", ")))

# Split the dataframe into training and test datasets
#train_id <- caret::createDataPartition(data$...1,p=0.8,list = T)

#train_df<- data[unlist(train_id),]
#test_df <- data[!data$...1%in%train_df$...1,]
train_df <- data %>% filter(year_month %in% train_groups)
test_df <- data %>% filter(!year_month %in% train_groups)


# ---------
train_df %>% 
  #filter(wc_02_av<1) %>% 
  ggplot(aes(y=obs_05,x=Station_S1,fill=Station_S1))+
  ggdist::stat_halfeye(adjust = 0.5,
                       justification=-0.2,
                       .width=0,
                       point_color=NA,show.legend=F)+
  geom_boxplot(width = 0.2,
               alpha=0.2,
               show.legend = FALSE)+ theme(legend.position = "none")   +
  labs(title="Train data set",y= "SMC m³/m³ at 5 cm of depth",x="Stations")+
  #scale_fill_tq()+theme_tq()+
  coord_flip()


#----------------


### ----------------------- Caret Models -------------------------------- ###
library(doParallel) 
model_test <- function(test_df,model){
  test_df$pred <- predict(model, newdata = test_df)
  R2<- caret::R2(test_df$pred,test_df$obs_05)
  RMSE<- caret::RMSE(test_df$pred,test_df$obs_05)
  AVE = (1 - sum((test_df$pred - test_df$obs_05)^2, na.rm = TRUE) /
           sum((test_df$obs_05- mean(test_df$obs_05, na.rm = TRUE))^2, na.rm = TRUE))
  pearson<- cor.test(test_df$pred,test_df$obs_05,method = "pearson")
  spearman<- cor.test(test_df$pred,test_df$obs_05,method = "spearman")
  print(data.frame(model=model$modelInfo$label,R2=R2,RMSE=RMSE,AVE=AVE,
                   pearson=pearson$estimate[[1]],spearman=spearman$estimate[[1]]))
}


mc <- makeCluster(detectCores()-2)
registerDoParallel(mc)

myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=10,
                          returnResamp='all', 
                          allowParallel=T)

set.seed(849)

fm<- as.formula(paste0("obs_05" , " ~ ", 
                       paste0(setdiff(names(train_df),c("obs_05","...1",
                                                        "Station_S1","S1","Date_S1",
                                                        "Orbit","Date_GTD",
                                                        "year_month", "sd_obs_02","sd_obs_05",
                                                        "obs_02","VV","VH","month","week")),collapse=" + " )))

tunegrid <- expand.grid(.mtry=seq(from=2,to=8,by=2))
rf <- train(fm, 
                 data=train_df,
                 method = "rf",
                 metric= "RMSE",
                 maximize=F,
                 trControl = myControl,
                 tuneGrid=tunegrid,
                 ntree=200)
varImpPlot(rf[11][[1]])
plot(rf[11][[1]])
model_test(test_df,model= gbmFit2)

test_df$pred <- predict(rf, newdata = test_df)
test_df$month <- as.factor(test_df$month)
test_df$abs_error <- abs(predict(rf, newdata = test_df) - test_df$obs_05)
test_df %>% ggplot(aes(y=abs_error,x=Station_S1,fill=Station_S1))+geom_boxplot()
test_df %>% ggplot(aes(y=abs_error,x=month,fill=month))+geom_boxplot()


ggscatter(test_df, x = "pred",  y = "obs_05",
          add = "reg.line",size = 0.415,
          panel.labs.background = list(fill = "skyblue3", color = "black"),
          add.params = list(color = "royalblue4",size=0.13),
          palette =  get_palette("Dark2", 3),ylim = c(0, .5))+
  stat_cor(aes(label = paste(..rr.label..,..r.label..,sep = "~`,`~"),digits = 1),
           size=4.3,label.sep='\n',digits = 2,r.digits = 2, method = "spearman",label.y = c(0.5))+
  geom_abline(intercept=0, slope=1,col='red',lty=3)+
  
  labs(x='Predicted', y='Observed',size=2)

#covs <- rast("./02_Data/05_Rasters/covs_no_resample.tif")
#covs

#predictors <- unlist(strsplit(as.character(fm[[3]]), "\\s\\+\\s"))[-1]

#SMC_rf <- terra::predict(covs[[names(covs)%in%predictors]],rf,na.rm=T)

# library(mapview)
# mapview(as.raster(SMC_rf))
# 
# tmap_mode("view")
# tm_shape(SMC_rf)+tm_raster()
# 
# qrf<- train(fm, 
#          data=train_df,
#          method = "qrf",
#          metric= "RMSE",
#          maximize=F,
#          trControl = myControl,
#          tuneGrid=tunegrid)
qrf<- train(fm,
               data=train_df,
               method = "qrf",
               metric= "RMSE",
               maximize=F,
               trControl = myControl,
               tuneGrid=tunegrid)
model_test(test_df,model= qrf)

ranger<- train(fm,
               data=train_df,
               method = "ranger",
               metric= "RMSE",
               maximize=F,
               trControl = myControl,
               #tuneGrid=tunegrid
               )
model_test(test_df,model= ranger)

varImpPlot(rf[11][[1]],main="Model 4, Months 3-11, Stations: 7")
# 
# #randomForest::varImpPlot(fit.rf$finalModel)
# 
# xgbTree <- train(fm, 
#                  data=train_df,
#                  method = "xgbTree",
#                  metric= "RMSE",
#                  preProc = c("center", "scale"),
#                  trControl = myControl)
# 
# gbm_h2o <- train(fm, 
#                  data=train_df,
#                  method = "gbm_h2o", # 
#                  metric= "RMSE",
#                  preProc = c("center", "scale"),
#                  trControl = myControl)
# 
gbmGrid <-  expand.grid(interaction.depth = c(1, 5,8, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(fm, data = train_df, 
                 method = "gbm", 
                 verbose = FALSE, 
                 trControl = myControl,
                 metric= "RMSE",
                 preProc = c("center", "scale"),
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
model_test(test_df,model= gbmFit2)
summary(gbmFit2, cBars = 10, las = 2)

varImpPlot(gbmFit2[11][[1]])
plot(varImp(gbmFit2),main="GBM relative importance")

imp
library(gbm)
ggplot(varImp(gbmFit2))+
  geom_area()+
  theme_classic()+
  labs(title="GBM relative importance")

##############
# this part just creates the data.frame for the plot part
library(dplyr)
imp <- data.frame(varImp(gbmFit2)[[1]])
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  
imp$var_categ <- rep(1:2, 5) # random var category

# this is the plot part, be sure to use reorder with the correct measure name
library(ggplot2) 
ggplot(imp, aes(y=reorder(varnames, Overall),x=Overall, weight= Overall,fill=Overall)) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "cadetblue3", high = "dodgerblue4") +
  xlab("Relative Importance") +
  ylab("Variable")


##############


gbmFit2
trellis.par.set(caretTheme())
plot(gbmFit2,xlab="N° trees")   
trellis.par.set(caretTheme())
plot(gbmFit2, metric = "Rsquared")
trellis.par.set(caretTheme())
plot(gbmFit2, metric = "RMSE", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(gbmFit2)  

gbm <- train(fm,
                     data=train_df,
                     method = "gbm", #
                     metric= "RMSE",
                     preProc = c("center", "scale"),
                     trControl = myControl)
# 
# xgbTree <- train(fm, 
#                      data=train_df,
#                      method = "xgbTree", # 
#                      metric= "RMSE",
#                      preProc = c("center", "scale"),
#                      trControl = myControl)
# 
xgbDART <- train(fm,
                 data=train_df,
                 method = "xgbDART", #
                 metric= "RMSE",
                 preProc = c("center", "scale"),
                 trControl = myControl)
model_test(test_df,model= xgbDART)


brnn <- train(fm,
                     data=train_df,
                     method = "brnn",
                     metric= "RMSE",
                     preProc = c("center", "scale"),
                     trControl = myControl)

dnn <- train(fm,
                  data=train_df,
                  method = "dnn",
                  metric= "RMSE",
                  preProc = c("center", "scale"),
                  trControl = myControl)

svmRadial<- train(fm, data = train_df, 
                  method = "svmRadial", 
                  trControl = myControl, 
                  preProc = c("center", "scale"),
                  tuneLength = 8,
                  metric = "RMSE")
model_test(test_df,model= svmRadial)

svmLinear3 <- train(fm,
                  data=train_df,
                  method = "svmLinear3",
                  metric= "RMSE",
                  preProc = c("center", "scale"),
                  trControl = myControl)

resamps <- resamples(list(GBM = gbmFit2,
                          SVM = svmLinear3,
                          RF = rf,
                          QRF=qrf))
summary(resamps)
trellis.par.set(caretTheme())
dotplot(resamps, metric = "RMSE")
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.4, .4, .4, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)

bwplot(resamps, layout = c(3, 1),
       scales = list(y = list(relation="free"),
                     x = list(relation="free")),
       par.settings = list(strip.background=list(col="steelblue3")))

trellis.par.set(caretTheme())
dotplot(resamps)# 

parallelplot(resamps)

 
# svmBoundrangeString <- train(fm, 
#                   data=train_df,
#                   method = "svmBoundrangeString",
#                   metric= "RMSE",
#                   preProc = c("center", "scale"),
#                   trControl = myControl)
# 
svmLinear2 <- train(fm,
                 data=train_df,
                 method = "svmLinear2",
                 metric= "RMSE",
                 preProc = c("center", "scale"),
                 trControl = myControl)


# models <- list(rf,xgbTree,gbm_h2o,gbm,xgbDART,brnn,dnn,avNNet,neuralnet,nnet,svmLinear3,svmBoundrangeString,svmLinear2)
# test_df_2 <- test_df
# for(i in models){
#   col_name <- as.character((i[1][[1]]))
#   test_df_2 <- test_df_2 %>%
#   mutate(!!col_name := predict(i, newdata = test_df_2))
# }
# 

  
model <- gbmFit2#rf#svmLinear2#dnn#svmLinear3#brnn#ranger# rf qrf ranger
model_test(test_df,model= model)

#train_df$pred<- predict(model, newdata = train_df)
test_df$pred<- predict(model, newdata = test_df)
#train_df$abs_error <- abs(train_df$pred - train_df$obs_05 )
# ggscatter(train_df, x = "pred", size = 0.3,color="abs_error",
#           y = "obs_05",# points
#           add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
#           add.params = list(color = "royalblue4",size=0.3))+
#   stat_cor(aes(label = paste(..rr.label..,..r.label..,sep = "~`,`~")),
#            size=3,label.sep='\n')+
#   geom_abline(intercept=0, slope=1,col='red',lty=2,size=0.6)+
#   labs(x='SMC m³/m³ Predicted', y='SMC m³/m³ Observed',size=2)+ ggtitle(paste0("Model Train"))
# train_df %>% ggplot(aes(x=angle,y = abs_error))+geom_point()

test_df$abs_error <- abs(test_df$pred - test_df$obs_05 )
ggplot(test_df,aes(x=month,y=abs_error,group=month))+geom_boxplot()
caret::RMSE(test_df$pred,test_df$obs_05)
caret::R2(test_df$pred,test_df$obs_05)

ggscatter(test_df, x = "pred", size = 0.3,
                      y = "obs_05",# points
                      add = "reg.line",panel.labs.background = list(fill = "skyblue3", color = "black"),
                      add.params = list(color = "royalblue4",size=0.3))+
              stat_cor(aes(label = paste(..rr.label..,..r.label..,sep = "~`,`~")),
                       size=3,label.sep='\n')+
              geom_abline(intercept=0, slope=1,col='red',lty=2,size=0.6)+
              labs(x='SMC m³/m³ Predicted', y='SMC m³/m³ Observed',size=2)+ 
  ggtitle(paste0("Model Validation "))

test_df %>% ggplot(aes(y=abs_error,x=Station_S1,fill=Station_S1))+geom_boxplot()+
  labs(y='Absolut error', x='Soil Moisture Stations',size=2)
# Assuming you have two arrays: actual_values and predicted_values


# Step 1: Compute the residuals
residuals = test_df$obs_05 - test_df$pred

# Step 2: Calculate the mean of the residuals
mean_residuals = mean(residuals)

# Step 3: Adjust the residuals by subtracting the mean
adjusted_residuals = residuals -  mean_residuals

# Step 4: Square each adjusted residual
squared_residuals = (adjusted_residuals)^2

# Step 5: Calculate the mean of the squared adjusted residuals
mean_squared_residuals = mean(squared_residuals)

# Step 6: Take the square root to obtain ubRMSE
ubRMSE = sqrt(mean_squared_residuals)

print("ubRMSE:", ubRMSE)
