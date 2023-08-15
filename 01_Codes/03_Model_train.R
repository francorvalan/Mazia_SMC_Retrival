library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(caret)
library(h2o)
data <- read_csv("./02_Data/04_Data_to_model/all_data_complete.csv")

str(data)
data <- data %>% filter(Station_S1%in%c("B1","B2","I1","I3","M5","P1","P3","S3"))

data <- data %>% filter(Station_S1%in%c("S3"))
data$landuse <- as.factor(data$landuse)
set.seed(31415)


data %>%group_by(Station_S1,Orbit) %>% 
  summarise(min_ang = min(angle),max_ang = max(angle)) 

data %>%group_by(Station_S1) %>%ggplot(aes(x=Station_S1,y=angle))+geom_boxplot(fill="skyblue3")+
  xlab("Stations")+ylab("Incidence Angle °")+ theme_grey()

data$year_month <- format(data$Date_GTD,"%y_%m")
groups <- unique(data$year_month)

# num_groups <- length(unique(data$year_month))
num_train_groups <- round(0.7 * length(unique(groups)))

train_groups <- sample(groups, size = num_train_groups)
 
print(paste0("Tests groups: ",paste0(train_groups, collapse = ", ")))
print(paste0("Tests groups: ",paste0(unique(groups)[!unique(groups)%in%train_groups], collapse = ", ")))

# Split the dataframe into training and test datasets
train_df <- data %>% filter(year_month %in% train_groups)
 
test_df <- data %>% filter(!year_month %in% train_groups)
str(test_df)

train_df %>% filter(Station_S1=="I1") %>% ggplot(aes(x=Date_GTD,y = obs_05))+geom_point(aes(col="train"),size=0.1)+
  geom_point(data=test_df %>% filter(Station_S1=="I1"),aes(x=Date_GTD,y = obs_05,col="test"),size=0.1)+
  ggtitle("Station I1") + ylab("SMC 5 cm m³/m³")+xlab("Date")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = "Data",
                     values = c("train" = 'darkblue',
                                "test"="red2"
                     ))



# 
localH2O = h2o.init()
#h2o.uploadFile(path = iris_path)

h2o.init(
  ip = "localhost",
  # -1 indica que se empleen todos los cores disponibles.
  nthreads = -1,
  # Máxima memoria disponible para el cluster.
  max_mem_size = "10g"
)
df<-  as.h2o(train_df)
splits <- h2o.splitFrame(df, ratios = 0.75, seed=1234)

train <- h2o.assign(splits[[1]], "train") # 75%
valid  <- h2o.assign(splits[[2]], "valid") # 12%
test   <- h2o.assign(as.h2o(test_df), "test")

y <- "obs_05"
x <- setdiff(names(train), c("obs_05","...1","Station_S1","S1","Date_S1","Orbit","VV","VH",
                             "Date_GTD","sd_obs_02","obs_02","sd_obs_05","year_month"))

hyper_params <- list(
  activation = c("Rectifier", "Maxout", "Tanh",
                 "RectifierWithDropout", "MaxoutWithDropout",
                 "TanhWithDropout"),                      
  hidden = list(c(5, 5, 5, 5, 5), c(10, 10, 10, 10), c(50, 50, 50), c(100, 100, 100)),
  epochs = c(50, 100, 200),
  l1 = c(0, 0.00001, 0.0001), 
  l2 = c(0, 0.00001, 0.0001),
  rate = c(0, 01, 0.005, 0.001),
  rate_annealing = c(1e-8, 1e-7, 1e-6),
  rho = c(0.9, 0.95, 0.99, 0.999),
  epsilon = c(1e-10, 1e-8, 1e-6, 1e-4),
  momentum_start = c(0, 0.5),
  momentum_stable = c(0.99, 0.5, 0),
  input_dropout_ratio = c(0, 0.1, 0.2),
  max_w2 = c(10, 100, 1000, 3.4028235e+38)
)
hyper_params


search_criteria <- list(strategy = "RandomDiscrete", 
                        max_models = 200,
                        max_runtime_secs = 900,
                        stopping_tolerance = 0.001,
                        stopping_rounds = 2,
                        seed = 1345767)

DNN_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = "DNN_grid_ID",
  x= x,
  y = y,
  training_frame = train,
  validation_frame =valid,  
  stopping_metric = "logloss",
  nfolds=10,
  hyper_params = hyper_params,
  search_criteria = search_criteria,
  seed = 42)


grid.dnn <- h2o.getGrid("DNN_grid_ID",sort_by="RMSE",decreasing=FALSE)

grid.dnn@summary_table[1,]

best_model <- h2o.getModel(grid.dnn@model_ids[[1]]) ## model with lowest logloss
best_model
best_params <- best_model@allparameters
h2o.mean_per_class_error(best_model, train = TRUE, valid = TRUE, xval = TRUE)

scoring_history<-best_model@model$scoring_history

plot(best_model,
     timestep = "epochs",
     metric = "classification_error")
plot(best_model,
     timestep = "epochs",
     metric = "logloss")

print(best_model@model$cross_validation_metrics_summary%>%.[,c(1,2)])

h2o.performance(best_model, newdata=train)     ## full train data
h2o.performance(best_model, newdata=valid)     ## full validation data

################------------------------------------------------------------
# Do a Grid Search to tune the hidden layers and the droput ratio
dl_hidden_grid <- h2o.grid(algorithm = "deeplearning",
                           grid_id = "dl_hidden_grid",
                           activation = c("RectifierWithDropout"),
                           epochs = 10,
                           seed = 42,
                           stopping_rounds = 3,
                           stopping_metric ="RMSE",
                           stopping_tolerance = 1e-3,
                           x = x,
                           y = y,
                           training_frame = train,
                           validation_frame = valid,
                           hyper_params = list(
                             hidden = list(c(100, 100), c(165, 165), c(200, 200), c(330, 330),
                                           c(165, 200)),
                             hidden_dropout_ratios = list(c(0,0), c(0.01,0.01), c(0.15,0.15),
                                                          c(0.30, 0.30), c(0.5,0.5))),
                           search_criteria = list(
                             strategy = "RandomDiscrete",
                             max_runtime_secs = 900,
                             seed = 42))

# Retrieve the Grid Search
dl_hidden_grid_rmse <- h2o.getGrid(grid_id = "dl_hidden_grid", sort_by = "rmse", decreasing = FALSE)
as.data.frame(dl_hidden_grid_rmse@summary_table)

top_dl <- h2o.getModel(dl_hidden_grid_rmse@model_ids[[1]]) #getting the best model


# Grid Search to tune the Max W2 and L2
dl_random_grid_rmse <- h2o.grid(algorithm = "deeplearning", 
                                grid_id = "dl_random_grid",
                                #hidden = top_dl@allparameters[["hidden"]],
                                #epochs = 50,
                                seed = 42,
                                #hidden_dropout_ratios = top_dl@allparameters[["hidden_dropout_ratios"]],
                                stopping_rounds = 5,
                                stopping_metric = "RMSE",
                                stopping_tolerance = 1e-3,
                                x = x,
                                y = y,
                                training_frame = train,
                                validation_frame = valid,
                                hyper_params = list(
                                  epochs = c(50, 100, 200),
                                  l1 = c(0, 0.00001, 0.0001), 
                                  #l2 = c(0, 0.00001, 0.0001),
                                  hidden = list(c(3, 3,3,3,3),c(5, 5,5,5,5),c(10, 10,10,10,10),c(30, 30,30,30), c(165, 165), c(200, 200)),
                                  hidden_dropout_ratios = list(c(0,0), c(0.01,0.01), c(0.15,0.15),
                                                               c(0.30, 0.30), c(0.5,0.5)),
                                  activation = c("Rectifier", "Maxout", "Tanh",
                                                                   "RectifierWithDropout", "MaxoutWithDropout",
                                                                   "TanhWithDropout"),
                                  max_w2 = c(1e38, 1e35, 1e36, 1e37, 1e34, 5e35),
                                  l2 = c(1e-8,1e-7,  1e-6, 1e-5, 1e-4, 5e-4, 1e-3, 0)),
                                  search_criteria = list(
                                  strategy = "RandomDiscrete",
                                  max_runtime_secs = 900,
                                  seed = 42))
# Retrieve the Grid Search
dl_random_grid_rmse <- h2o.getGrid(grid_id = "dl_random_grid", sort_by = "rmse", decreasing = FALSE)
as.data.frame(dl_random_grid_rmse@summary_table)

# Retrieve the best model from the Grid Search
tuned_dl <- h2o.getModel(dl_random_grid_rmse@model_ids[[1]]) #getting the best model

tuned_dl@allparameters
# Checkpointing for DL model to increase the number of epochs
h2o.performance(tuned_dl, newdata=valid)
h2o.varimp_plot(tuned_dl)

dl_checkpoint<- h2o.deeplearning(x = x,
                                  y = y,
                                  training_frame = train,
                                  model_id = "dl_checkpoint",
                                  validation_frame = valid,
                                  #checkpoint = dl_random_grid_rmse@model_ids[[1]],
                                  activation = "RectifierWithDropout",
                                  #hidden = tuned_dl@allparameters[["hidden"]],
                                  #epochs = 400,
                                  #seed = 42,
                                  #hidden_dropout_ratios = tuned_dl@allparameters[["hidden_dropout_ratios"]],
                                  #l2 = tuned_dl@parameters$l2,
                                  #max_w2 = tuned_dl@parameters$max_w2,
                                  #reproducible = TRUE,
                                  #stopping_rounds = 5,
                                  #stopping_metric = "RMSE",
                                  #stopping_tolerance= 1e-5
                                  )
# Save the validation performance of the best DL model
valid_tuned_dl_perf <- h2o.performance(dl_checkpoint, valid)
# Compare the RMSE of the default and tuned DL model
h2o.rmse(valid_def_dl_perf)

# Print the validation RMSE for the tuned model 
h2o.rmse(valid_tuned_dl_perf)
#############----------------------- XGBOOST --------------------------------
# Import the H2O library
library(h2o)
# Initialize the H2O Cluster
localH2O = h2o.init()

df<-  as.h2o(train_df)
splits <- h2o.splitFrame(df, ratios = 0.75, seed=1234)

train <- h2o.assign(splits[[1]], "train") # 75%
valid  <- h2o.assign(splits[[2]], "valid") # 12%
test   <- h2o.assign(as.h2o(test_df), "test")

y <- "obs_05"
x <- setdiff(names(train), c("obs_05","...1","Station_S1","S1","Date_S1","Orbit","VV","VH",
                             "Date_GTD","sd_obs_02","obs_02","sd_obs_05","year_month"))


# Set-up the Grid Search
xgb_depth_grid <- h2o.grid(algorithm = "xgboost",
                           grid_id = "xgb_depth_grid",
                           stopping_rounds = 3,
                           stopping_metric ="RMSE",
                           stopping_tolerance = 1e-3,
                           seed = 42,
                           ntrees = 300,
                           x = x,
                           y = y,
                           training_frame = train,
                           validation_frame = valid,
                           hyper_params = list(
                             max_depth = c(5,7,9,10,12,13,15,20)),
                           search_criteria = list(
                             strategy = "Cartesian"))

# Retrieve the grid search sorted by RMSE and in ascending order
xgb_depth_grid_rmse <- h2o.getGrid(grid_id = "xgb_depth_grid", sort_by = "rmse", decreasing = FALSE)
as.data.frame(xgb_depth_grid_rmse@summary_table)



# Create sequences for three different parameters to explore more models
xgb_sample_rate <- seq(from = 0.2, to = 1, by = 0.01)
xgb_col_sample_rate <- seq(from = 0.2, to = 1, by = 0.01)
xgb_col_sample_rate_per_tree <- seq(from = 0.2, to = 1, by = 0.01)

xgb_random_grid <- h2o.grid(algorithm = "xgboost",
                            grid_id = "xgb_random_grid",
                            stopping_rounds = 3,
                            stopping_metric = "RMSE",
                            stopping_tolerance = 1e-3,
                            seed = 42,
                            ntrees = 500,
                            learn_rate = 0.25,
                            x = x,
                            y = y,
                            training_frame = train,
                            validation_frame = valid,
                            hyper_params = list(
                              max_depth = c(5,6,7,9),
                              sample_rate = xgb_sample_rate, 
                              col_sample_rate = xgb_col_sample_rate, 
                              col_sample_rate_per_tree = xgb_col_sample_rate_per_tree),
                            search_criteria = list(
                              strategy = "RandomDiscrete",
                              max_runtime_secs = 900,
                              seed = 42))


# Retrieve the second Grid Search for the XGBoost
xgb_random_grid_rmse <- h2o.getGrid(grid_id = "xgb_random_grid", sort_by = "rmse", decreasing = FALSE)
as.data.frame(xgb_random_grid_rmse@summary_table)

# Retrieve the best model from the Grid Search
tuned_xgb <- h2o.getModel(xgb_random_grid_rmse@model_ids[[1]]) #getting the best model

# Save the validation performance of the tuned model
valid_tuned_xgb_perf <- h2o.performance(tuned_xgb, valid)

# Compare the RMSE of the default and tuned XGBoost
h2o.rmse(valid_def_xgb_perf)
h2o.rmse(valid_tuned_xgb_perf)


# Compare the MAE of the default and tuned XGBoost
h2o.mae(valid_def_xgb_perf)
h2o.mae(valid_tuned_xgb_perf)



