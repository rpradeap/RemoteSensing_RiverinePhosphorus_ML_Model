#INSTALL ALL LIBRARIES
source("C:\\Research\\Function_Code\\R\\Library.R")
name <- "XGBTree_Base"
         # Change to your directory


#DEFINE ML AlGORITHM
trainmethod = "xgbTree"
validation = 2
if (validation == 1) {out <- "90%training"} else {out <- "10%validation"}


#STEP1: Get Dataset
# Retrieve dataset
matchup_data_clean <- read.csv("C:\\Research\\TP_Part1\\Data_V1\\matchup_data_clean_good.csv")
          # Includes matchup data for gauge TP measurement and corresponding river reflectance data
          # Change to your directory


#STEP2: Create TRAIN and VALIDATION
# Create data partition into training and validation dataset based on space, time, and magnitude
df <- data_partition(matchup_data_clean)
          # Divides the dataset into 90% training and 10% validation dataset


df <- df%>%
  holdout() %>%
  ungroup() %>%
  mutate(value = log(ResultMeasureValue)) %>%  #log the TP measurement for data distribution normalization 
  filter(value!=-Inf)%>%
  mutate_if(is.character,as.factor) %>%
  as.data.frame()%>%suppressWarnings()


train_test <- create_train_validate(df = df,train_partition = 1,validate_partition = validation) 
          # Change validate_partitions into 1 to use 10% validation data for model estimation
          # Change validte_partitions into 2 to use 90% training data for model estimation for checking any overfitting issues


train <- train_test$train_set
validate <- train_test$validate_set


#STEP 3: Develop a ML Model 
#Get Selected features : Feature_1 is the list of significant features used to develop this model and 
#feature_2 is the same list with station characteristic's column name such as lat and lon. 
source("C:\\Research\\TP_Part1\\Data_V1\\Features.R")


#Hyper parametric Tuning

# grid_base <- expand.grid(
#   nrounds = seq(50,70,10),
#   max_depth = seq(500, 900, 100),
#   colsample_bytree = c(0.05, 0.1),
#   eta = c(0.05, 0.1),
#   gamma =c(0.5, 1),
#   min_child_weight =c(5,15),
#   subsample =c(0.5, 0.75)
# )


#grid_final <- grid_best(train, grid_base, features_1)
grid_final <- expand.grid( nrounds = 70, max_depth =900, colsample_bytree = 0.1, eta = 0.05, gamma =1, min_child_weight =15, subsample =0.5)


#Model Training
model <- train_model(train, features_1, grid_final, trainmethod)


#STEP 4: Model Performance Evaluation
#Estimation
output<- prediction_function(model, validate)


#Model Evaluation
evals <- output %>%
  mutate(Actual = (Actual), 
         Predicted = (Predicted)) %>%
  summarise(rmse = rmse(Actual, Predicted),          # Calculate RMSE
            MAE = percent_bias_n(Actual, Predicted), # Calculate MAE
            NSE = NSE(Predicted, Actual),        # Calculating NSE
            rsquared = cor(Actual, Predicted)^2  # Calculating R-squared
  )


print_result(evals)
