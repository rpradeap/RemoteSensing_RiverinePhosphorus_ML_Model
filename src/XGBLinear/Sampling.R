#INSTALL ALL LIBRARIES
source("C:\\Research\\TP_Part1\\Code\\Riverine_TP_Models\\function\\Library.R")
name <- "XGBLinear_Sampling"
# Change to your directory


#DEFINE MODEL NAME
trainmethod = "xgbLinear"
validation = 1
if (validation == 1) {out <- "90%training"} else {out <- "10%validation"}


#STEP1: Get Dataset
# Retrieve dataset
matchup_data_clean <- read.csv("C:\\Research\\TP_Part1\\Code\\Riverine_TP_Models\\data\\matchup_data_clean_good.csv")
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


train_test <- create_train_validate(df = df,train_partition = 1,validate_partition =validation) 
# Change validate_partitions into 1 to use 10% validation data for model estimation
# Change validte_partitions into 2 to use 90% training data for model estimation for checking any overfitting issues


train <- train_test$train_set
validate <- train_test$validate_set


label_partition_values <- data.frame(
  label = c(0, 2.99990844307448, 5.00013733538828, 18.0000305189752, 19),
  train_partition = 1,
  validate_partition =validation
)


result_list <- list()


# CLUSTERING METHOD
# Loop through the label and partition values and create train and validate sets
for (i in seq(nrow(label_partition_values))) {
  label_value <- label_partition_values$label[i]
  train_partition_value <- label_partition_values$train_partition[1]
  validate_partition_value <- label_partition_values$validate_partition[1]
  
  result <- create_train_validate(df, label_value, train_partition_value, validate_partition_value)
  
  # Store the results in a list
  result_list[[paste0("train.", i)]] <- result$train_set
  result_list[[paste0("validate.", i)]] <- result$validate_set
}

train.1 <- result_list[["train.1"]]
validate.1 <- result_list[["validate.1"]]

train.2 <- result_list[["train.2"]]
validate.2 <- result_list[["validate.2"]]

train.3 <- result_list[["train.3"]]
validate.3 <- result_list[["validate.3"]]

train.4 <- result_list[["train.4"]]
validate.4 <- result_list[["validate.4"]]

train.5 <- result_list[["train.5"]]
validate.5 <- result_list[["validate.5"]]


#STEP 3: Develop a ML Model 
#Selected features 
source("C:\\Research\\TP_Part1\\Code\\Riverine_TP_Models\\cache\\Features.R")


#Hyper parametric Tuning
# grid_base <- expand.grid(
#   nrounds = seq(10,100,10),
#   alpha = c(0.25,0.5),
#   lambda = c(0.25,0.5,0.75),
#   eta = c(0.01, 0.05, 0.1))


#grid_final <- grid_best(train, grid_base, features_1)
grid_final <- expand.grid(nrounds = 10, alpha = 0.25, lambda= 0.75, eta= 0.01) 

#SAMPLING METHOD
          #Iterate to find the best parametric combination for sampling
          # This creates a list of parametric combination and the corresponding Rsquare value for each cluster model.
          # Rsquare1 is the value of Rsquare for cluster 1  from the Cluster.R and respectively; it should be numeric

# R2.1 <- sampling_train_data(train.1, features_1, features_2, grid_final, trainmethod, validate.1)
# R2.2 <- sampling_train_data(train.2, features_1, features_2, grid_final, trainmethod, validate.2)
# R2.3 <- sampling_train_data(train.3, features_1, features_2, grid_final, trainmethod, validate.3)
# R2.4 <- sampling_train_data(train.4, features_1, features_2, grid_final, trainmethod, validate.4)
# R2.5 <- sampling_train_data(train.5, features_1, features_2, grid_final, trainmethod, validate.5)

 
#write.csv(R2.3$undersampling, "C:\\Users\\Admin\\OneDrive - University of Cincinnati\\Desktop\\Folder\\XGBlinear\\Undersample\\R2_3.csv") 
 
#Select the parametric with the best Rsquare value based on above iterative results
train.1 <- Smote_train(train.1, features_2, 0.7, 10) 
train.4 <- Smote_train(train.4, features_2, 0.1, 19) 
train.2 <- undersample_filter(train.2, 0.25, 10, 0, 1) 
train.3 <- undersample_filter(train.3, 0.15, 10, 0.075, 0.925) 
train.5 <- undersample_filter(train.5, 0.15, 25, 0.35, 0.65) 


model.1 <- train_model(train.1, features_1, grid_final, trainmethod)
model.2 <- train_model(train.2, features_1, grid_final, trainmethod)
model.3 <- train_model(train.3, features_1, grid_final, trainmethod)
model.4 <- train_model(train.4, features_1, grid_final, trainmethod)
model.5 <- train_model(train.5, features_1, grid_final, trainmethod)


#STEP 4: Model Performance Evaluation
#Estimation
output.1<- prediction_function(model.1, validate.1)
output.2<- prediction_function(model.2, validate.2)
output.3<- prediction_function(model.3, validate.3)
output.4<- prediction_function(model.4, validate.4)
output.5<- prediction_function(model.5, validate.5)


output <- rbind(output.1, output.2, output.3, output.4, output.5)


#Model Evaluation
evals <- output%>%
  mutate(Actual = (Actual), 
         Predicted = (Predicted)) %>%
  summarise(rmse = rmse(Actual, Predicted),          # Calculate RMSE
            MAE = percent_bias_n(Actual, Predicted), # Calculate MAE
            NSE = NSE(Predicted, Actual),        # Calculating NSE
            rsquared = cor(Actual, Predicted)^2  # Calculating R-squared
  )

print_result(evals)
