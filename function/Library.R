suppressPackageStartupMessages({
  
  library(feather)
  library(viridis)
  library(maps)
  library(magrittr)
  library(mlbench)
  library(caret)
  library(randomForest)
  library(doParallel)
  library(onehot)
  library(xgboost)
  library(Metrics)
  library(purrr)
  library(data.table)
  library(mltools)
  library(ggthemes)
  library(dplyr)
  library(ggplot2)
  library(mltools)
  library(CAST)
  library(future)
  library(foreign)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(forcats)
  library(UBL)
  library(hydroGOF)
  library(NbClust)

  
  holdout <- function(x) {
    set.seed(22)
    
    train <- x %>%
      group_by(time_group, long_group, mag) %>%
      sample_frac(.9) %>%
      ungroup() %>%
      dplyr::mutate(.partitions = 1)
    
    set.seed(22)
    
    validate <- x %>%
      anti_join(train) %>%
      dplyr::mutate(.partitions = 2)
    
    out <- train %>%
      bind_rows(validate) 
    
    return(out)
  }
  
  
})

#----------------------------------------------------------------
train_model <- function(train, features_1, grid_final, trainmethod) {
  set.seed(10)
  
  # Create a parallel cluster
  cl <- makePSOCKcluster(detectCores() - 2)
  registerDoParallel(cl)
  
  # Create spatial-temporal folds
  folds <- CreateSpacetimeFolds(train, spacevar = "long_group", timevar = "time_group", k = 3)
  
  # Define training control
  train_control_final <- trainControl(
    method = "cv",
    savePredictions = TRUE,
    returnResamp = 'final',
    index = folds$index,
    indexOut = folds$indexOut,
    verboseIter = TRUE,
    allowParallel = TRUE,
    p = 0.8
  )
  
  # Train the model
  model <- train(
    x = train[, features_1],
    y = train$value,
    trControl = train_control_final,
    tuneGrid = grid_final,
    method = trainmethod,
    importance = TRUE,
    verbose = TRUE
  )
  
  # Stop the parallel cluster
  stopCluster(cl)
  
  return(model)
}
#----------------------------------------------------------------
  
#---------------------------------------------------------------- 
  percent_bias_n <- function(actual, predicted){
    return (mean((abs(actual-predicted)/actual)))
  }
  
#----------------------------------------------------------------
  prediction_function <- function(model, data) {
    pred <- predict(model, data[, features_1])
    actual <- data$value
    lon <- data$lon
    lat <- data$lat
    date <- data$ActivityStartDate
    Collection.M <- data$SampleCollectionMethod.MethodIdentifier
    Analytical.M <- data$ResultAnalyticalMethod.MethodIdentifier
    
    output <- tibble(
      Predicted = exp(pred),
      Actual = exp(actual),
      date = as.Date(date),
      long = lon,
      lat = lat,
      Collection = Collection.M,
      Analytical = Analytical.M
    ) %>%
      mutate(
        residual = Actual - Predicted,
        year = year(date),
        month = month(date),
        error.1 = abs(Actual - Predicted) * 100 / Actual,
        error.2 = abs(Actual - Predicted) * 100 / Predicted
      )
    
    return(output)
  }
  
  
#------------------------------------------------------------
 run_feature_selection <- function(train, iter, out_folder, features) {
    tuneGrid1 <- expand.grid(
      nrounds = 300,
      eta = 0.01,
      lambda = 0.5,
      alpha = 0.5
    )
    
    set.seed(10)
    folds <- CreateSpacetimeFolds(
      train,
      spacevar = "long_group",
      timevar = "time_group",
      k = 3
    )
    
    control <- trainControl(
      method = "cv",
      savePredictions = 'none',
      returnResamp = 'final',
      index = folds$index,
      indexOut = folds$indexOut,
      p = 0.8
    )
    
    cl <- makePSOCKcluster(availableCores() - 2)
    registerDoParallel(cl)
    
    ffs <- ffs(
      train[, features],
      train$value,
      method = 'xgbLinear',
      metric = 'RMSE',
      tuneGrid = tuneGrid1,
      Control = control,
      verbose = TRUE
    )
    
    on.exit(stopCluster(cl))
    registerDoSEQ()
    
    ffsResults <- ffs$perf_all
    
    write.csv(
      ffsResults,
      sprintf('%s/ffsResults_%s.csv', out_loc, iter))
    
    
    save(
      ffs,
      file = sprintf('%s/ffs_%s.RData', out_loc, iter)
    )
    
    features_1 <- ffsResults[ffsResults$RMSE == min(ffsResults$RMSE), ]
    
    
    if (nrow(features_1) > 1) {
      features_1 <- features_1[features_1$nvar == min(features_1$nvar), ] %>%
        dplyr::select(-c(nvar, RMSE, SE)) %>%
        paste(.) %>% .[. != 'NA']
    } else {
      features_1 <- features_1 %>%
        dplyr::select(-c(nvar, RMSE, SE)) %>%
        paste(.) %>% .[. != 'NA']
    }
    
    write.csv(features_1, sprintf('%s/featuresSelected.csv', out_loc, iter))
    
    
    return(features_1)
  }
  
#-------------------------------------------------------------------------------
 
 
data_partition <- function(df) {
   df$lat_group <- cut_number(df$lat, 2, right = FALSE)
   df$long_group <- cut_number(df$lon, 3, right = FALSE)
   
   df <- df %>%
     mutate(
       date = lubridate::ymd(ActivityStartDate),
       julian = as.numeric(format(date, "%j")),
       value = as.numeric(ResultMeasureValue),
       space_group = paste0(lat_group, long_group)
     ) %>%
     filter(!is.na(value))
   
   df$time_group = cut_number(df$julian, 3, right = FALSE)
   df$mag_c <- cut(
     df$value,
     quantile(
       x = df$value,
       c(0, 0.2, 0.4, 0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 0.93, 0.96, 1),
       include.lowest = TRUE
     )
   )
   
   df$mag <- factor(
     df$mag_c,
     c(0.2, 0.4, 0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 0.93, 0.96, 1)
   )
   
   return(df)
} 

#-------------------------------------------------------------------------------

create_train_validate <- function(df, label_value = NULL, train_partition, validate_partition) {
  if (is.null(label_value)) {
    df_filtered <- df %>% filter(.partitions %in% c(train_partition, validate_partition))
  } else {
    df_filtered <- df %>% filter(label == label_value, .partitions %in% c(train_partition, validate_partition))
  }
  
  train_set <- df_filtered %>%
    filter(.partitions == train_partition) %>%
    ungroup() %>%
    as.data.frame()
  
  validate_set <- df_filtered %>%
    filter(.partitions == validate_partition) %>%
    ungroup() %>%
    as.data.frame()
  
  return(list(train_set = train_set, validate_set = validate_set))
}

#`````````````````````````````````````````````````````````````````````````````````

undersample_filter <- function(block, a, b, low, high) {
  block.location <- block %>%
    group_by(MonitoringLocationIdentifier) %>%
    summarise(Count = n(), TP = mean(ResultMeasureValue))
  
  lowTP.HighCount <- block.location %>%
    filter(TP < a & Count > b) %>%
    mutate(Remove = as.integer(0.30 * Count))  # Get an estimate of undersample
  
  lowTP_matchup <- data.frame()
  
  # Iterate over the sequence of row indices from 1 to the number of rows in lowTP.HighCount
  for (i in 1:nrow(lowTP.HighCount)) {
    location <- lowTP.HighCount$MonitoringLocationIdentifier[i]
    data <- block %>% filter(MonitoringLocationIdentifier == location)
    low.quantile <- quantile(data$ResultMeasureValue, probs = low)
    high.quantile <- quantile(data$ResultMeasureValue, probs = high)
    data.filter <- data %>% filter(ResultMeasureValue > low.quantile & ResultMeasureValue < high.quantile)
    
    lowTP_matchup <- rbind(lowTP_matchup, data.filter)
  }
  
  otherTP_matchup <- block %>%
    filter(!(MonitoringLocationIdentifier %in% lowTP.HighCount$MonitoringLocationIdentifier))
  new.block <- rbind(otherTP_matchup, lowTP_matchup)
  
  return(new.block)
}
#-------------------------------------------------------------------------------

undersampling_function <- function(data_backup, validate, features_1, grid_final, trainmethod) {
  
  R2 <- data.frame(matrix(ncol = 6, nrow = 0))
  
  for (a in seq(0.05, 0.3, 0.05)) {         # Threshold
    for (b in seq(10, 30, 5)) {              # Count
      for (p in seq(0, 90, 5)) {              # Cutoff for undersampling
        
        
        low <- (p / 2) / 100  
        high <- 1 - ((p / 2) / 100)
        
        print(a)
        print(b)
        print(p)
        print(low)
        print(high)
        
        result_block <- undersample_filter(data_backup, a, b, low, high)
        
        train <- result_block 
        model <- train_model(train, features_1, grid_final, trainmethod)
        output <- prediction_function(model, validate)
        
        model <- lm(Predicted ~ Actual, output)
        summary <- summary(model)
        k = summary(model)$r.squared
        R2 <- rbind(R2, data.frame("Threshold" = a, "Count" = b, "Low" = low, "High" = high, "R2" = k, "Train" = nrow(train)))
      }
    }
  }
  
  return(R2)
}


#-------------------------------------------------------------------------------


Smote_train <- function(data_backup, features_2, i, j) {
  
  train_data <- data_backup %>% 
    ungroup() %>%
    as.data.frame()
  
  set.seed(10)  # Set seed outside the loop
  
  train_data <- SmoteRegress(ResultMeasureValue ~ ., train_data[, features_2], thr.rel = i, C.perc = "balance",
                             k = j, dist = "Euclidean")  
  
  train_data.1 <- train_data %>%
    mutate(lat_group = cut_number(lat, 2, right = FALSE),
           long_group = cut_number(lon, 3, right = FALSE),
           value = as.numeric(ResultMeasureValue),
           space_group = paste0(lat_group, long_group),
           time_group = cut_number(julian, 3, right = FALSE)) %>%
    mutate(value = log(value))
  
  return(train_data.1)
}

Smote_function <- function(data_backup, features_1, features_2, grid_final, trainmethod, validate) {
  
  R2 <- data.frame(matrix(ncol = 4, nrow = 0))
  for (i in seq(0.1, 1, by = 0.05)) {
    
    for (j in 10:30) {
      set.seed(10)  # Set seed outside the loop
      
      train_data.1 <- Smote_train(data_backup, features_2, i, j)
      
      model <- train_model(train_data.1, features_1, grid_final, trainmethod)
      output <- prediction_function(model, validate)
      
      model <- lm(Predicted ~ Actual, output)
      k <- summary(model)$r.squared
      
      R2 <- rbind(R2, data.frame(Threshold = i, KNN = j, R2 = k, Count = nrow(train_data.1)))
      print(data.frame(Threshold = i, KNN = j, R2 = k))
    }
  }
  
  return(R2)
}
#-------------------------------------------------------------------------------

set.seed(22)

train_validate <- function(train_data, frac) {
  train_test <- train_data %>%
    group_by(time_group, long_group, mag) %>%
    sample_frac(frac) %>%
    ungroup() 
  
  validate_test <- train_data %>%
    anti_join(train_test)
  
  return(list(train_test = train_test, validate_test = validate_test))
}

#-------------------------------------------------------------------------------

reduce_features_correlation <- function(matchup_data_clean, features, cutoff_value){
  
  reduce_feature <-  matchup_data_clean[, features]

  # Normalize the dataframe
  reduce_feature_normalized <- as.data.frame(scale(reduce_feature))
  
  # Calculate correlation matrix
  correlation_matrix <- cor(reduce_feature_normalized)
  
  # Find highly correlated columns
  highly_correlated <- findCorrelation(correlation_matrix, cutoff = cutoff_value)
  
  # Remove highly correlated columns
  reduce_feature_filtered <- reduce_feature_normalized[, -highly_correlated]
  
  features <- colnames(reduce_feature_filtered)
  
  return(features)
  
}


#-------------------------------------------------------------------------------

grid_best <- function(train, grid_base, features_1){
  
  set.seed(10)
  cl <- makePSOCKcluster(detectCores()-2)
  registerDoParallel(cl)
  folds <- CreateSpacetimeFolds(train, spacevar = "long_group", timevar = "time_group" , k=3)
  
  train_control_final <- caret::trainControl(
    method = "cv",
    savePredictions = T,
    returnResamp = 'final',
    index = folds$index,
    indexOut = folds$indexOut,
    verboseIter = T,
    allowParallel = TRUE,
    p = 0.8)
  
  
  model <- caret::train(
    x = train[,features_1],
    y = train$value,
    trControl = train_control_final,
    tuneGrid = grid_base,
    method = trainmethod,
    importance = T,
    verbose = TRUE)
  
  stopCluster(cl)
  best_hyperparameters <- model$bestTune
  
  return (best_hyperparameters)

}


#-------------------------------------------------------------------------------
# Determine the current working directory

print_result <-function(evals){
  current_directory <- getwd()
  
  # Define the output file path
  output_file_path <- file.path(current_directory, paste0(name, "_results.txt"))
  
  # Convert the tibble to a character vector
  evals_character <- c(
    "Evaluation Metrics:",  # Title
    paste("RMSE:", evals$rmse),
    paste("MAE:", evals$MAE),
    paste("NSE:", evals$NSE),
    paste("Rsquared:", evals$rsquared)
  )
  
  # Write results to the file
  result <- writeLines(evals_character, output_file_path)
  
  return (result)

}

#-------------------------------------------------------------------------------

sampling_train_data <- function(data_backup, features_1, features_2, grid_final, trainmethod, validate) {
  
  # Perform undersampling
  undersampling_results <- undersampling_function(data_backup, validate, features_1, grid_final, trainmethod)
  
  # Perform SMOTE
  Smote_results <- Smote_function(data_backup, features_1, features_2, grid_final, trainmethod, validate)
  
  # Initialize the results list to hold both data frames
  results <- list(
    undersampling = undersampling_results,
    smote = Smote_results
  )
  return(list(
    undersampling = undersampling_results,
    smote = Smote_results
  ))
}
