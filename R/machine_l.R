#Analysis
set.seed(0220)
holdout_indices <- createDataPartition(data$engagement,
                                       p = 0.25,
                                       list = FALSE) # Set list to FALSE for numeric indices
test_tbl <- data[holdout_indices, ]
training_tbl <-data[-holdout_indices, ]

training_folds <-createFolds(data$engagement)


##create a table2_tbl
# Set up parallel processing
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

time_par <- function(model, training_tbl, na.pass) {
  tic()  # Start the timer
  model_fit <- train(engagement ~ p_support + m_support + sleep_q + justice, 
                     data = training_tbl, 
                     method = model,
                     preProcess = c("center"),
                     trControl = trainControl(method = "cv", 
                                              number = 10, 
                                              verboseIter = TRUE)
  )
  time_taken <- toc(log = TRUE)  # End the timer and store the elapsed time
  return(time_taken$toc - time_taken$tic)  # Return the elapsed time
}


#Calculated the time for each model
model_lm_time_par <- time_par("lm", training_tbl)
model_net_time_par <- time_par("glmnet", training_tbl)
model_rf_time_par <- time_par("rf", training_tbl)
model_xgb_time_par <- time_par("xgbLinear", training_tbl)

