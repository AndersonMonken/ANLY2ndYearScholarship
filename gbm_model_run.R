
library("gbm")
library(RColorBrewer)
library(caret)
library(parallel)
library(dplyr)
library(tidyr)

param_list <- list(interaction.depth = c(1,2,3,4,5,6,7,8), 
                   shrinkage = c(0.01,0.0075,0.005,0.0025),
                   ntree = c(seq(1,50,5),seq(50,2500,50))
)
param_df_gbm <- do.call(expand.grid, param_list) %>% mutate(test_mse = 0, train_mse = 0)

gbm_run <- function(i){
  mergedata <- readRDS('model_data.rds')
  set.seed(67645)
  
  
  param_list <- list(interaction.depth = c(1,2,3,4,5,6,7,8), 
                     shrinkage = c(0.01,0.0075,0.005,0.0025),
                     ntree = c(seq(1,50,5),seq(50,2500,50))
  )
  param_df_gbm <- do.call(expand.grid, param_list) %>% mutate(test_mse = 0, train_mse = 0)
  
  splitIndex <- sample(1:5, nrow(mergedata), replace = TRUE)
  
  list_return <- list()
  temp_test_mse <- 0
  for (iter in 1:5){
    
    test  <- mergedata[splitIndex == iter,]
    train <- mergedata[splitIndex != iter,]
    
    gbm_model <- gbm(cancer_per100k ~ ., 
                     data = train, 
                     distribution = 'gaussian', 
                     n.trees = param_df_gbm[i,'ntree'], 
                     interaction.depth = param_df_gbm[i,'interaction.depth'], 
                     shrinkage = param_df_gbm[i,'shrinkage'])
    
    
    pred_test <- predict(gbm_model,newdata = test, n.trees = param_df_gbm[i,'ntree'])
    temp_test_mse <- temp_test_mse + sum((pred_test - test$cancer_per100k)^2/nrow(test), na.rm = TRUE)
  }
  list_return$test_mse <- temp_test_mse / 5
  
  return(list_return)
}

###### WARNING DON'T RUN THIS CODE UNLESS YOU HAVE A LOT OF CORES
result_list_gbm <- mclapply(X = 1:nrow(param_df_gbm), FUN = gbm_run, mc.cores = 24)


# Gathering results
model_vec <- c()
test_mse_vec <- c()
train_mse_vec <- c()
for (i in 1:length(result_list_gbm)){
  model_vec <- c(model_vec, result_list_gbm[[i]][['model']])
  test_mse_vec <- c(test_mse_vec, result_list_gbm[[i]][['test_mse']])
  train_mse_vec <- c(train_mse_vec, result_list_gbm[[i]][['train_mse']])
}
decision_df_gbm <- param_df_gbm
decision_df_gbm['test_mse'] <- test_mse_vec
decision_df_gbm['train_mse'] <- train_mse_vec


saveRDS(decision_df_gbm,'gbmresult.rds')
