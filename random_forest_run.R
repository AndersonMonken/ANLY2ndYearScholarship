library(randomForest)
library(randomForestExplainer)
library(caret)
library(parallel)
library(dplyr)
library(tidyr)
map_data <- readRDS('map_data.rds') %>% as.data.frame()
map_data_df <- map_data %>% dplyr::select(cancer_per100k, sum_site_score,num_sites, state, pp_renewable, pp_carbon, num_pp, sum_capacity_mw, capacity_mw_renewable, capacity_mw_carbon, region, epa_region) %>% filter(!is.na(cancer_per100k)) %>% mutate(state = as.factor(state), region = as.factor(region)) %>% drop_na()

saveRDS(map_data_df, 'model_data.rds')

library(randomForest)
library(randomForestExplainer)
library(caret)
library(parallel)
library(dplyr)
library(tidyr)
map_data <- readRDS('map_data.rds') %>% as.data.frame()
map_data_df <- map_data %>% dplyr::select(cancer_per100k, sum_site_score,num_sites, state, pp_renewable, pp_carbon, num_pp, sum_capacity_mw, capacity_mw_renewable, capacity_mw_carbon, region, epa_region) %>% filter(!is.na(cancer_per100k)) %>% mutate(state = as.factor(state), region = as.factor(region)) %>% drop_na()

saveRDS(map_data_df, 'model_data.rds')



param_list <- list(mtry = c(4,6,11), 
                   ntree = c(seq(1,149,1),seq(150,500,5)))
param_df_forest <- do.call(expand.grid, param_list) %>% mutate(test_mse = 0, train_mse = 0)


forest_run <- function(i){
  map_data_df <- readRDS('model_data.rds')
  set.seed(67645)
  splitIndex <- sample(1:5, nrow(map_data_df), replace = TRUE)
  
  temp_test_mse <- 0
  list_return <- list()
  param_list <- list(mtry = c(4,6,11), 
 ntree = c(seq(1,149,1),seq(150,500,5)))
 param_df_forest <- do.call(expand.grid, param_list) %>% mutate(test_mse = 0)
 for (iter in 1:5){
   test_df  <- map_data_df[splitIndex == iter,]
   train_df <- map_data_df[splitIndex != iter,]
   
   randomForest_model <- randomForest(cancer_per100k ~ ., 
                                      data = train_df, 
                                      mtry = param_df_forest[i,'mtry'], 
                                      ntree = param_df_forest[i,'ntree'],
                                      localImp = TRUE)
   
   pred_test <- predict(randomForest_model,newdata = test_df)
   temp_test_mse <- temp_test_mse +  sum((pred_test - test_df$cancer_per100k)^2/nrow(test_df), na.rm = TRUE)
 }
 list_return$test_mse <- temp_test_mse / 5
 
 return(list_return)
}
###### WARNING DON'T RUN THIS CODE UNLESS YOU HAVE A LOT OF CORES
result_list_forest <- mclapply(X = 1:nrow(param_df_forest), FUN = forest_run, mc.cores = 24)


# Gathering results
model_vec <- c()
test_mse_vec <- c()
train_mse_vec <- c()
for (i in 1:length(result_list_forest)){
  model_vec <- c(model_vec, result_list_forest[[i]][['model']])
  test_mse_vec <- c(test_mse_vec, result_list_forest[[i]][['test_mse']])
  train_mse_vec <- c(train_mse_vec, result_list_forest[[i]][['train_mse']])
}
decision_df <- param_df_forest
decision_df['test_mse'] <- test_mse_vec
decision_df['train_mse'] <- train_mse_vec

saveRDS(decision_df,'forestresult.rds')
