library(xgboost)
library(tidyverse)
library(rsample)

install.packages('rsample')


df <- read_csv('creditRisk_creditCardFraud/data_fraude.csv')

df <- 
df %>% 
  select(-...1) %>% 
  rename(`0` = `0...2`,
         'Class' = `0...21`)

split <- initial_split(df, prop = 0.80)

train <- training(split)
test <- testing(split)

makeXGBMatrix <- function(xvars, yvar, df){
  
  XGBmatrix <-
    df %>% 
    select(all_of(xvars)) %>% 
    as.matrix %>% 
    xgb.DMatrix(., 
                label=df[[yvar]])
  
  return(XGBmatrix)
  
}

trainXGB <- 
makeXGBMatrix(
  xvars = (df %>% names)[df %>% names != 'Class'],
  yvar = 'Class',
  df = train
)

testXGB <- 
  makeXGBMatrix(
    xvars = (df %>% names)[df %>% names != 'Class'],
    yvar = 'Class',
    df = test
  )

fitXGB <- function(trainXGB,
                   testXGB,
                   iter){
  
  best_metric <- 0
  best_params <- list()
    
  for (iteration in 1:iter){
    
    params <- list(
      tree_method = 'exact',
      booster = 'gbtree', # 'gblinear'
      eta = runif(1, 0.01, 0.3), 
      max_depth = sample(5:14, 1),
      lambda = runif(1, 0.01, 0.4),
      alpha = runif(1, 0.01, 0.4),
      gamma = runif(1, 0, 20),
      subsample = runif(1, .5, 1),
      colsample_bytree = runif(1, .5, 1),
      objective = 'binary:logistic',
      eval_metric = 'auc'
    )
    
    xgb_cv <- 
    xgb.cv(
      nfold = 10,
      params = params,
      nrounds = 20000,
      data = trainXGB,
      verbose = 2,
      maximize = T,
      early_stopping_rounds = 2
    )
    
    auc = xgb_cv$evaluation_log$test_auc_mean[xgb_cv$best_iteration]
    
    if(auc > best_metric){
      best_metric = xgb_cv$evaluation_log$test_auc_mean[xgb_cv$best_iteration]
      best_params = params
    }
  }
    
  finalXGB <-
    xgb.train(
      params = best_params,
      nrounds = 20000,
      data = trainXGB,
      verbose = 2,
      maximize = F,
      early_stopping_rounds = 5,
      watchlist = list(train = trainXGB,
                       test = testXGB)
    )
  
  return(list(finalXGB = finalXGB,
              best_params = best_params))
}

}






