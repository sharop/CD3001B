## install.packages(c('tidyverse', 'xgboost', 'rsample')) ## update later

library(tidyverse)
library(xgboost)
library(rsample)

df <- 
  read_csv('/Users/maugarciagarza/Downloads/data_fraude.csv') %>% 
  select(-X1)

df$`0_1`

df <- 
df %>% 
  rename("fraude" = "0_1")

split <- initial_split(df, prop = 0.80)

train <- training(split)
test <- testing(split)

xvars <- (df %>% names)[df %>% names != 'fraude']
yvar <- 'fraude'

makeXGBMatrix <- function(xvars, yvar, df){
  
  XGBmatrix <-
    df %>% 
    select(all_of(xvars)) %>% 
    as.matrix %>% 
    xgb.DMatrix(., 
                label=df[[yvar]])
  
  return(XGBmatrix)
  
}

xgbTrain <- makeXGBMatrix(xvars=xvars,
                          yvar=yvar,
                          df=train)

xgbTest <- makeXGBMatrix(xvars=xvars,
                          yvar=yvar,
                          df=test)

fitxqboost <- function(xgbTrain, xgbTest, iterations){
  auc = 0
  params = list()
  
  for (i in 1:iterations){
  hparams = list(
    booster = 'gbtree',
    objective = 'binary:logistic',
    eval_metric = 'auc',
    eta = runif(1, 0.01, 0.3),
    lambda = runif(1, 0.01, 0.4),
    alpha = runif(1, 0.01, 0.4),
    gamma = runif(1, 0, 20),
    max_depth = sample(5:14,1), 
    subsample = runif(1,0.5,1),
    colsample_bytree = runif(1, 0.5, 1)
  )  
  xgbCV <- xgb.cv(
    params = hparams, 
    nfold = 10,
    nrounds = 10000,
    early_stopping_rounds = 2,
    maximize = TRUE,
    data = xgbTrain,
    verbose = 2
  )
  auc2 <- xgbCV$evaluation_log[xgbCV$best_iteration]$test_auc_mean
  if(auc2 > auc){
    auc = auc2
    params = hparams
  }
  }
 finalmodel <-  xgb.train(
    params= params,
    data=xgbTrain,
    nrounds=50,
    early_stopping_rounds=3,
    watchlist = list(training = xgbTrain,
                     testing = xgbTest),
    maximize=T
  )
  return(finalmodel)
}

modelo <- fitxqboost(xgbTrain, xgbTest, iterations = 10)

modelo$evaluation_log %>% 
  pivot_longer(cols = c('training_auc','testing_auc'),
               names_to = 'sample',
               values_to = 'value') %>% 
  ggplot(aes(iter, value, group=sample, colour=sample)) +
  geom_line()



      
