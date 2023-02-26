install.packages(c('tidyverse', 'xgboost', 'rsample'))

library(tidyverse)
library(xgboost)
library(rsample)

df <- 
  read_csv('../../Desktop/data_fraude.csv') %>% 
  select(-...1)

df <- 
df %>% 
  rename("zero" = "0...2",
         'flag'= "0...21")

split <- initial_split(df, prop = 0.80)

train <- training(split)
test <- testing(split)

xvars <- (df %>% names)[df %>% names != 'flag']
yvar <- 'flag'

makeXGBMatrix <- function(xvars, yvar, df){
  
  XGBmatrix <-
    df %>% 
    select(all_of(xvars)) %>% 
    as.matrix %>% 
    xgb.DMatrix(., label=df[[yvar]])
  
  return(XGBmatrix)
  
}

xgbTrain <- makeXGBMatrix(xvars=xvars,
                          yvar=yvar,
                          df=train)

xgbTest <- makeXGBMatrix(xvars=xvars,
                          yvar=yvar,
                          df=test)

t <-
xgb.train(
  params=list(
    booster='gbtree',
    eta=0.3,
    gamma=5,
    max_depth=6,
    subsample=0.8,
    lambda=0.001,
    alpha=0.001,
    objective='reg:logistic',
    eval_metric='auc'
  ),
  data=xgbTrain,
  nrounds=50,
  early_stopping_rounds=,
  watchlist = list(training = xgbTrain,
                   testing = xgbTest),
  maximize=T
)

t$evaluation_log %>% 
  pivot_longer(cols = c('training_auc','testing_auc'),
               names_to = 'sample',
               values_to = 'value') %>% 
  ggplot(aes(iter, value, group=sample, colour=sample)) +
  geom_line()
      
  )
