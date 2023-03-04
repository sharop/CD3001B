library(caret)
library(tidyverse)
library(xgboost)
library(rsample)
library(hrbrthemes)

# load all supervised modelling functions
source('supervisedModellingXGBOOST/cross_validation_xgb_binaryclassification.R')
source('supervisedModellingXGBOOST/cross_validation_xgb_linear_regression.R')
source('supervisedModellingXGBOOST/cross_validation_xgb_multiclass.R')
source('supervisedModellingXGBOOST/fitXGBoost.R')

# pre-process all data

df_train <-
  read_csv('/Users/jd/Documents/CD3001B/profession_classification/train.csv') %>%
  select(-Var_1, -Segmentation) %>%
  mutate(
    Ever_Married = recode(Ever_Married, 'No' = 0, 'Yes' = 1),
    Profession = recode(
      Profession,
      'Healthcare' = 0,
      'Engineer' = 1,
      'Lawyer' = 2,
      'Entertainment' = 3,
      'Artist' = 4,
      'Executive' = 5,
      'Doctor' = 6,
      'Homemaker' = 7,
      'Marketing' = 8
    )
  ) %>%
  drop_na(Profession)

df_test <-
  read_csv('/Users/jd/Documents/CD3001B/profession_classification/test.csv') %>%
  select(-Var_1) %>%
  mutate(
    Ever_Married = recode(Ever_Married, 'No' = 0, 'Yes' = 1),
    Profession = recode(
      Profession,
      'Healthcare' = 0,
      'Engineer' = 1,
      'Lawyer' = 2,
      'Entertainment' = 3,
      'Artist' = 4,
      'Executive' = 5,
      'Doctor' = 6,
      'Homemaker' = 7,
      'Marketing' = 8
    )
  ) %>%
  drop_na(Profession)

# split train/test if needs be

train <- training(split)
test <- testing(split)

xvars <- (df_train %>% names)[df_train %>% names != 'Profession']
yvar <- 'Profession'

# create xgboost matrices

xgbTrain <- makeXGBMatrix(xvars = xvars,
                          yvar = yvar,
                          df = df_train)

xgbTest <- makeXGBMatrix(xvars = xvars,
                         yvar = yvar,
                         df = df_test)

modelo <- fitXGB(xgbTrain,
                 xgbTest,
                 iterations = 5,
                 model_type = 'multiclass')

# visualise model performance with confusion matrix and error visualisation

cm <-
  caret::confusionMatrix(as_factor(predict(modelo, xgbTest)), as_factor(df_test$Profession))

plot <-
  visualise_error(evaluation_log = modelo$evaluation_log,
                  error_metric = 'mlogloss')