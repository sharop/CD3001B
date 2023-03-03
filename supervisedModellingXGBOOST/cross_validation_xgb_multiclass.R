## install.packages(c('tidyverse', 'xgboost', 'rsample')) ## update later

library(tidyverse)
library(xgboost)
library(rsample)
library(hrbrthemes)

# leer datos

df_train <- 
  read_csv('/Users/jd/Documents/CD3001B/profession_classification/train.csv') %>% 
  select(-Var_1,-Segmentation) %>% 
  mutate(Ever_Married = recode(Ever_Married, 'No'= 0,'Yes'= 1),
         Profession = recode(Profession, 'Healthcare'=0,
                                          'Engineer'=1,     
                                          'Lawyer'=2,       
                                          'Entertainment'=3,
                                          'Artist'=4,
                                          'Executive'=5,    
                                          'Doctor'=6, 
                                          'Homemaker'=7,  
                                          'Marketing'= 8
                             )
         ) %>% 
  drop_na(Profession)

df_test <- 
  read_csv('/Users/jd/Documents/CD3001B/profession_classification/test.csv') %>% 
  select(-Var_1) %>% 
  mutate(Ever_Married = recode(Ever_Married, 'No'= 0,'Yes'= 1),
         Profession = recode(Profession, 
                             'Healthcare'=0,
                             'Engineer'=1,     
                             'Lawyer'=2,       
                             'Entertainment'=3,
                             'Artist'=4,
                             'Executive'=5,    
                             'Doctor'=6, 
                             'Homemaker'=7,  
                             'Marketing'= 8
         )
  ) %>% 
  drop_na(Profession)

# pre-process datos (si es necesario)


# crear matrices de xgboost

xvars <- (df_train %>% names)[df_train %>% names != 'Profession']
yvar <- 'Profession'


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
                          df=df_train)

xgbTest <- makeXGBMatrix(xvars=xvars,
                          yvar=yvar,
                          df=df_test)

fitxgboost <- function(xgbTrain, xgbTest, iterations){
  
  # iniciar el valor de la métrica del cual partimos (alto para métricas que queremos reducir
  # y viceversa)
  
  logloss = 0
  params = list()
  
  for (i in 1:iterations){
    
    # muestrear números de forma aleatoria (dentro de un rango) para los hiperparámetros 
    # cada iteración 
      
    hparams = list(
      eta = runif(1, 0.01, 0.3),
      lambda = runif(1, 0.01, 0.2),
      alpha = runif(1, 0.01, 0.2),
      gamma = runif(1, 0, 20),
      max_depth = sample(5:14,1), 
      subsample = runif(1,0.5,1),
      colsample_bytree = runif(1, 0.5, 1)
  )  


    # ajustar un modelo utilizando validación cruzada para probar los hiperparámetros en
    # todas las regiones de los datos de entrenamiento
    
  xgbCV <- xgb.cv(
    booster = 'gbtree',
    objective = 'multi:softmax', 
    eval_metric = 'mlogloss',
    params = hparams, 
    nfold = 10,
    nrounds = 10000,
    early_stopping_rounds = 2,
    maximize = F,
    data = xgbTrain,
    verbose = 2,
    num_class = 9
  )
  
  # registrar la métrica
  
  logloss2 <- xgbCV$evaluation_log[xgbCV$best_iteration]$test_mlogloss_mean
  
  # si la métrica alcanzada fuera mejor que la actual, reemplazar la actual  y guardar los
  # hiperparámetros del modelo que llegó a ella.
  # Nota : la métrica debe ser menor si se esta reduciendo (ej rmse) o mayor si se esta aumentando (ej auc)
  
  
  if(logloss2 < logloss){
    logloss2 = logloss
    params = hparams
  }
  }
  
  # Ajustar un modelo final con los mejores hiperparámetros, probándolo ahora en el test set
  
 finalmodel <-  xgb.train(
    params= params,
    data=xgbTrain,
    nrounds=50,
    early_stopping_rounds=3,
    num_class = 9,
    watchlist = list(training = xgbTrain,
                     testing = xgbTest),
    maximize=F
  )
  return(finalmodel)
}

# correr la función 

modelo <- fitxgboost(xgbTrain, xgbTest, iterations = 50)

# visualizar el error en la muestra de training y test durante el entrenamiento

modelo$evaluation_log %>% 
  pivot_longer(cols = c('training_mlogloss','testing_mlogloss'),
               names_to = 'sample',
               values_to = 'value') %>% 
  ggplot(aes(iter, value, group=sample, colour=sample)) +
  geom_line() +
  theme_bw()


library(caret)

caret::confusionMatrix(as_factor(predict(modelo, xgbTest)), as_factor(df_test$Profession))

      
