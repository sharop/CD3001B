## install.packages(c('tidyverse', 'xgboost', 'rsample')) ## update later

library(tidyverse)
library(xgboost)
library(rsample)

# leer datos

df <- 
  read_csv('/Users/jd/Documents/CD3001B/income_regression/data.csv')

# pre-process datos (si es necesario)

# dividir train / test 

split <- initial_split(df, prop = 0.80)

train <- training(split)
test <- testing(split)

# crear matrices de xgboost

xvars <- (df %>% names)[df %>% names != 'fnlwgt']
yvar <- 'fnlwgt'


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

fitxgboost <- function(xgbTrain, xgbTest, iterations){
  
  # iniciar el valor de la métrica del cual partimos (alto para métricas que queremos reducir
  # y viceversa)
  
  rmse = Inf
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
    objective = 'reg:squarederror', 
    eval_metric = 'rmse',
    params = hparams, 
    nfold = 10,
    nrounds = 10000,
    early_stopping_rounds = 2,
    maximize = F,
    data = xgbTrain,
    verbose = 2
  )
  
  # registrar la métrica
  
  rmse2 <- xgbCV$evaluation_log[xgbCV$best_iteration]$test_rmse_mean
  
  # si la métrica alcanzada fuera mejor que la actual, reemplazar la actual  y guardar los
  # hiperparámetros del modelo que llegó a ella.
  # Nota : la métrica debe ser menor si se esta reduciendo (ej rmse) o mayor si se esta aumentando (ej auc)
  
  
  if(rmse2 < rmse){
    rmse = rmse2
    params = hparams
  }
  }
  
  # Ajustar un modelo final con los mejores hiperparámetros, probándolo ahora en el test set
  
 finalmodel <-  xgb.train(
    params= params,
    data=xgbTrain,
    nrounds=50,
    early_stopping_rounds=3,
    watchlist = list(training = xgbTrain,
                     testing = xgbTest),
    maximize=F
  )
  return(finalmodel)
}

# correr la función 

modelo <- fitxgboost(xgbTrain, xgbTest, iterations = 10)

# visualizar el error en la muestra de training y test durante el entrenamiento

modelo$evaluation_log %>% 
  pivot_longer(cols = c('training_rmse','testing_rmse'),
               names_to = 'sample',
               values_to = 'value') %>% 
  ggplot(aes(iter, value, group=sample, colour=sample)) +
  geom_line()



      
