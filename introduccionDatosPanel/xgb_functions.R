# code to fit an ensemble of shallow decision trees with xgboost
# hyperparameter iterative tuning with random search
# 4-fold sliding window validation for time series


searchXGB <- function(xgb_folds,final_model_xgb){
  
  # random search best xgb hyperparams, keeps best result
  
  min_rmse_mean = Inf
  best_rmse_mean = Inf
  best_params = list()
  
  for(i in 1:20){
    
    params = list(
      booster = 'gbtree',
      eta = runif(1, 0.1, 0.3),
      max_depth = sample(6:8, 1),
      lambda = runif(1, 0.01, 0.4),
      alpha = runif(1, 0.01, 0.4),  
      gamma = runif(1, 0, 20),
      subsample = runif(1, .5, 1),
      colsample_bytree = runif(1, .5, 1),
      objective = 'reg:squarederror',
      eval_metric='rmse'
    )
    
    fold_rmse <- list()
    
    for (fold in 1:length(xgb_folds)){
      
      iterXGB <-
        xgb.train(
          params = params,
          nrounds = 20000,
          data = xgb_folds[[fold]]$train,
          verbose = 2,
          maximize = F,
          early_stopping_rounds = 2,
          watchlist = list(
            test = xgb_folds[[fold]]$test)
        )
      
      fold_rmse[[fold]] <- iterXGB$evaluation_log[iterXGB$best_iteration]$test_rmse
      
    }
    
    rmse_mean <- sum(unlist(fold_rmse))/length(fold_rmse)
    
    # keep error if it beats the minimum achieved so far // 
    min_rmse_mean <-
      if_else(
        rmse_mean < min_rmse_mean,
        rmse_mean,
        min_rmse_mean)
    
    # update global results and retain all things needed
    if(min_rmse_mean < best_rmse_mean){
      best_rmse_mean <- min_rmse_mean
      best_params <- params
    }
    
  }

  finalXGB <-
    
    xgb.train(
      params = best_params,
      nrounds = 20000,
      data = final_model_xgb$train,
      verbose = 2,
      maximize = F,
      early_stopping_rounds = 5,
      watchlist = list(train = final_model_xgb$train,
                       test = final_model_xgb$test)
    )
  
  return(
    list(
      finalXGB = finalXGB,
      best_params = best_params)
  )  
}

splitTrainTest <- function(df,strat_var=NULL){
  
  split <- initial_split(df, prop = .75, strata=strat_var) 
  
  return(list(train = training(split),
              test = testing(split)
    )
  )
  
}

# function to create an XGBoost matrix out of a tibble
makeXGBMatrix <- function(xvars, yvar, dataz){
  
  XGBmatrix <-
    dataz %>% 
    select(all_of(xvars)) %>% 
    as.matrix %>% 
    xgb.DMatrix(., label=dataz[[yvar]])
  
  return(XGBmatrix)
  
}

make_shap_viz <- function(dataz, finalXGB, yvar, shap_viz_name, ...){
  
  dataMatrix <-
    dataz %>% 
    select(-(yvar)) %>% 
    as.matrix
  
  xgb.ggplot.shap.summary(
    model = finalXGB,
    top_n = 20,
    data = dataMatrix) +
    theme_ipsum() +
    labs(y = yvar,
         x = 'Características de la Venta') +
    #ggtitle(shap_viz_name) +
    theme(
      text = element_text(colour="lightgray"),
      axis.text.x = element_text(
        colour="lightgray"),
      axis.text.y = element_text(
        colour="lightgray"),
      legend.title=element_text(
        colour="lightgray")
      
    ) +
    labs(colour = "") +
    scale_x_discrete(label=abbreviate)
    
}