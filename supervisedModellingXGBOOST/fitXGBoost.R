# fit model with crossvalidation (choose model type between regression, binary or multiclass)

fitXGB <- function(xgbTrain, xgbTest, iterations, model_type){
  
  if(model_type=='regression'){
    
    return(fitXGBRegression(xgbTrain=xgbTrain, xgbTest=xgbTest, iterations=iterations))
    
  }
  
  else if(model_type=='binary'){
    
    return(fitXGBBinary(xgbTrain=xgbTrain, xgbTest=xgbTest, iterations=iterations))
    
  }
  
  else if(model_type=='multiclass'){
    
    return(fitXGBMulticlass(xgbTrain=xgbTrain, xgbTest=xgbTest, iterations=iterations))
    
  }
  
}

visualise_error <- function(evaluation_log, error_metric){

  if(error_metric == 'rmse'){
    
    return(evaluation_log %>% 
             pivot_longer(cols = c('training_rmse','testing_rmse'),
                          names_to = 'sample',
                          values_to = 'value') %>% 
             ggplot(aes(iter, value, group=sample, colour=sample)) +
             geom_line()
           )
    
  }
   
    else if(error_metric == 'auc'){
      
      return(evaluation_log%>% 
               pivot_longer(cols = c('training_auc','testing_auc'),
                            names_to = 'sample',
                            values_to = 'value') %>% 
               ggplot(aes(iter, value, group=sample, colour=sample)) +
               geom_line()
      )
      
    }
    
    else if(error_metric == 'mlogloss'){
      
      return(evaluation_log %>% 
               pivot_longer(cols = c('training_mlogloss','testing_mlogloss'),
                            names_to = 'sample',
                            values_to = 'value') %>% 
               ggplot(aes(iter, value, group=sample, colour=sample)) +
               geom_line()
      )
      
    }

}