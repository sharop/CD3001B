# code to fit an ensemble of shallow decision trees with xgboost
# hyperparameter iterative tuning with random search
# 4-fold sliding window validation for time series


searchXGB <- function(train_xgb, test_xgb) {
  # random search best xgb hyperparams, keeps best result
  
  max_auc_mean = 0
  best_auc_mean = 0
  best_params = list()
  
  for (i in 1:10) {
    
    params = list(
      tree_method = 'exact',
      booster = 'gbtree',
      eta = runif(1, 0.01, 0.3),
      max_depth = sample(5:14, 1),
      lambda = runif(1, 0.01, 0.4),
      alpha = runif(1, 0.01, 0.4),
      gamma = runif(1, 0, 20),
      subsample = runif(1, .5, 1),
      colsample_bytree = runif(1, .5, 1),
      objective = 'binary:logitraw',
      eval_metric = 'auc'
    )
    
    iterXGB <-
      xgb.cv(
        nfold = 10,
        params = params,
        nrounds = 20000,
        data = train_xgb,
        verbose = 2,
        maximize = T,
        early_stopping_rounds = 2
      )
    
    # keep error if it beats the minimum achieved so far //
    max_auc_mean <-
      if_else(
        iterXGB$evaluation_log$test_auc_mean[iterXGB$best_iteration] > max_auc_mean,
        iterXGB$evaluation_log$test_auc_mean[iterXGB$best_iteration],
        max_auc_mean
      )
    
    # update global results and retain all things needed
    if (max_auc_mean > best_auc_mean) {
      best_auc_mean <- max_auc_mean
      best_params <- params
    }
    
  }
  
  finalXGB <-
    
    xgb.train(
      params = best_params,
      nrounds = 20000,
      data = train_xgb,
      verbose = 2,
      maximize = F,
      early_stopping_rounds = 5,
      watchlist = list(train = train_xgb,
                       test = test_xgb)
    )
  
  return(list(finalXGB = finalXGB,
              best_params = best_params))
}

splitTrainTest <- function(df, strat_var = NULL) {
  split <- initial_split(df, prop = .75, strata = strat_var)
  
  return(list(train = training(split),
              test = testing(split)))
  
}

# function to create an XGBoost matrix out of a tibble
makeXGBMatrix <- function(xvars, yvar, dataz) {
  XGBmatrix <-
    dataz %>%
    select(all_of(xvars)) %>%
    as.matrix %>%
    xgb.DMatrix(., label = dataz[[yvar]])
  
  return(XGBmatrix)
  
}

obs_pred_df <- function(y,
                        XGB_Matrix,
                        XGB) {
  return(tibble(
    y = y,
    yhat = ifelse(predict(XGB, XGB_Matrix) < 0.5, 0, 1),
    hit = ifelse(y == yhat, 1, 0)
  ))
  
}

obs_pred_df_raw <- function(y, cutoff, XGB_Matrix, XGB) {
  return(tibble(
    y = y,
    yhat = ifelse(predict(XGB, XGB_Matrix)  > cutoff, 1, 0),
    hit = ifelse(y == yhat, 1, 0)
  ))
  
}

plot_confusion_matrix <- function(df,
                                  title = '') {
  return(
    df %>%
      count(y, yhat, sort = T) %>%
      mutate(y = as_factor(y),
             yhat = as_factor(yhat)) %>%
      ggplot(aes(y, yhat)) +
      geom_tile(aes(fill = n), colour = "white") +
      geom_text(
        aes(label = sprintf("%1.0f", n)),
        vjust = 1,
        colour = 'white',
        size = 12
      ) +
      theme_bw() +
      scale_fill_gradient(low = "blue", high = "lightblue") +
      theme(legend.position = "none") +
      ylab("Predicted Labels") +
      xlab("Observed Labels") +
      ggtitle(title) +
      theme(legend.position = "none")
  )
}

all_model_metrics <- function(df) {
  return(
    tibble(
      auc = ModelMetrics::auc(df$y, df$yhat),
      f1 = ModelMetrics::f1Score(df$y, df$yhat),
      precision = ModelMetrics::precision(df$y, df$yhat),
      recall =  ModelMetrics::recall(df$y, df$yhat),
      accuracy = sum(df$hit) / nrow(df)
      
    )
  )
  
}

visualize_all_variables <- function(data) {
  # Create an empty list to store the visualizations
  plot_list <- list()
  
  # Loop through each column in the dataset
  
  for (col in colnames(data)) {
    # Determine the type of variable and create a corresponding plot
    
    if (is.numeric(data[[col]])) {
      plot_list[[col]] <-
        ggplot(data, aes(x = !!sym(col))) +
        geom_bar(stat = 'count',
                 colour = 'lightblue',
                 alpha = 0.6) +
        theme_ipsum() +
        theme(
          text = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white", size = 12),
          axis.text.y = element_text(colour = "white", size = 12),
          legend.title = element_text(colour = "white", size = 12)
          
        )
      
    } else if (is.character(data[[col]])) {
      plot_list[[col]] <-
        ggplot(data, aes(x = !!sym(col))) +
        geom_bar(colour = 'lightblue', alpha = 0.6) +
        theme_ipsum() +
        theme(
          text = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white", size = 12),
          axis.text.y = element_text(colour = "white", size = 12),
          legend.title = element_text(colour = "white", size = 12)
          
        )
      
    } else {
      plot_list[[col]] <-
        ggplot(data, aes(x = !!sym(col))) +
        geom_density(colour = 'lightblue', alpha = 0.6) +
        theme_ipsum() +
        theme(
          text = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white", size = 12),
          axis.text.y = element_text(colour = "white", size = 12),
          legend.title = element_text(colour = "white", size = 12)
          
        )
      
    }
  }
  
  return(plot_list)
}

minmax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

make_roc_table <- function(model,
                           data,
                           yflag,
                           nrows,
                           obs_pred_df_raw) {
  upper_limit <- max(predict(model, data))
  
  lower_limit <- min(predict(model, data))
  
  by <- (upper_limit - lower_limit) / nrows
  
  intervals <- seq(from = lower_limit,
                   to = upper_limit,
                   by = by)
  
  TN = c()
  FN = c()
  FP = c()
  TP = c()
  precision = c()
  TPR_recall = c()
  FPR = c()
  lift <- c()
  threshold <- c()
  N <- c()
  f1 <- c()
  
  p <- sum(yflag)
  n <- length(yflag)
  
  for (cutoff in intervals) {
    y_yhat <- obs_pred_df_raw(
      y = yflag,
      cutoff = cutoff,
      XGB_Matrix = data,
      XGB = model
    )
    
    TN_ =
      y_yhat %>%
      filter((y == 0) & (yhat == 0)) %>%
      nrow
    
    FN_ =
      y_yhat %>%
      filter((y == 1) & (yhat == 0)) %>%
      nrow
    
    FP_ =
      y_yhat %>%
      filter((y == 0) & (yhat == 1)) %>%
      nrow
    
    TP_ =
      y_yhat %>%
      filter((y == 1) & (yhat == 1)) %>%
      nrow
    
    f1_ = TP_ / (TP_ + ((FP_ + FN_) * 0.5))
    
    precision_ = TP_ / (FP_ + TP_)
    
    N_ = sum(TN_, FN_, FP_, TP_)
    
    lift_ = precision_ / (p / n)
    
    FPR_ = FP_ / (FP_ + TN_)
    
    TPR_recall_ = TP_ / (TP_ + FN_)
    
    threshold = c(threshold,
                  cutoff)
    
    TN = c(TN, TN_)
    FN = c(FN, FN_)
    FP = c(FP, FP_)
    TP = c(TP, TP_)
    precision = c(precision, precision_)
    TPR_recall = c(TPR_recall, TPR_recall_)
    f1 = c(f1, f1_)
    FPR = c(FPR, FPR_)
    lift = c(lift, lift_)
    N = c(N, N_)
    
  }
  
  
  return (tibble(threshold, TP, FP, FN, TN, N, precision, TPR_recall, f1, FPR, lift))
  
  
}


plot_cm_slider <- function(TN, FP, FN, TP) {
  observed <- factor(c(0, 0, 1, 1))
  predicted <- factor(c(0, 1, 0, 1))
  Y <- c(TN, FP, FN, TP)
  return(
    data.frame(observed,
               predicted, Y) %>%
      ggplot(aes(x = observed, y = predicted)) +
      geom_tile(aes(
        fill = Y,
        colour = "white"
      )) +
      geom_text(
        aes(label = sprintf("%1.0f", Y)),
        vjust = 1,
        colour = 'white',
        size = 12
      ) +
      scale_fill_gradient(low = "blue", high = "lightblue") +
      theme_bw() +
      theme(legend.position = "none")
  )
}

make_shap_viz <-
  function(dataz, finalXGB, yvar, shap_viz_name, ...) {
    blues <-
      c(
        "#F7FBFF",
        "#DEEBF7" ,
        "#C6DBEF" ,
        "#9ECAE1",
        "#6BAED6" ,
        "#4292C6",
        "#2171B5",
        "#08519C",
        "#08306B"
        )
    
    dataMatrix <-
      dataz %>%
      select(!(all_of(yvar))) %>%
      as.matrix
    
    shap_viz <-
      xgb.ggplot.shap.summary(model = finalXGB,
                              top_n = 20,
                              data = dataMatrix)
    
    total_length <-
      max(shap_viz[[1]]$feature_value) - min(shap_viz[[1]]$feature_value)
    increase <- total_length / 10
    #
    shap_viz <-
      xgb.ggplot.shap.summary(model = finalXGB,
                              top_n = 20,
                              data = dataMatrix) +
      theme_ipsum() +
      scale_color_gradientn(
        colours = blues,
        name = "Variables Independientes",
        breaks = c(
          min(shap_viz[[1]]$feature_value) + increase,
          max(shap_viz[[1]]$feature_value) - increase
        ),
        labels = c("low", "high")
      ) +
      labs(y = yvar,
           x = 'Dimension') +
      theme(legend.position = 'bottom') +
      theme(
        text = element_text(colour = "white", size = 12),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        legend.title = element_text(colour = "white")
        
      )
    
    return(shap_viz)
    
  }
