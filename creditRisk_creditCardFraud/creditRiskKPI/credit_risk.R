library(hrbrthemes)
library(rsample)
library(xgboost)
library(tidyverse)
library(kableExtra)
library(fastDummies)
library(scales)
library(rsample)
library(ROSE)
source('creditRisk_creditCardFraud/xgb_functions.R')


df <-
  read_csv(
    '/Users/jd/Documents/CD3001B/creditRisk_creditCardFraud/credit_risk_dataset.csv'
  ) %>%
  rename_with(., ~ tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%
  map_if(.p = is.character,
         .f = tolower) %>%
  as_tibble %>%
  filter(person_age < 99) %>%
  mutate(flag = case_match(cb_person_default_on_file,
                           "n" ~ 0,
                           "y" ~ 1)) %>%
  select(-cb_person_default_on_file,
         -person_emp_length,
         -loan_int_rate)

df %>%
  head %>%
  kable(.,
        format = "html",
        booktabs = TRUE) %>%
  kable_styling(font_size = 12) %>%
  scroll_box(width = "100%")

df %>%
  glimpse()

df %>%
  ggplot(aes(loan_amnt, fill = loan_grade, colour = loan_grade)) +
  geom_histogram(alpha = 0.5, position = "fill") +
  theme_ipsum() +
  theme(legend.position = 'bottom')

plist <- visualize_all_variables(df)

df <-
  df %>%
  dummy_cols(
    remove_selected_columns = T,
    remove_first_dummy = T,
    select_columns = c("person_home_ownership",
                       "loan_intent",
                       "loan_grade")
  ) %>%
  map_if(
    .p = function(x)
      ! identical(unique(x), c(1, 0)),
    .f = function(x)
      minmax(x)
  ) %>%
  as_tibble

### ROSE

df %>%
  map(.,
      function(x)
        sum(is.na(.)))

df_split <- initial_split(df, prop = 0.80)

train_data <- training(df_split)
test_data <- testing(df_split)

train_data <-
  ROSE(flag ~ ., data = train_data, seed = 123)$data %>%
  as_tibble()

test_data <-
  ROSE(flag ~ ., data = test_data, seed = 123)$data %>%
  as_tibble()

#df_split2 <- initial_split(train_data, prop = 0.80, strata = 'flag')

#train_data <- training(df_split2)
#test_data <- testing(df_split2)

xvars = names(train_data)[names(train_data) != 'flag']
yvar = 'flag'

train_xgb = makeXGBMatrix(train_data, xvars = xvars, yvar = yvar)
test_xgb = makeXGBMatrix(test_data, xvars = xvars, yvar = yvar)
#valid_xgb =  makeXGBMatrix(validation_data, xvars = xvars, yvar = yvar)

finalXGB <-
  searchXGB(train_xgb,
            test_xgb)


obs_pred_train <- obs_pred_df(XGB = finalXGB$finalXGB,
                              XGB_Matrix = train_xgb,
                              y = train_data$flag)


obs_pred_test <- obs_pred_df(XGB = finalXGB$finalXGB,
                             XGB_Matrix = test_xgb,
                             y = test_data$flag)


cm_train <- plot_confusion_matrix(obs_pred_train)
cm_test <- plot_confusion_matrix(obs_pred_test)

train_metrics <- all_model_metrics(obs_pred_train)
test_metrics <- all_model_metrics(obs_pred_test)


roc_table_train <-
  make_roc_table(
    model = finalXGB$finalXGB,
    data = train_xgb,
    yflag = train_data$flag,
    nrows = 200,
    obs_pred_df_raw = obs_pred_df_raw
  )

roc_table_test <-
  make_roc_table(
    model = finalXGB$finalXGB,
    data = test_xgb,
    yflag = test_data$flag,
    nrows = 200,
    obs_pred_df_raw = obs_pred_df_raw
  )


finalXGB$finalXGB$evaluation_log %>% 
  as_tibble %>% 
  pivot_longer(cols = c('train_auc','test_auc'),
               values_to = 'auc',
               names_to = 'sample') %>% 
  ggplot(aes(iter,auc,col=sample,group=sample)) +
  geom_line(alpha=0.6) +
  theme_ipsum() +
  scale_colour_manual(name='',values=c("blue", 'lightblue'))
  


 
  
