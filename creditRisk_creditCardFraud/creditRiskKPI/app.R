#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(thematic)

thematic_shiny(font = "auto")
  ui <- 
    fluidPage(
      navbarPage(
        theme = bslib::bs_theme(bootswatch = 'slate'
        ),
        '',
        tabPanel(div(img(src = "binoculars.png")),
                 fluidRow(
                   column(12, align="center",
                          tags$pre("




 ██████╗██████╗ ███████╗██████╗ ██╗████████╗    ██████╗ ██╗███████╗██╗  ██╗
██╔════╝██╔══██╗██╔════╝██╔══██╗██║╚══██╔══╝    ██╔══██╗██║██╔════╝██║ ██╔╝
██║     ██████╔╝█████╗  ██║  ██║██║   ██║       ██████╔╝██║███████╗█████╔╝ 
██║     ██╔══██╗██╔══╝  ██║  ██║██║   ██║       ██╔══██╗██║╚════██║██╔═██╗ 
╚██████╗██║  ██║███████╗██████╔╝██║   ██║       ██║  ██║██║███████║██║  ██╗
 ╚═════╝╚═╝  ╚═╝╚══════╝╚═════╝ ╚═╝   ╚═╝       ╚═╝  ╚═╝╚═╝╚══════╝╚═╝  ╚═╝
                                                                           
                                                                         


"))),
                 br(),
                 br()
                 ),
        tabPanel("Intro - Data Viz",
                 tableOutput("head_kable"),
                 plotOutput('loangrade'),
                 plotOutput('flag'),
                 plotOutput('person_age'),
                 plotOutput('person_home_ownership'),
                 plotOutput('person_income'),
                 plotOutput('loan_intent'),
                 plotOutput('loan_percent_income')
                 ),
        tabPanel("Classification Model",
                 plotOutput('loss_viz'),
                 hr(),
                 plotOutput('shap_viz'),
                 hr(),
                 fluidRow(
                   column(width = 6, 
                          plotOutput('cm_train')
                          ),
                   column(width = 6,
                          plotOutput('cm_test')
                          )
                 )
                 ),
        tabPanel("Thresholding",
                 sliderInput("decimal",
                             "Score Threshold:",
                             min = 6,
                             max = 100,
                             value = 0,
                             ticks = T),
                 tableOutput("values"),
                 hr(),
                 fluidRow(
                   column(width = 6, 
                          plotOutput('dynamic_roc')
                          ),
                   column(width = 6,
                         plotOutput('dynamic_metrics')
                          )
                   )
                 ),
        tabPanel("KPI: Lending Target"),         
        tabPanel(div(img(src = "binoculars.png")
                     )
                 )
        
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {

  library(hrbrthemes)
  library(rsample)
  library(xgboost)
  library(tidyverse)
  library(kableExtra)
  library(fastDummies)
  library(scales)
  library(rsample)
  library(ROSE)
  source('/Users/jd/Documents/CD3001B/creditRisk_creditCardFraud/xgb_functions.R')
  
  
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
  
df1 <- 
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
  
output$head_kable  <- function() {
  
  df1 %>%
    head %>%
    kable(.,
          format = "html",
          booktabs = TRUE) %>%
    kable_styling(font_size = 12,"striped") %>%
    scroll_box(width = "100%")
  }

  plist <- visualize_all_variables(df)

    
  output$person_age = renderPlot({
      plist$person_age
      })
  
  output$person_home_ownership = renderPlot({
    plist$person_home_ownership
  })
  
  output$person_income = renderPlot({
    plist$person_income
  })
  
  output$loan_intent = renderPlot({
    plist$loan_intent
  })
  
  
  output$loan_percent_income = renderPlot({
    plist$loan_percent_income
  })
  
  output$flag = renderPlot({
    plist$flag
  })
  
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
  
  
  xvars = names(train_data)[names(train_data) != 'flag']
  yvar = 'flag'
  
  train_xgb = makeXGBMatrix(train_data, xvars = xvars, yvar = yvar)
  test_xgb = makeXGBMatrix(test_data, xvars = xvars, yvar = yvar)

  finalXGB <-
    searchXGB(train_xgb,
              test_xgb)
  
  a <-  df1 %>%
    ggplot(aes(loan_amnt, fill = loan_grade, colour = loan_grade)) +
    geom_histogram(alpha = 0.5, position = "fill") +
    theme_ipsum() +
    theme(legend.position = 'bottom') +
    theme(
      text = element_text(colour="white"),
      axis.text.x = element_text(
        colour="white"),
      axis.text.y = element_text(
        colour="white"),
      legend.title=element_text(
        colour="white")
      
    )
  
  b <-
    finalXGB$finalXGB$evaluation_log %>% 
    as_tibble %>% 
    pivot_longer(cols = c('train_auc','test_auc'),
                 values_to = 'auc',
                 names_to = 'sample') %>% 
    ggplot(aes(iter,auc,col=sample,group=sample)) +
    geom_line(alpha=0.6) +
    scale_colour_manual(name='',values=c('lightblue',"blue")) +
    theme_ipsum() +
    theme(legend.position = 'bottom') +
    theme(
      text = element_text(colour="white"),
      axis.text.x = element_text(
        colour="white"),
      axis.text.y = element_text(
        colour="white"),
      legend.title=element_text(
        colour="white")
      
    )
  
  c <- make_shap_viz(shap_viz_name = '',
                     yvar = yvar,
                     finalXGB = finalXGB$finalXGB,
                     dataz = df)
  
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
      nrows = 100,
      obs_pred_df_raw = obs_pred_df_raw
    )
  
  roc_table_test <-
    make_roc_table(
      model = finalXGB$finalXGB,
      data = test_xgb,
      yflag = test_data$flag,
      nrows = 100,
      obs_pred_df_raw = obs_pred_df_raw
    )
    
    output$loangrade <- renderPlot({
      a
    })
    
    output$loss_viz <- renderPlot({
      b
    })
    
    output$shap_viz <- renderPlot({
      c
    })

    sliderValues <- reactive({
      
      roc_table_test[1:input$decimal,] 
      
    })
    
    output$values <- function()({
      sliderValues() %>% 
        tail %>% 
        kable(.,
              format = "html",
              booktabs = TRUE) %>%
        kable_styling(font_size = 12,"striped") %>%
        scroll_box(width = "100%")
    })    
  
    output$dynamic_roc <- renderPlot({
      
      plot_cm_slider(TP=sliderValues() %>% 
                       tail(1) %>% 
                       select(TP) %>% 
                       unlist,
                     FP=sliderValues() %>% 
                       tail(1) %>% 
                       select(FP) %>% 
                       unlist,
                     TN=sliderValues() %>% 
                       tail(1) %>% 
                       select(TN) %>% 
                       unlist,
                     FN=sliderValues() %>% 
                       tail(1) %>% 
                       select(FN) %>% 
                       unlist
      )
      
    })
    
    output$dynamic_metrics <- renderPlot({
      
      sliderValues() %>% 
        rename(recall=TPR_recall) %>% 
        drop_na() %>% 
        pivot_longer(names_to = 'metric',
                     values_to = 'value',
                     cols = c('lift', 'precision','recall')) %>% 
        ggplot(aes(x=threshold, y=value, group=metric, colour=metric)) +
        geom_line() +
        theme_ipsum() +
        xlim(c(0.05, .8)) +
        scale_colour_manual(values=c("#000066","#339999",'lightblue'))+
        theme(
          legend.position = 'bottom',
          text = element_text(colour="white",size=12),
          axis.text.x = element_text(
            colour="white"),
          axis.text.y = element_text(
            colour="white"),
          legend.title=element_text(
            colour="white"),
          strip.text = element_text(colour = 'white')
          
        ) + facet_wrap(~metric, nrow=3)
    
    })    
  
    output$cm_train <- renderPlot({
      cm_train
    })
    
    output$cm_test <- renderPlot({
      cm_test
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
