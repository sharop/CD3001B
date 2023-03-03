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
      theme = bslib::bs_theme(bootswatch = 'cosmo'),
      '',
      tabPanel(div(img(src = "binoculars.png")),
               fluidRow(column(
                 12,
                 align = "center",
                 tags$pre(
                   "


 ██████╗██████╗ ███████╗██████╗ ██╗████████╗    ██████╗ ██╗███████╗██╗  ██╗
██╔════╝██╔══██╗██╔════╝██╔══██╗██║╚══██╔══╝    ██╔══██╗██║██╔════╝██║ ██╔╝
██║     ██████╔╝█████╗  ██║  ██║██║   ██║       ██████╔╝██║███████╗█████╔╝
██║     ██╔══██╗██╔══╝  ██║  ██║██║   ██║       ██╔══██╗██║╚════██║██╔═██╗
╚██████╗██║  ██║███████╗██████╔╝██║   ██║       ██║  ██║██║███████║██║  ██╗
 ╚═════╝╚═╝  ╚═╝╚══════╝╚═════╝ ╚═╝   ╚═╝       ╚═╝  ╚═╝╚═╝╚══════╝╚═╝  ╚═╝



"
                 )
               )),
               br(),
               hr(),
               p(
                 "The definition of a KPI, or a key performance indicator, is a measurable value used to evaluate how successful a person or organization is at reaching a target."
               ),
               br(),
               p(
                 "KPIs should be aligned with the overall business strategy and outcomes, they should also be actionable, realistic and measurable."
               ),
               br(),
               p(
                 "This app explores lending KPIs (i.e. classification metrics and their impact on revenue) and how to adjust them through modelling users' propensity to default on their loans."
               ),
               br(),
               p(
                 "A credit-risk data set was used to this end. 20% of the sample was left for KPI adjustment." 
               ),
               br(),
               p(
                 "Out of the 80% used for modelling, 80% was set for training and 20% for testing the model.",
               ),
               br(),
               p(
                 "A random hyper-parameter search was used to optimise a decision tree ensemble fit using xgboost."
               ),
               br(),
               p(
                 "Variable contribution was visualised using Shapley additive values"
               )),
      tabPanel(
        "Intro - Data Viz",
        tableOutput("head_kable"),
        plotOutput('loangrade'),
        plotOutput('flag'),
        plotOutput('person_age'),
        plotOutput('person_home_ownership'),
        plotOutput('person_income'),
        plotOutput('loan_intent'),
        plotOutput('loan_percent_income')
      ),
      tabPanel(
        "Classification Model",
        plotOutput('loss_viz'),
        hr(),
        plotOutput('shap_viz'),
        hr(),
        fluidRow(column(width = 6,
                        plotOutput('cm_train')),
                 column(width = 6,
                        plotOutput('cm_test')))
      ),
      tabPanel(
        "Thresholding",
        sliderInput(
          "decimal",
          "Score Threshold:",
          min = 6,
          max = 100,
          value = 0,
          ticks = T
        ),
        tableOutput("values"),
        hr(),
        fluidRow(column(width = 6,
                        plotOutput('dynamic_roc')),
                 column(width = 6,
                        plotOutput('dynamic_metrics')))
      ),
      tabPanel(
        "KPIs: Lending Targets",
        h3("Portfolio Amount: $10,000,000"),
        br(),
        sliderInput(
          'prestamo',
          "KPI 1: Average Loan Amount ($):",
          min = 3000,
          max = 7000,
          value = 3000,
          ticks = T
        ),
        sliderInput(
          'duracion',
          "KPI 2: Average Duration (months):",
          min = 1,
          max = 12,
          value = 6,
          ticks = T
        ),
        sliderInput(
          'interes',
          "KPI 3: Average Monthly Interest Rate (%):",
          min = 0.01,
          max = 0.4,
          value = 0.1,
          ticks = T
        ),
        h3("Revenue (Amount Recovered (i.e. sum lent + interest) - Amount Not Recovered (i.e. bad debt):"),
        br(),
        textOutput('ganancia')
      ),
      tabPanel(
        div(img(src = "binoculars.png")
            )
        )
    )
  )

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
  source('credit_risk_functions.R')
  options(scipen=1000000000)
  
  df <-
    read_csv('credit_risk_dataset.csv') %>%
    rename_with(.,
                ~ tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%
    map_if(.p = is.character,
           .f = tolower) %>%
    as_tibble %>%
    filter(person_age < 99) %>%
    mutate(flag = case_match(cb_person_default_on_file,
                             "n" ~ 0,
                             "y" ~ 1)) %>%
    select(-cb_person_default_on_file,-loan_grade) %>%
    drop_na()
  
  apetite <- initial_split(df, prop = 0.80, strata = 'flag')
  
  df <- training(apetite)
  
  riskApetiteSample <- testing(apetite)
  
  df1 <-
    read_csv('credit_risk_dataset.csv') %>%
    rename_with(., ~ tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%
    map_if(.p = is.character,
           .f = tolower) %>%
    as_tibble %>%
    filter(person_age < 99) %>%
    mutate(flag = case_match(cb_person_default_on_file,
                             "n" ~ 0,
                             "y" ~ 1)) %>%
    select(-cb_person_default_on_file,-loan_grade) %>%
    drop_na()
  
  output$head_kable  <- function() {
    df1 %>%
      head %>%
      kable(.,
            format = "html",
            booktabs = TRUE) %>%
      kable_styling(font_size = 12, "striped") %>%
      scroll_box(width = "100%")
  }
  
  g_calculator <- function(cantidad_promedio,
                           duracion_promedio,
                           interes_promedio) {
    debt <-
      tibble(flag = riskApetiteSample$flag,
             prob = predict(finalXGB$finalXGB, riskApetiteXGB)) %>%
      group_by(prob) %>%
      summarise(
        score_defaulters = sum(flag),
        score_non_defaulters = length(flag) - sum(flag)
      ) %>%
      arrange(prob) %>%
      mutate(
        total_defaulters = cumsum(score_defaulters),
        total_non_defaulters = cumsum(score_non_defaulters)
      ) %>%
      group_by(prob, total_defaulters, total_non_defaulters) %>%
      summarise(total = sum(total_defaulters,
                            total_non_defaulters)) %>%
      ungroup() %>%
      filter(total <= 10000000 / cantidad_promedio) %>%
      tail(n = 1)
    
    return(paste(
      "$",
      format(
        (
          cantidad_promedio * duracion_promedio * interes_promedio * (10000000 / cantidad_promedio)
        ) - (debt$total_defaulters * cantidad_promedio) - 10000000
      ,
      big.mark = ","
    ))
    )
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
                         "loan_intent")
    ) %>%
    map_if(
      .p = function(x)
        ! identical(unique(x), c(1, 0)),
      .f = function(x)
        minmax(x)
    ) %>%
    as_tibble
  
  riskApetiteSample <-
    riskApetiteSample %>%
    dummy_cols(
      remove_selected_columns = T,
      remove_first_dummy = T,
      select_columns = c("person_home_ownership",
                         "loan_intent")
    ) %>%
    map_if(
      .p = function(x)
        ! identical(unique(x), c(1, 0)),
      .f = function(x)
        minmax(x)
    ) %>%
    as_tibble
  
  ### ROSE
  
  df_split <- initial_split(df, prop = 0.80)
  
  train_data <- training(df_split)
  test_data <- testing(df_split)
  
  train_data <-
    ROSE(flag ~ ., data = train_data, seed = 123)$data %>%
    as_tibble()
  
  test_data <-
    ROSE(flag ~ ., data = test_data, seed = 123)$data %>%
    as_tibble()
  
  riskApetiteSample <-
    ROSE(flag ~ ., data = riskApetiteSample, seed = 123)$data %>%
    as_tibble()
  
  
  xvars = names(train_data)[names(train_data) != 'flag']
  yvar = 'flag'
  
  train_xgb = makeXGBMatrix(train_data, xvars = xvars, yvar = yvar)
  test_xgb = makeXGBMatrix(test_data, xvars = xvars, yvar = yvar)
  
  
  riskApetiteXGB <-
    makeXGBMatrix(dataz = riskApetiteSample,
                  yvar = yvar,
                  xvars = xvars)
  
  finalXGB <-
    searchXGB(train_xgb,
              test_xgb)
  
  a <-
    df1 %>%
    ggplot(aes(loan_amnt, fill = loan_intent, colour = loan_intent)) +
    geom_histogram(alpha = 0.5, position = "fill") +
    theme_ipsum() +
    theme(legend.position = 'bottom') +
    theme(
      text = element_text(colour = "white"),
      axis.text.x = element_text(colour = "white"),
      axis.text.y = element_text(colour = "white"),
      legend.title = element_text(colour = "white")
      
    )
  
  b <-
    finalXGB$finalXGB$evaluation_log %>%
    as_tibble %>%
    pivot_longer(
      cols = c('train_auc', 'test_auc'),
      values_to = 'auc',
      names_to = 'sample'
    ) %>%
    ggplot(aes(iter, auc, col = sample, group = sample)) +
    geom_line(linewidth = 1.5) +
    scale_colour_manual(name = '', values = c('lightblue', "blue")) +
    theme_ipsum() +
    theme(legend.position = 'bottom') +
    theme(
      text = element_text(colour = "white"),
      axis.text.x = element_text(colour = "white"),
      axis.text.y = element_text(colour = "white"),
      legend.title = element_text(colour = "white")
      
    )
  
  c <-
    make_shap_viz(
      shap_viz_name = '',
      yvar = yvar,
      finalXGB = finalXGB$finalXGB,
      dataz = df
    )
  
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
    roc_table_test[1:input$decimal, ]
    
  })
  
  output$values <- function()
    ({
      sliderValues() %>%
        tail %>%
        kable(.,
              format = "html",
              booktabs = TRUE) %>%
        kable_styling(font_size = 12, "striped") %>%
        scroll_box(width = "100%")
      
    })
  
  # cantidad_promedio <- reactive({
  #   input$prestamo
  # })
  #
  # duracion_promedio <- reactive({
  #   input$duracion
  # })
  #
  # interes_promedio <- reactive({
  #   input$interes
  # })
  
  output$ganancia <- renderText({
    g_calculator(
      cantidad_promedio = input$prestamo,
      duracion_promedio = input$duracion,
      interes_promedio = input$interes
    )
    
  })
  
  output$dynamic_roc <- renderPlot({
    plot_cm_slider(
      TP = sliderValues() %>%
        tail(1) %>%
        select(TP) %>%
        unlist,
      FP = sliderValues() %>%
        tail(1) %>%
        select(FP) %>%
        unlist,
      TN = sliderValues() %>%
        tail(1) %>%
        select(TN) %>%
        unlist,
      FN = sliderValues() %>%
        tail(1) %>%
        select(FN) %>%
        unlist
    )
    
  })
  
  output$dynamic_metrics <- renderPlot({
    sliderValues() %>%
      rename(recall = TPR_recall) %>%
      drop_na() %>%
      pivot_longer(
        names_to = 'metric',
        values_to = 'value',
        cols = c('lift', 'precision', 'recall', "f1")
      ) %>%
      ggplot(aes(
        x = threshold,
        y = value,
        group = metric,
        colour = metric
      )) +
      geom_line(linewidth = 1.5) +
      theme_ipsum() +
      xlim(c(0.05, .8)) +
      scale_colour_manual(values = c("#000066", "#339999", 'lightblue', "aquamarine")) +
      theme(
        legend.position = 'bottom',
        text = element_text(colour = "white", size = 12),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        legend.title = element_text(colour = "white"),
        strip.text = element_text(colour = 'white')
        
      ) + facet_wrap(~ metric)
    
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
