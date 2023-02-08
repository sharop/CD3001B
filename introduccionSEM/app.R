library(shiny)
library(shinythemes)
library(thematic)
library(bslib)
# Define UI ----


thematic_shiny(font = "auto")

ui <-
  fluidPage(
    navbarPage(
      theme = bslib::bs_theme(bootswatch = 'slate'),
      '',
      tabPanel(div(img(src = "regression_icon.png")),
               fluidRow(column(
                 12,
                 align = "center",
                 verbatimTextOutput("show_string")
               )),
               br(),
               fluidRow(column(
                 12,
                 align = "center",
                 img(src = "caricatura_correlacion.png")
               ))
      ),
      tabPanel(
        "Introducción",
        strong(h1("Chat-GPT Say Whaaaat?")),
        br(),
        code(
          "INPUT: Escribe 20 bulletpoints introductorios a modelos de ecuaciones estructurales "
        ),
        br(),
        br(),
        p(
          "Modelos de ecuaciones estructurales son un tipo de modelos multivariantes que representan relaciones causales entre variables."
        ),
        br(),
        p(
          "Son ampliamente utilizados en investigación social, psicología, economía y ciencias de la salud."
        ),
        br(),
        p(
          "Los modelos de ecuaciones estructurales pueden ser utilizados para predecir la influencia de una variable sobre otras."
        ),
        br(),
        p(
          "Estos modelos se basan en la teoría causal y en la identificación de relaciones directas e indirectas entre variables."
        ),
        br(),
        p(
          "Las ecuaciones estructurales pueden incluir variables latentes, que son variables no observables pero que influyen en las variables observadas."
        ),
        br(),
        p(
          "Los modelos de ecuaciones estructurales se pueden utilizar para predecir la influencia de los cambios en una variable sobre otras variables."
        ),
        br(),
        p(
          "Los modelos de ecuaciones estructurales pueden ser utilizados para diseñar políticas y programas en una variedad de contextos."
        ),
        br(),
        p(
          "Los modelos de ecuaciones estructurales se pueden utilizar para controlar los efectos de variables confundentes."
        ),
        br(),
        p(
          "Los modelos de ecuaciones estructurales pueden ser aplicados a diferentes tipos de datos, como los datos de encuestas y registros administrativos."
        ),
        br(),
        p(
          "Son útiles para comprender las relaciones complejas entre las variables en un sistema."
        ),
        br(),
        p(
          "Los modelos de ecuaciones estructurales pueden ser utilizados para identificar puntos críticos y oportunidades de intervención en un sistema."
        ),
        br(),
        p(
          "Se pueden utilizar para comparar diferentes escenarios y evaluar su impacto sobre las variables clave."
        ),
        br(),
        p(
          "Son útiles para identificar las interacciones entre las variables y su efecto combinado sobre otras variables."
        ),
        br()
        
      ),
      tabPanel(
        "Causalidad",
        h2("Qué es una relación causal?"),
        fluidRow(column(width = 12,
                        br(),
                        tags$div(
                          tags$ul(
                            p(
                              "Ruiz, MA; Pardo, A; San Martín, M. Modelos de Ecuaciones Estructurales. Papeles del Psicólogo, 2010. Vol. 31(1), pp. 34-45:"
                            ),
                            br(),
                            tags$li(
                              "La existencia de una relación causal entre las variables debe venir sustentada por la articulación teórica del modelo y no por su estimación con datos de tipo transversal. Para demostrar científicamente la existencia de una relación causal deberemos recurrir al diseño de un experimento controlado con asignación aleatoria de los sujetos a las condiciones del estudio."
                            ),
                            br(),
                            p(
                              "Fox, J. Linear Structural-Equation Models. https://socialsciences.mcmaster.ca/jfox/Courses/R/IQSBarcelona/SEMs-chapter.pdf, 1984."
                            ),
                            br(),
                            tags$li(
                              "Structural Equation Models in no way avoid the pitfalls of drawing causal inferences from observational data. The term 'causal model', then, at once promises too much and is nonspecific."
                            ),
                            br(),
                            tags$li(
                              "Otra aproximación a modelos de causalidad basado en redes Bayesianas: https://github.com/quantumblacklabs/causalnex"
                            ),
                            br(),
                            hr()
                          )
                        ))),
        h2("Modelo de Síndrome Metabólico"),
        fluidRow(column(width = 8,
                        img(src = 'causalidad_1.png'))),
        br(),
        tags$div(tags$ul(
          tags$li(
            "Ortiz, M; Fernández-Pera. Modelo de Ecuaciones Estructurales: Una guía para ciencias médicas y ciencias de la salud. Terapia Psicológica. Versión Online. ISSN 0718-4808"
          ),
          br(),
          tags$li(
            "Síndrome Metabólico es un factor latente (representado por círculos), que no es medido directamente sino más bien evaluado indirectamente usando seis indicadores (perímetro de cintura, triglicéridos, colesterol HDL, glucosa, presión sistólica y presión diastólica). El Síndrome Metabólico es predicho por otro factor latente de Estrés Psicológico, el cual es medido indirectamente por dos indicadores que sí son medidos directamente (estrés agudo y estrés crónico). El sexo (variable observada) predice simultáneamente al Síndrome Metabólico y al Estrés Psicológico. La nomenclatura de SEM indica que los factores latentes sean identificados con una mayúscula inicial y las variables observadas o indicadores con minúsculas."
          ),
          br()
        )),
        br(),
        hr()
      ),
      tabPanel(
        "Componentes",
        strong(h1("Tipos de variables")),
        br(),
        p(
          "Ruiz, MA; Pardo, A; San Martín, M. Modelos de Ecuaciones Estructurales. Papeles del Psicólogo, 2010. Vol. 31(1), pp. 34-45:"
        ),
        tags$div(
          tags$ul(
            tags$li(
              "Variable observada o indicador. Variables que se mide a los sujetos. Por ejemplo, las preguntas de un cuestionario."
            ),
            br(),
            tags$li(
              "Variable latente. Característica que se desearía medir pero que no se puede observar y que está libre de error de medición. Por ejemplo, una dimensión de un cuestionario o un factor en un análisis factorial exploratorio."
            ),
            br(),
            tags$li(
              "Variable error. Representa tanto los errores asociados a la medición de una variable como el conjunto de variables que no han sido contempladas en el modelo y que pueden afectar a la medición de una variable observada. Se considera que son variables de tipo latente por no ser observables directamente. El error asociado a la variable dependiente representa el error de predicción."
            ),
            br(),
            tags$li(
              "Variable de agrupación. Variable categóricas que representa la pertenencia a las distintas subpoblaciones que se desea comparar. Cada código representa una subpoblación."
            ),
            br(),
            tags$li(
              "Variable exógena. Variable que afecta a otra variable y que no recibe efecto de ninguna variable. Las variables independientes de un modelo de regresión son exógenas."
            ),
            br(),
            tags$li(
              "Variable endógena. Variable que recibe efecto de otra variable. La variable dependiente de un modelo de regresión es endógena. Toda variable endógena debe ir acompañada de un error."
            )
          )
        ),
        br(),
        hr(),
        strong(h1("Tipos de error")),
        br(),
        tags$div(tags$ul(
          tags$li("Error de medición"),
          br(),
          tags$li("Error de predicción")
        )),
        br(),
        hr(),
        strong(h1("Convenciones de Representación")),
        tags$div(
          tags$ul(
            br(),
            tags$li(
              "Las variables observables se representan encerradas en rectángulos."
            ),
            br(),
            tags$li(
              "Las variables no observables (latentes) se representan encerradas en óvalos o círculos."
            ),
            br(),
            tags$li(
              "Los errores (sean de medición o de predicción) se representan sin rectángulos ni círculos (aunque algunos programas las dibujan como variables latentes)."
            ),
            br(),
            tags$li(
              "Las relaciones bidireccionales (correlaciones y covarianzas) se representan como vectores curvos con una flecha en cada extremo."
            ),
            br(),
            tags$li(
              "Cualquier efecto estructural se representa como una flecha recta, cuyo origen es la variable predictora y cuyo final, donde se encuentra la punta de la flecha, es la variable dependiente."
            ),
            br(),
            tags$li(
              "Los parámetros del modelo se representan sobre la flecha correspondiente."
            ),
            br(),
            tags$li(
              "Cualquier variable que reciba efecto de otras variables del modelo deberá incluir también un término error."
            ),
            br(),
            tags$li(
              "Aunque no es necesario que el usuario lo especifique, los programas suelen incluir, junto a cada variable, su varianza y, si se trata de una variable dependiente, su correspondiente proporción de varianza explicada."
            )
          )
        ),
        hr(),
        strong(h1("Noción de Ajuste")),
        tags$div(
          tags$ul(
            br(),
            tags$li(
              "En regresión lineal, cuando hablamos de las estimaciones de los parámetros, escogemos aquellas estimaciones que mejor ajustan el modelo a los datos, en el sentido de que minimizan los errores de predicción cometidos con el modelo para el conjunto de sujetos de la muestra (en el método de mínimos cuadrados)."
            ),
            br(),
            tags$li(
              "Por el contrario, en los modelos de ecuaciones estructurales, lo que se pretende ajustar son las covarianzas entre las variables, en vez de buscar el ajuste a los datos."
            ),
            br(),
            tags$li(
              "En lugar de minimizar la diferencia entre los valores pronosticados y los observados a nivel individual, se minimiza la diferencia entre las covarianzas observadas en la muestra y las covarianzas pronosticadas por el modelo estructural."
            ),
            br(),
            tags$li(
              "Este es el motivo por el que a estos modelos también se les llama de estructura de covarianza (covariance structure models; Long, 1983)."
            )
          )
        )
        
      ),
      tabPanel(
        "Relaciones",
        h2("Covariación vs Causalidad"),
        fluidRow(column(width = 8,
                        br(),
                        tags$div(
                          tags$ul(
                            p(
                              "Ruiz, MA; Pardo, A; San Martín, M. Modelos de Ecuaciones Estructurales. Papeles del Psicólogo, 2010. Vol. 31(1), pp. 34-45:"
                            ),
                            br(),
                            tags$li(
                              "Decimos que dos fenómenos covarían, o que están correlacionados, cuando al observar una mayor cantidad de uno de los fenómenos también se observa una mayor cantidad del otro (o menor si la relación es negativa)."
                            ),
                            br(),
                            tags$li(
                              "El cambio de perspectiva desde la covariación observada a la causalidad atribuida a dos variables lo lleva a cabo el investigador, que es quien hipotetiza la causalidad. Saris y Stronkhorst (1984)."
                            ),
                            hr()
                          )
                        )),
                 column(
                   width = 4,
                   img(src = 'covariacion_1.png'),
                   img(src = 'causalidad_2.png')
                 )),
        h2("Relación Espuria"),
        fluidRow(column(width = 8,
                        br(),
                        tags$div(
                          tags$ul(
                            br(),
                            tags$li(
                              "En una relación causal básica o una relación de covariación hay involucradas dos variables. En una relación espuria la relación comprende al menos tres variables."
                            ),
                            br(),
                            tags$li(
                              "Esta es la razón por la cual la covariación entre dos variables puede ser muy elevada y, sin embargo, ser nula su relación causal."
                            ),
                            br(),
                            tags$li(
                              "Un ejemplo típico de relación espuria es la que se da entre estatura e inteligencia en preescolares. Si medimos ambas variables en niños de preescolar es muy posible que encontremos una alta relación entre ellas; sin embargo, a nadie se le ocurre pensar que la estatura causa la inteligencia. Existe una tercera variable, el desarrollo del niño (la edad), que es causa de ambas variables y que hace que se observe esa relación."
                            )
                          )
                        )),
                 column(width = 4,
                        img(src = 'rel_espuria.png'))),
        hr(),
        h2('Relación Causal Directa e Indirecta'),
        fluidRow(column(width = 8,
                        br(),
                        tags$div(
                          tags$ul(
                            br(),
                            tags$li(
                              "Una relación causal indirecta implica la presencia de tres variables."
                            ),
                            br(),
                            tags$li(
                              "Existe una relación indirecta entre dos variables cuando una tercera variable modula o mediatiza el efecto entre ambas."
                            ),
                            br(),
                            tags$li(
                              "Consideremos la relación entre la aptitud, el rendimiento y la motivación. Podemos pensar en el nivel de motivación como una variable que modula la relación entre la aptitud y el rendimiento."
                            ),
                            br(),
                            tags$li(
                              "El modelo de la figura propone que existe un efecto directo de la aptitud sobre la motivación y de la motivación sobre el rendimiento. Además, existe un efecto indirecto entre la aptitud y el rendimiento. El efecto indirecto de la variable aptitud sobre el rendimiento puede ser potenciado (o atenuado) por la variable moduladora motivación."
                            )
                          )
                        )),
                 column(
                   width = 4,
                   img(src = 'rel_indirecta1.png'),
                   img(src = 'rel_indirecta2.png')
                 )),
        hr(),
        h2('Relación Causal Recíproca'),
        fluidRow(column(width = 8,
                        br(),
                        tags$div(
                          tags$ul(
                            br(),
                            tags$li(
                              "La relación causal entre dos variables puede ser recíproca o unidireccional."
                            ),
                            br(),
                            tags$li(
                              "Cuando la relación es recíproca (bidireccional) la variable causa es a su vez efecto de la otra."
                            ),
                            br(),
                            tags$li(
                              "Una relación recíproca es en definitiva un bucle de retroalimentación entre dos variables. La relación causal recíproca puede ser directa o indirecta, implicando a otras variables antes de cerrase el bucle."
                            ),
                            br(),
                            tags$li(
                              "La relación entre la Ansiedad y el Rendimiento puede representarse como un bucle recíproco: cuanto mayor es la ansiedad, peor es el rendimiento; y cuanto peor es el rendimiento, mayor es la ansiedad."
                            )
                          )
                        )),
                 column(width = 4,
                        img(src = 'rel_reciproca1.png'))),
        hr(),
        h2('Efectos Totales'),
        fluidRow(column(width = 8,
                        br(),
                        tags$div(
                          tags$ul(
                            br(),
                            tags$li(
                              "Existe un último tipo de efecto (o relación): los efectos no analizados."
                            ),
                            br(),
                            tags$li(
                              "A la suma de los efectos espurios más los efectos no analizados se les denomina efectos no causales."
                            ),
                            br(),
                            tags$li(
                              "Una vez que el modelo está definido, los efectos espurios aparecen cuando las variables endógenas están correlacionadas más allá de los efectos estimados (apareciendo covarianzas entre los errores de predicción)"
                            ),
                            br(),
                            tags$li(
                              "Los efectos no analizados aparecen cuando las variables observables están correlacionadas más allá de lo que el modelo predice (apareciendo covarianzas entre los errores de medición)."
                            )
                          )
                        )),
                 column(width = 4,
                        img(src = 'rel_reciproca1.png')))
      ),
      tabPanel(
        title = "Expresiones",
        fluidRow(
          
          withMathJax(),
          helpText('El ajuste de un modelo se puede expresar en una hipótesis fundamental, que propone que, si el modelo es correcto y conociéramos los parámetros del modelo estructural, la matriz de covarianzas poblacional podría ser reproducida exactamente a partir de la combinación de los parámetros del modelo:
           $$\\sum\\ = \\sum \\theta\\ $$ donde Σ es la matriz de varianzas-covarianzas poblacional entre las variables observables, θ es un vector que contiene los parámetros del modelo y Σ(θ) es la matriz de varianzas-covarianzas derivada como una función de los parámetros contenidos en el vector θ.')
                 ),
        br(),
        hr(),
        fluidRow(
          column(
            width = 8,
            "Modelo Estructural de Regresión Lineal"),
          column(
            width = 4,
            img(src = 'sem_de_regresion.png'))
          ),
          br(),
        hr(),
          fluidRow(
            column(
              width = 8,
              "Ecuación de regresión lineal expresada en y = bx + c γ es el coeficiente de regresión y ε la variable que representa el término error, que se asume que es independiente de x y cuyo valor esperado es cero."),
            column(
              width = 4,
              img(src = 'ecuacion_regresion.png'))
            ),
        br(),
        hr(),
            fluidRow(
              column(
                width = 8,
                "Matriz de varianzas-covarianzas de x y y"),
              column(
                width = 4,
                img(src = 'matriz_var_covar.png'))
            ),
        br(),
        hr(),
        fluidRow(
          column(
            width = 8,
            "Matriz Σ expresada en función de la ecuación de regresión lineal. La varianza de la variable dependiente es función del parámetro γ al cuadrado y de la varianza de los errores"),
          column(
            width = 4,
            img(src = 'ecuacion_lineal_de_varianza_y.png'))
        ),
        br(),
        hr(),
        fluidRow(
          column(
            width = 8,
            "La covarianza de x y y es función del parámetro γ y de la varianza de x"),
          column(
            width = 4,
            img(src = 'covarianza_x_y.png'))
        ),
        br(),
        hr(),
        fluidRow(
          column(
            width = 8,
            "Entonces, la matriz de varianzas- covarianzas poblacional se puede escribir:"),
          column(
            width = 4,
            img(src = 'matriz_var_covar2.png'))
        ),
        br(),
        hr(),
        fluidRow(
          column(
            width = 8,
            "Podemos sustituir ahora en la ecuación inicial y volver a expresar la hipótesis básica"),
          column(
            width = 4,
            img(src = 'hipotesis_basica2.png'))
        ),
        br(),
        hr()
        ),
      tabPanel(
        "Pasos",
        p(
          "Ruiz, MA; Pardo, A; San Martín, M. Modelos de Ecuaciones Estructurales. Papeles del Psicólogo, 2010. Vol. 31(1), pp. 34-45:"
        ),
        strong(h1("Formulación del Modelo")),
        br(),
        tags$div(
          tags$ul(
            tags$li(
              "La estimación de un modelo comienza con la formulación de la teoría que lo sustenta."
            ),
            br(),
            tags$li(
              "El modelo teórico debe especificar las relaciones que se espera encontrar entre las variables (correlaciones, efectos directos, efectos indirectos, bucles)."
            ),
            br(),
            tags$li(
              "Si una variable no es directamente observable, deben mencionarse los indicadores que permiten medirla."
            ),
            br(),
            tags$li(
              "Lo normal es formular el modelo en formato gráfico; a partir de ahí es fácil identificar las ecuaciones y los parámetros."
            ),
            br(),
            tags$li(
              "Puede suceder que el modelo no esté completamente identificado, lo que querrá decir que se está intentando estimar más parámetros que el número de piezas de información contenidas en la matriz de varianzas-covarianzas."
            ),
            br(),
            tags$li(
              "Variable endógena. Variable que recibe efecto de otra variable. La variable dependiente de un modelo de regresión es endógena. Toda variable endógena debe ir acompañada de un error."
            )
          )
        ),
        strong(h1("Estimación de los Parámetros")),
        br(),
        tags$div(
          tags$ul(
            tags$li(
              "Si las estimaciones obtenidas no reproducen correctamente los datos observados, habrá que rechazar el modelo y con ello la teoría que lo soportaba."
            ),
            br(),
            tags$li("Valoración técnica de los valores estimados para los parámetros."),
            br(),
            tags$li("La magnitud de los parámetros debe ser la adecuada."),
            br(),
            tags$li("La magnitud de los parámtros debe ser la adecuada."),
            br(),
            tags$li("Los efectos deben ser significativamente distintos de cero"),
            br(),
            tags$li(
              "No deben obtenerse estimaciones impropias (como varianzas negativa)."
            ),
            br(),
            tags$li(
              "Puede ocurrir que alguna de las estimaciones tenga un valor próximo a cero; cuando ocurre esto es recomendable simplificar el modelo eliminando el correspondiente efecto."
            ),
            br(),
            tags$li("Por último, el modelo debe interpretarse en todas sus partes.")
            
          )
        ),
        strong(h1("Ajuste")),
        br(),
        tags$div(tags$ul(
          tags$li("El objetivo de la estimación es obtener los valores de los parámetros que permiten mantener la igualdad con los datos muestrales y los poblacionales."),
          br(),
          tags$li("Las estimaciones se realizan intentando maximizar el ajuste del modelo"),
          br(),
          tags$li("Se utiliza alguna medida que resuma la magnitud de las diferencias entre las varianzas y covarianzas observadas y las reproducidas, y se intenta minimizar dichas diferencias.")
        ))
      ),
      tabPanel(
        title = "Bondad de Ajuste",
        fluidRow(
          column(
            width = 12,
            tags$div(tags$ul(
              tags$li("Estadísticos de ajuste absoluto (valoran los residuos)"),
              br(),
              tags$li("Estadísticos de ajuste relativo (comparan el ajuste respecto a otro modelo de peor ajuste)"),
              br(),
              tags$li("Estadísticos de de ajuste parsimonioso (valoran el ajuste respecto al número de parámetros utilizado).")
            )))
        ),
        br(),
        hr(),
        fluidRow(
          column(
            width = 12 ,
            img(src = 'tabla_de_criterios_de_ajuste.png'))
          ),
      br(),
      hr(),
      fluidRow(
        column(
          width = 8,
          'El estadístico chi-cuadrado es conceptualmente el más atractivo; permite contrastar la hipótesis nula de que todos los errores del modelo son nulos, por lo que interesa mantener dicha hipótesis con la muestra utilizada. Sin embargo, es muy sensible al tamaño muestral: con muestras grandes (mayores de 100 ó 200 casos) es relativamente fácil rechazar la hipótesis nula cuando el modelo de hecho consigue un buen ajuste. Por este motivo, además de valorar su significación estadística, suele compararse con sus grados de libertad. Siempre se informa de este estadístico.'
        )
      )
    ),
    tabPanel(
      title = "Ejemplo",
    ),
      tabPanel(div(img(src = "graph.png")))
      
    )
  )

# Define server logic ----
server <- function(input, output, session) {
  library(shiny)
  
  
  output$ex1 <- renderUI({
    withMathJax(helpText('Dynamic output 1:  $$\\alpha^2$$'))
  })
  
  output$show_string <- renderText({
  
    sem_oh_yeah <- 
      c("", "  ____   U _____ u  __  __         U  ___ u  _   _        __   __U _____ u    _       _   _    _    ", 
        " / __\"| u\\| ___\"|/U|' \\/ '|u        \\/\"_ \\/ |'| |'|       \\ \\ / /\\| ___\"|/U  /\"\\  u  |'| |'| U|\"|u  ", 
        "<\\___ \\/  |  _|\"  \\| |\\/| |/        | | | |/| |_| |\\       \\ V /  |  _|\"   \\/ _ \\/  /| |_| |\\\\| |/  ", 
        " u___) |  | |___   | |  | |     .-,_| |_| |U|  _  |u      U_|\"|_u | |___   / ___ \\  U|  _  |u |_|   ", 
        " |____/>> |_____|  |_|  |_|      \\_)-\\___/  |_| |_|         |_|   |_____| /_/   \\_\\  |_| |_|  (_)   ", 
        "  )(  (__)<<   >> <<,-,,-.            \\\\    //   \\\\     .-,//|(_  <<   >>  \\\\    >>  //   \\\\  |||_  ", 
        " (__)    (__) (__) (./  \\.)          (__)  (_\") (\"_)     \\_) (__)(__) (__)(__)  (__)(_\") (\"_)(__)_) "
      )
    
    paste0(sem_oh_yeah, collapse = "\n")
  
  })
  
  #
  # library(tidyverse)
  # library(lubridate)
  # library(hrbrthemes)
  # library(viridis)
  # library(kableExtra)
  # library(rsample)
  # library(xgboost)
  # library(corrplot)
  # library(yaml)
  # library(data.table)
  # library(SHAPforxgboost)
  # library(streamgraph)
  # library(RColorBrewer)
  # library(ggcorrplot)
  # library(gganimate)
  # library(wesanderson)
  #
  # source('code/xgb_functions.R')
  # source('code/viz.R')
  #
  # jd_colours_blues <- c("#F7FBFF","#DEEBF7" ,"#C6DBEF" ,"#9ECAE1", "#6BAED6" ,"#4292C6", "#2171B5", "#08519C", "#08306B")
  # jd_colours_many <- c("#2171B5", "#08519C",'#FFC300', '#D70040', '#D2042D', '#DC143C', '#D3D3D3','#7F7F7F', '#FFDB58')
  #
  # raw_data <- read_csv('data/mexico_market_data_recategorised.csv')
  # indices_file = 'data/recat_all_indices_mexico.csv'
  # loss_file = 'data/recat_loss_table_mexico.csv'
  #
  # indices = read_csv(indices_file) %>%
  #   select(-`...1`) %>%
  #   mutate(date = as_date(date),
  #          category = recode(category,
  #                            Total='ICCMX',
  #                            apoyo_y_seguridad = 'support and security',
  #                            estabilidad_laboral = 'work stability',
  #                            factores_externos = 'external factors',
  #                            gastos = 'expenses',
  #                            grandes_gastos = 'large expenses',
  #                            inversion_y_ahorro = 'investment and savings',
  #                            ocio = 'leisure'
  #          )
  #   )
  #
  #
  # modelling_data <-
  #   indices %>%
  #   pivot_wider(id_cols = date,
  #               names_from = c(category,
  #                              sub_category),
  #               names_sep = '_',
  #               values_from = index)
  #
  # names(modelling_data) <- str_remove(str_replace_all(replacement = "_",
  #                                                     string = tolower(names(modelling_data)),
  #                                                     pattern = " "
  # ),
  # '_na'
  # )
  #
  # corr_table <-
  #   indices %>%
  #   pivot_wider(id_cols = date,
  #               names_from = c(category, sub_category),
  #               names_glue = "{category}_{sub_category}",
  #               values_from = index) %>%
  #   rename_with(.,
  #               .fn = ~ tolower(str_remove(string = .,
  #                                          pattern = '_NA')),
  #               .cols = everything()) %>%
  #   select(-date) %>%
  #   cor(., method = 'p')
  #
  # xvars =  c("support_and_security" ,
  #            "work_stability",
  #            "external_factors",
  #            "expenses",
  #            "large_expenses",
  #            "investment_and_savings",
  #            "leisure")
  #
  # yvar = 'iccmx'
  #
  # df_date <- modelling_data %>% select(date, xvars, yvar)
  #
  # df <- df_date %>% select(-date)
  #
  # dsplit <- initial_split(df_date, prop = 0.85)
  #
  # train_data_date <- training(dsplit)
  #
  # train_data <- train_data_date %>% select(-date)
  #
  # test_data_date <- testing(dsplit)
  #
  # test_data <- test_data_date %>% select(-date)
  #
  # XGB_All <- xgb.DMatrix(as.matrix(df[xvars]),label=df[[yvar]])
  #
  # XGB_Train <- xgb.DMatrix(as.matrix(train_data[xvars]), label=train_data[[yvar]])
  #
  # XGB_Test <- xgb.DMatrix(as.matrix(test_data[xvars]), label=test_data[[yvar]])
  #
  # XGB <- list()
  #
  # XGB <- searchXGB(XGB_Train,
  #                  XGB_Test,
  #                  XGB_All)
  #
  # output$cover <- renderImage({
  #
  #   outfile <- tempfile(pattern="outfile", fileext='.png')
  #
  #   anim <-
  #     indices %>%
  #     filter(category != 'ICCMX',
  #            category != 'support and security',
  #            is.na(sub_category)) %>%
  #     ggplot(., aes(x=date, y=index, colour=factor(category), alpha=0.8)) +
  #     geom_line() +
  #     theme_ipsum() +
  #     scale_colour_manual(values = c(wes_palettes$Darjeeling1,wes_palettes$Darjeeling1[1])) +
  #     theme(
  #       legend.position="none",
  #       panel.spacing = unit(0.1, "lines"),
  #       strip.text.x = element_blank(),
  #       plot.title = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.text.x = element_blank()
  #     ) +
  #     ylab('') +
  #     xlab('') +
  #     #      facet_wrap(~category) +
  #     transition_reveal(date)
  #
  #   anim_save("outfile.png", animate(anim, height = 529, width = 856, units = "px", bg = 'transparent'))
  #
  #   list(src = "outfile.png",
  #        contentType = 'image/png',
  #        width = 856,
  #        height = 529,
  #        alt = "This is alternate text"
  #   )}, deleteFile = TRUE)
  #
  # output$iccmx <- renderPlot({
  #   indices %>%
  #     filter(category == 'ICCMX') %>%
  #     select(date, index) %>%
  #     arrange(date) %>%
  #     ggplot(aes(x=date, y=index)) +
  #     geom_line(colour=wes_palettes$Darjeeling1[2]) +
  #     stat_smooth(method=loess,
  #                 level=0.99,
  #                 fullrange=T,
  #                 alpha=0.8,
  #                 se=F,
  #                 span=0.5,
  #                 linetype=9,
  #                 colour=wes_palettes$Darjeeling1[3]) +
  #     theme_ipsum() +
  #     theme(
  #       text = element_text(colour="lightgray"),
  #       legend.position="none",
  #       panel.spacing = unit(0.1, "lines"),
  #       strip.text.x = element_text(
  #         colour="lightgray"),
  #       plot.title = element_text(
  #         colour="lightgray"),
  #       axis.text.y = element_text(
  #         colour="lightgray"),
  #       axis.text.x = element_text(
  #         colour="lightgray")
  #     ) +
  #     ylab('index') +
  #     xlab('date')})
  #
  # output$dimensions <- renderPlot({
  #
  #   indices %>%
  #     filter(category != 'ICCMX',
  #            is.na(sub_category)) %>%
  #     ggplot(., aes(x=date, y=index,colour=category,alpha=0.8)) +
  #     geom_line() +
  #     stat_smooth(method=loess,
  #                 level=0.99,
  #                 fullrange=T,
  #                 alpha=0.8,
  #                 se=F,
  #                 span=0.5,
  #                 linetype=9,
  #                 colour=sample(jd_colours_blues[6]))+
  #     scale_colour_manual(values = c(wes_palettes$Darjeeling1,wes_palettes$Darjeeling1[1:2])) +
  #     theme_ipsum() +
  #     theme(
  #       text = element_text(colour="lightgray"),
  #       legend.position="none",
  #       panel.spacing = unit(0.1, "lines"),
  #       strip.text.x = element_text(colour="lightgray"),
  #       plot.title = element_text(colour="lightgray"),
  #       axis.text.y = element_text(colour="lightgray"),
  #       axis.text.x = element_text(colour="lightgray")
  #     ) +
  #     ylab('dimension') +
  #     facet_wrap(~category)
  #
  # })
  #
  #
  # output$correlations <-renderPlot({
  #   make_corrplot(indices)
  # }, height = 800, width = 800 )
  #
  #
  # output$shapviz <- renderPlot({
  #
  #   make_shap_viz(shap_viz_name = '',
  #                 yvar = 'iccmx',
  #                 finalXGB = XGB$finalXGB,
  #                 dataz = modelling_data[c(xvars,yvar)]
  #   )
  #
  # })
  #
  # output$tseries_shap <- renderPlot({
  #
  #   make_tseries_shap(
  #
  #     XGB,
  #     df,
  #     df_date,
  #     jd_colours_many,
  #     yvar='iccmx')
  
  #})
  
  
  
}
# Run the app ----
shinyApp(ui = ui, server = server)
