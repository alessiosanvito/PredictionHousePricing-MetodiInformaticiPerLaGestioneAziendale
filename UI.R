library(shinyjs)
library(shinydashboard)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Presentazione dei dati"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Istogrammi", tabName = "istogrammi", icon = icon("chart-bar")),
      menuItem("Boxplot", tabName = "boxplot", icon = icon("box")),
      menuItem("Qqplot", tabName = "qqplot", icon = icon("chart-line")),
      menuItem("Correlazione tra le variabili", tabName = "correlazione",
               icon = icon("bezier-curve")),
      menuItem("Predizione", tabName = "pred", 
               icon = icon("file-invoice-dollar"))
    )
  ),
  dashboardBody( 
    tabItems(
      
      #Prima serie di grafici
      tabItem(tabName = "istogrammi",
              fluidRow(
                box(selectInput(inputId="hist_selected",
                            label = "Seleziona una variabile:",
                            choices = c("Nessuna selezione","price", "bedrooms", 
                                        "bathrooms", "sqft_living", "sqft_lot",
                                        "waterfront", "condition",
                                        "sqft_basement", "yr_built", 
                                        "yr_renovated"),
                            selected = "Nessuna selezione"), width = 10,
                box(plotOutput("istogramma"), width = 10)
              )
            )
          ),
      #Seconda serie di grafici
      tabItem(tabName = "boxplot",
              fluidRow(
                box(selectInput(inputId = "box_selected",
                                 label = "Seleziona una variabile:",
                                 choices = c("Nessuna selezione", "price", 
                                             "bedrooms", "bathrooms", 
                                             "sqft_living", "sqft_lot",
                                             "condition"),
                                 selected = "Nessuna selezione"), width = 10,
                box(plotOutput("box_plot"), width = 10)  
                )
              )
            ),
      #terza serie di grafici
      tabItem(tabName = "qqplot",
        fluidRow(
          box(selectInput(inputId="qq_selected",
                             label = "Seleziona una variabile:",
                             choices = c("Nessuna selezione", "price",
                                         "bedrooms", "bathrooms", 
                                         "sqft_living", "sqft_lot",
                                         "condition"),
                             selected = "Nessuna selezione"), width = 10,
                 box(plotOutput("qq_plot"), width = 10)
          )
        )
      ),
      #quarta serie di grafici
      tabItem(tabName = "correlazione",
              fluidRow(
                box(radioButtons(inputId = "corr_selected",
                                 label = "Seleziona le variabili:",
                                 choices = c("Nessuna selezione", "corr_mixed",
                                             "corr_superficie", "heatmap",
                                             "corr_citta", "corr_PCA"),
                                 selected = "Nessuna selezione"), width = 10,
                box(plotOutput("correlazioni"), width = 10)
                )
              )
      ),
      #quinta sezione
      tabItem(tabName = "pred",
              fluidRow(
                box(
                  numericInput(
                  'bedrooms',
                  'Numero di camere da letto',
                  value = 0,
                  min = 0,
                  max = 9
                ),
                numericInput(
                  'bathrooms',
                  'Numero di bagni',
                  value = 1, 
                  min = 0,
                  max = 5
                ),
                numericInput(
                  'sqft_living',
                  'Dimensione superficie abitabile',
                  value = 700,
                  min = 370,
                  max = 5960
                ),
                numericInput(
                  'sqft_lot',
                  'Dimensione superficie lotto',
                  value = 700, 
                  min = 638, 
                  max = 1074218
                ),
                numericInput(
                  'waterfront',
                  'Casa vicina (1) o lontana (0) dal mare',
                  value = 0,
                  min = 0, 
                  max = 1
                ),
                numericInput(
                  'condition',
                  'Condizione della casa',
                  value = 1,
                  min = 1,
                  max = 5
                ),
                numericInput(
                  'sqft_basement',
                  'Casa ha un seminterrato (1) oppure no (0)',
                  value = 0,
                  min = 0,
                  max= 1
                ),
                numericInput(
                  'yr_built',
                  'casa costruita dopo il 1971 (1) o prima (0)',
                  value = 0,
                  min = 0,
                  max = 1
                ),
                numericInput(
                  'yr_renovated',
                  'Casa ristrutturata (1) o non ristrutturata (0)',
                  value = 0,
                  min = 0,
                  max = 1
                ),
                numericInput(
                  'city',
                  'Citta in cui si trova la casa',
                  value = 1, 
                  min = 1,
                  max = 44
                ),
                numericInput(
                  'statezip',
                  'CAP della citta in cui si trova la casa',
                  value = 1, 
                  min = 1, 
                  max = 77
                )
                
              ),
              box(textOutput(outputId = "predict_price"), style = "font-size: 200%;")
              )
      )
    )
  )
)
  

