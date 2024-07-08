# Carregar bibliotecas necessárias
library(shiny)
library(dplyr)

# Interface do usuário
ui <- fluidPage(
  titlePanel("Previsões de Vendas Walmart"),
  
  tabsetPanel(
    tabPanel("Previsões",
             sidebarLayout(
               sidebarPanel(
                 selectInput("model", "Escolha o Modelo:",
                             choices = c("ARIMA", "ETS", "MLPE", "RandomForest", "VAR", "ARIMAX")),
                 selectInput("iteration", "Escolha a Iteração:",
                             choices = as.character(1:10))
               ),
               mainPanel(
                 tableOutput("results")
               )
             )
    ),
    tabPanel("Metricas",
             sidebarLayout(
               sidebarPanel(
                 selectInput("metrics_model", "Escolha o Modelo:",
                             choices = c("ARIMA", "ETS", "MLPE", "RandomForest", "VAR", "ARIMAX"))
               ),
               mainPanel(
                 tableOutput("metrics"),
                 uiOutput("metrics_image")
               )
             )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  # Variável reativa para armazenar a matriz de vendas previstas
  Matriz_Vendas_Previstas <- reactiveVal(NULL)
  
  get_model_file <- function(model) {
    switch(model,
           "ARIMA" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/ARIMAprevisoes_walmart.csv",
           "ETS" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/ETSprevisoes_walmart.csv",
           "MLPE" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/MLPEprevisoes_walmart.csv",
           "RandomForest" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/RandomForestprevisoes_walmart.csv",
           "VAR" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/VARprevisoes_walmart.csv",
           "ARIMAX" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/ARIMAXprevisoes_walmart.csv")
  }
  
  get_metrics_file <- function(model) {
    switch(model,
           "ARIMA" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/ARIMAmedian_metrics.csv",
           "ETS" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/ETSmedian_metrics.csv",
           "MLPE" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/MLPEmedian_metrics.csv",
           "RandomForest" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/RandomForestmedian_metrics.csv",
           "VAR" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/var_updated.csv",
           "ARIMAX" = "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Metricas/arimax_updated.csv")
  }
  
  output$results <- renderTable({
    model_file <- get_model_file(input$model)
    
    if (!file.exists(model_file)) {
      return(data.frame(Mensagem = "Arquivo não encontrado."))
    }
    
    data <- read.csv(model_file)
    iteration_data <- data %>% filter(Iteracao == as.numeric(input$iteration))
    
    if (nrow(iteration_data) == 0) {
      return(data.frame(Mensagem = "Nenhum dado encontrado para a iteração selecionada."))
    }
    
    iteration_matrix <- as.matrix(iteration_data[, c("Semana1", "Semana2", "Semana3", "Semana4")])
    rownames(iteration_matrix) <- iteration_data$Departamento
    
    # Converter os valores da matriz para inteiros
    iteration_matrix <- apply(iteration_matrix, 2, as.integer)
    
    # Atualizar a variável reativa e imprimir na consola
    Matriz_Vendas_Previstas(iteration_matrix)
    print(Matriz_Vendas_Previstas())
    
    return(iteration_matrix)
  })
  
  output$metrics <- renderTable({
    metrics_file <- get_metrics_file(input$metrics_model)
    
    if (!file.exists(metrics_file)) {
      return(data.frame(Mensagem = "Arquivo não encontrado."))
    }
    
    metrics_data <- read.csv(metrics_file, row.names = 1)
    
    return(metrics_data)
  })
  
  # Adicione qualquer outro output que você queira renderizar aqui.
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)

