# Carregar bibliotecas necessárias
library(forecast)
library(rminer)
library(randomForest)

# Ler os dados do arquivo CSV
data <- read.csv("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/projeto/walmart.csv")

# Converter a coluna Date para o formato Date
data$Date <- as.Date(data$Date)

# Inicializar uma lista para armazenar as previsões por departamento
departamentos <- colnames(data)[4:7]
num_departamentos <- length(departamentos)
num_semanas <- 4  # Número de semanas para prever
num_iteracoes <- 10

# Data frame para armazenar todas as previsões com ID de iteração
previsoes_df <- data.frame()

# Fase 1: Implementação inicial com 10 iterações
for (i in 1:num_departamentos) {
  dep <- departamentos[i]
  cat("Fase 1: Previsão para o departamento:", dep, "\n")
  
  # Selecionar a coluna correspondente ao departamento
  d1 <- data[, dep]
  
  # Inicializar os dados de treino com os dados originais
  TS_dep_train <- d1[1:(length(d1) - num_semanas)]
  
  for (j in 1:num_iteracoes) {
    # Definir lags com base no conhecimento
    timelags <- c(1:4)
    D <- CasesSeries(TS_dep_train, timelags)
    
    # Tamanho da janela de treino
    W2 <- length(TS_dep_train) - max(timelags)
    
    # Divisão em conjunto de treino e teste
    H2 <- holdout(D$y, ratio = num_semanas, mode = "incremental", iter = 1, window = W2)
    
    # Treinamento do modelo de previsão (utilizando randomForest)
    M2 <- fit(y ~ ., D[H2$tr, ], model = "randomForest")
    
    # Realizar a previsão
    Pred <- lforecast(M2, D, start = (length(H2$tr) + 1), horizon = num_semanas)
    
    # Criar um data frame temporário para armazenar os resultados da iteração atual
    iteracao_df <- data.frame(
      Iteracao = j,
      Departamento = dep,
      Semana1 = Pred[1],
      Semana2 = Pred[2],
      Semana3 = Pred[3],
      Semana4 = Pred[4]
    )
    
    # Adicionar os resultados da iteração atual ao data frame principal
    previsoes_df <- rbind(previsoes_df, iteracao_df)
    
    # Adicionar as previsões aos dados de treino para a próxima iteração
    TS_dep_train <- c(TS_dep_train, as.numeric(Pred))
  }
}

# Definir o caminho e o nome do arquivo de saída
output_file <- "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/RandomForestprevisoes_walmart.csv"

# Escrever o data frame em um arquivo CSV
write.csv(previsoes_df, file = output_file, row.names = FALSE)

# Mensagem de confirmação
cat("Dados previstos foram exportados para:", output_file, "\n")

