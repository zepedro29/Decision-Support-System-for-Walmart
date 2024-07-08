library(rminer)

# Load and Preprocess Data
cat("read Walmart Sales\n")
TS <- read.csv("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/projeto/walmart.csv")

# Obter os nomes das colunas dos departamentos
departamentos <- colnames(TS)[grep("^WSdep", colnames(TS))]

# Inicializar uma lista para armazenar as previsões por departamento
num_departamentos <- length(departamentos)
num_semanas <- 4  # Número de semanas para prever
num_iteracoes <- 10

# Data frame para armazenar todas as previsões com ID de iteração
previsoes_df <- data.frame()

# Fase 1: Implementação inicial com 10 iterações
for (i in 1:num_departamentos) {
  dep <- departamentos[i]
  cat("Fase 1: Previsão para o departamento:", dep, "\n")
  
  dep_data <- TS[, dep]
  
  K <- 4  # Frequency (e.g., weeks)
  L <- length(dep_data)
  NTS <- K # number of predictions
  H <- NTS # from 1 to H ahead predictions
  
  # Inicializar os dados de treino com os dados originais
  TS_dep_train <- dep_data[1:(length(dep_data) - num_semanas)]
  
  for (j in 1:num_iteracoes) {
    # Define lags based on your knowledge (e.g., 1, 12, 13)
    d <- CasesSeries(TS_dep_train, c(1, 12, 13))  # Create CasesSeries object with lags
    
    # Train-Test Split
    LTR <- length(TS_dep_train)
    Y <- dep_data[(LTR + 1):(LTR + num_semanas)]
    
    # Train MLP Model
    hd <- holdout(d$y, ratio = num_semanas, mode = "order")
    NN2 <- fit(y ~ ., d[hd$tr,], model = "mlpe")
    
    # Multi-step Forecasts
    init <- hd$ts[1]
    F5 <- lforecast(NN2, d, start = init, horizon = H)
    Pred <- F5
    
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
output_file <- "C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/Previsoes/MLPEprevisoes_walmart.csv"

# Escrever o data frame em um arquivo CSV
write.csv(previsoes_df, file = output_file, row.names = FALSE)

# Mensagem de confirmação
cat("Dados previstos foram exportados para:", output_file, "\n")
